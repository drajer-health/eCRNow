
-- ========== CREATE FUNCTION TO ADD COLUMN IF NOT EXISTS ==========
CREATE OR REPLACE FUNCTION check_and_add_column(
    t_table_name TEXT, t_column_name TEXT, t_column_type TEXT
) RETURNS VOID AS $$
DECLARE
    col_exists BOOLEAN;
BEGIN
    -- Check if the column exists
    SELECT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = t_table_name
        AND column_name = t_column_name
    ) INTO col_exists;

    -- Add column if it does not exist
    IF NOT col_exists THEN
        EXECUTE format(
            'ALTER TABLE %I ADD COLUMN %I %s',
            t_table_name, t_column_name, t_column_type
        );
    END IF;
END;
$$ LANGUAGE plpgsql;

-- ========== CREATE FUNCTION TO ADD INDEX IF NOT EXISTS ==========
CREATE OR REPLACE FUNCTION check_and_create_index(
    table_name TEXT, index_name TEXT, column_name TEXT
) RETURNS VOID AS $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_indexes
                   WHERE tablename = table_name
                   AND indexname = index_name) THEN
        EXECUTE format('CREATE INDEX %I ON %I (%I)', index_name, table_name, column_name);
    END IF;
END;
$$ LANGUAGE plpgsql;


- ========== CREATE STORE PROCEDURE ==========


CREATE OR REPLACE PROCEDURE update_database_schema()
LANGUAGE plpgsql
AS $$
DECLARE
    col_exists BOOLEAN;
    table_exists BOOLEAN;
BEGIN
    -- ========== CHECK & UPDATE healthcare_setting TABLE ==========
    SELECT EXISTS (SELECT 1 FROM information_schema.tables
                   WHERE table_name = 'healthcare_setting') INTO table_exists;
    IF table_exists THEN
        -- Check and add 'debug_enabled' column if it does not exist
        SELECT EXISTS (SELECT 1 FROM information_schema.columns
                       WHERE table_name = 'healthcare_setting'
                       AND column_name = 'debug_enabled') INTO col_exists;
        IF NOT col_exists THEN
            EXECUTE 'ALTER TABLE healthcare_setting ADD COLUMN debug_enabled INTEGER DEFAULT 1';
        END IF;

        -- Modify 'is_xdr' column default value if it exists
        SELECT EXISTS (SELECT 1 FROM information_schema.columns
                       WHERE table_name = 'healthcare_setting'
                       AND column_name = 'is_xdr') INTO col_exists;
        IF col_exists THEN
            EXECUTE 'ALTER TABLE healthcare_setting ALTER COLUMN is_xdr SET DEFAULT 0';
        END IF;

        -- Add other missing columns
        PERFORM check_and_add_column('healthcare_setting', 'smtp_tls_version', 'VARCHAR(256) DEFAULT ''TLSv1.2''');
        PERFORM check_and_add_column('healthcare_setting', 'backend_auth_alg', 'TEXT NULL');
        PERFORM check_and_add_column('healthcare_setting', 'backend_auth_kid', 'TEXT NULL');
        PERFORM check_and_add_column('healthcare_setting', 'direct_endpoint_cert_alias', 'TEXT');
        PERFORM check_and_add_column('healthcare_setting', 'smtp_auth_enabled', 'INTEGER DEFAULT 0');
        PERFORM check_and_add_column('healthcare_setting', 'smtp_ssl_enabled', 'INTEGER DEFAULT 0');
    END IF;

    -- ========== CHECK & UPDATE kar_info TABLE ==========
    SELECT EXISTS (SELECT 1 FROM information_schema.tables
                   WHERE table_name = 'kar_info') INTO table_exists;
    IF table_exists THEN
        PERFORM check_and_add_column('kar_info', 'kar_available', 'INTEGER DEFAULT 0');
    END IF;

    -- ========== CHECK & UPDATE notification_context TABLE ==========
    SELECT EXISTS (SELECT 1 FROM information_schema.tables
                   WHERE table_name = 'notification_context') INTO table_exists;
    IF table_exists THEN
        PERFORM check_and_add_column('notification_context', 'encounter_class', 'VARCHAR(255)');
        PERFORM check_and_add_column('notification_context', 'relaunch_notification_data', 'VARCHAR(255)');

        -- Add unique constraint if not exists
        IF NOT EXISTS (SELECT 1 FROM information_schema.table_constraints
                       WHERE table_name = 'notification_context'
                       AND constraint_name = 'unique_notification_context') THEN
            EXECUTE 'ALTER TABLE notification_context ADD CONSTRAINT unique_notification_context
            UNIQUE (fhir_server_base_url, patient_id, notification_resource_id, notification_resource_type, trigger_event)';
        END IF;
    END IF;

    -- ========== CHECK & UPDATE public_health_authority TABLE ==========
    SELECT EXISTS (SELECT 1 FROM information_schema.tables
                   WHERE table_name = 'public_health_authority') INTO table_exists;
    IF table_exists THEN
        PERFORM check_and_add_column('public_health_authority', 'backend_auth_key_alias', 'TEXT');
        PERFORM check_and_add_column('public_health_authority', 'backend_auth_alg', 'TEXT NULL');
        PERFORM check_and_add_column('public_health_authority', 'backend_auth_kid', 'TEXT NULL');
    END IF;

    -- ========== CREATE INDEXES IF NOT EXISTS ==========
    PERFORM check_and_create_index('ph_messages', 'idx_not_res_id', 'notified_resource_id');
    PERFORM check_and_create_index('ph_messages', 'idx_kar_id', 'kar_unique_id');
    PERFORM check_and_create_index('scheduled_tasks', 'idx_exec_time', 'execution_time');

END $$;

 -- ========== CALL  PROCEDURE TO UPDATE  DB SCHEMA  ==========

    CALL update_database_schema();