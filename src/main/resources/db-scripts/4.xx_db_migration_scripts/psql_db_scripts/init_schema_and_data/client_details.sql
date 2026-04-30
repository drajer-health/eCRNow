-- ==================================================================
-- client_details.sql
-- Purpose: Create client_details_v2 and migrate data from client_details
-- ==================================================================

-- Create sequence for Hibernate ID generation
CREATE SEQUENCE IF NOT EXISTS public.client_details_v2_seq
    START WITH 1
    INCREMENT BY 50
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

DO
$$
DECLARE
    old_table TEXT := 'client_details';
    new_table TEXT := 'client_details_v2';
    row_count_old BIGINT := 0;
    row_count_new_before BIGINT := 0;
    row_count_new_after BIGINT := 0;
    missing_cols INT := 0;
    missing_in_new TEXT := '';
    col_list TEXT := '';
    select_list TEXT := '';
    col RECORD;
BEGIN
    ------------------------------------------------------------------
    -- 0. If old table does NOT exist -> create new table (if needed) and exit
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = old_table
    ) THEN

        RAISE NOTICE 'Old table "%" does not exist. Creating new table "%" (if not present) and skipping migration.',
            old_table, new_table;

        IF NOT EXISTS (
            SELECT 1 FROM information_schema.tables
            WHERE table_schema = 'public' AND table_name = new_table
        ) THEN

            EXECUTE format($fmt$
                CREATE TABLE %I (
                    id INTEGER DEFAULT nextval('client_details_v2_seq') PRIMARY KEY,
                    is_provider_launch INT DEFAULT 0,
                    is_system_launch INT DEFAULT 1,
                    is_multi_tenant_system_launch INT DEFAULT 0,
                    is_user_account_launch INT DEFAULT 0,
                    clientId VARCHAR(8000) NOT NULL,
                    clientSecret TEXT NULL,
                    fhir_server_base_url VARCHAR(255) NOT NULL,
                    token_url VARCHAR(8000) NULL,
                    scopes TEXT NOT NULL,
                    access_token TEXT NULL,
                    token_expiry INT DEFAULT 0,
                    token_expiry_date TIMESTAMP NULL,
                    is_direct INT DEFAULT 0,
                    is_xdr INT DEFAULT 0,
                    is_restapi INT DEFAULT 0,
                    direct_host VARCHAR(8000) NULL,
                    direct_user VARCHAR(255) NULL,
                    direct_pwd VARCHAR(255) NULL,
                    smtp_url VARCHAR(255) NULL,
                    smtp_port VARCHAR(255) NULL,
                    imap_url VARCHAR(255) NULL,
                    imap_port VARCHAR(255) NULL,
                    direct_recipient_address VARCHAR(255) NULL,
                    xdr_recipient_address VARCHAR(255) NULL,
                    rest_api_url VARCHAR(255) NULL,
                    aa_id VARCHAR(255) NULL,
                    encounter_start_time VARCHAR(255) NULL,
                    encounter_end_time VARCHAR(255) NULL,
                    is_covid19 INT DEFAULT 0,
                    is_full_ecr INT DEFAULT 1,
                    is_emergent_reporting_enabled INT DEFAULT 0,
                    rrprocessing_createdocRef INT DEFAULT 0,
                    rrprocessing_invokerestapi INT DEFAULT 0,
                    rrprocessing_both INT DEFAULT 0,
                    rr_rest_api_url VARCHAR(255) NULL,
                    rr_doc_ref_mime_type VARCHAR(255) NULL,
                    debug_fhir_query_and_eicr INT DEFAULT 0,
                    require_aud INT DEFAULT 0,
                    last_updated_ts TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
                );
            $fmt$, new_table);

            RAISE NOTICE 'New table "%" created successfully.', new_table;
        ELSE
            RAISE NOTICE 'New table "%" already exists.', new_table;
        END IF;

        RETURN;
    END IF;

    ------------------------------------------------------------------
    -- 1. Old table exists -> ensure new table exists (create if needed)
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = new_table
    ) THEN
        RAISE NOTICE 'Creating new table "%" for migration...', new_table;

        EXECUTE format($fmt$
            CREATE TABLE %I (
                id INTEGER DEFAULT nextval('client_details_v2_seq') PRIMARY KEY,
                is_provider_launch INT DEFAULT 0,
                is_system_launch INT DEFAULT 1,
                is_multi_tenant_system_launch INT DEFAULT 0,
                is_user_account_launch INT DEFAULT 0,
                clientId VARCHAR(8000) NOT NULL,
                clientSecret TEXT NULL,
                fhir_server_base_url VARCHAR(255) NOT NULL,
                token_url VARCHAR(8000) NULL,
                scopes TEXT NOT NULL,
                access_token TEXT NULL,
                token_expiry INT DEFAULT 0,
                token_expiry_date TIMESTAMP NULL,
                is_direct INT DEFAULT 0,
                is_xdr INT DEFAULT 0,
                is_restapi INT DEFAULT 0,
                direct_host VARCHAR(8000) NULL,
                direct_user VARCHAR(255) NULL,
                direct_pwd VARCHAR(255) NULL,
                smtp_url VARCHAR(255) NULL,
                smtp_port VARCHAR(255) NULL,
                imap_url VARCHAR(255) NULL,
                imap_port VARCHAR(255) NULL,
                direct_recipient_address VARCHAR(255) NULL,
                xdr_recipient_address VARCHAR(255) NULL,
                rest_api_url VARCHAR(255) NULL,
                aa_id VARCHAR(255) NULL,
                encounter_start_time VARCHAR(255) NULL,
                encounter_end_time VARCHAR(255) NULL,
                is_covid19 INT DEFAULT 0,
                is_full_ecr INT DEFAULT 1,
                is_emergent_reporting_enabled INT DEFAULT 0,
                rrprocessing_createdocRef INT DEFAULT 0,
                rrprocessing_invokerestapi INT DEFAULT 0,
                rrprocessing_both INT DEFAULT 0,
                rr_rest_api_url VARCHAR(255) NULL,
                rr_doc_ref_mime_type VARCHAR(255) NULL,
                debug_fhir_query_and_eicr INT DEFAULT 0,
                require_aud INT DEFAULT 0,
                last_updated_ts TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
            );
        $fmt$, new_table);

        RAISE NOTICE 'New table "%" created.', new_table;
    ELSE
        RAISE NOTICE 'New table "%" already exists.', new_table;
    END IF;

    ------------------------------------------------------------------
    -- 2. Verify column compatibility: find columns present in new_table but
    --    missing in old_table (these would prevent a straight copy)
    ------------------------------------------------------------------
    SELECT COUNT(*) INTO missing_cols
    FROM (
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = new_table
        EXCEPT
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table
    ) diff;

    SELECT COALESCE(string_agg(column_name, ', '), 'None')
    INTO missing_in_new
    FROM (
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = new_table
        EXCEPT
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table
    ) diff;

    IF missing_cols > 0 THEN
        RAISE EXCEPTION 'Column mismatch: % column(s) exist in "%" but are missing from "%": %. Please add the missing columns or adjust the new table before migrating.',
            missing_cols, new_table, old_table, missing_in_new;
    ELSE
        RAISE NOTICE 'Column compatibility check passed: all columns in "%" exist in "%".', new_table, old_table;
    END IF;

    ------------------------------------------------------------------
    -- 3. Build matching column list (intersection) for migration
    ------------------------------------------------------------------
    FOR col IN
        SELECT c1.column_name
        FROM information_schema.columns c1
        JOIN information_schema.columns c2
          ON c1.column_name = c2.column_name
        WHERE c1.table_schema = 'public' AND c2.table_schema = 'public'
          AND c1.table_name = old_table
          AND c2.table_name = new_table
        ORDER BY c1.ordinal_position
    LOOP
        col_list := col_list || format('%I, ', col.column_name);
        select_list := select_list || format('%I, ', col.column_name);
    END LOOP;

    IF col_list = '' THEN
        RAISE NOTICE 'No matching columns found between "%" and "%". Nothing to migrate.', old_table, new_table;
        RETURN;
    END IF;

    col_list := regexp_replace(col_list, ',\s*$', '');
    select_list := regexp_replace(select_list, ',\s*$', '');

    ------------------------------------------------------------------
    -- 4. Perform data migration and validate row counts
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I;', old_table) INTO row_count_old;
    EXECUTE format('SELECT COUNT(*) FROM %I;', new_table) INTO row_count_new_before;

    IF row_count_old = 0 THEN
        RAISE NOTICE 'No rows to migrate from "%".', old_table;
        RETURN;
    END IF;

    RAISE NOTICE 'Migrating % rows from "%" to "%"...', row_count_old, old_table, new_table;

    EXECUTE format(
        'INSERT INTO %I (%s) SELECT %s FROM %I;',
        new_table, col_list, select_list, old_table
    );

    EXECUTE format('SELECT COUNT(*) FROM %I;', new_table) INTO row_count_new_after;

    IF (row_count_new_after - row_count_new_before) != row_count_old THEN
        RAISE EXCEPTION 'Row count mismatch after migration: expected to insert % rows but actual inserted % rows (new_before=% new_after=%).',
            row_count_old, (row_count_new_after - row_count_new_before), row_count_new_before, row_count_new_after;
    ELSE
        RAISE NOTICE 'Data migration complete: % rows copied (new table total: %).', row_count_old, row_count_new_after;
    END IF;

EXCEPTION
    WHEN OTHERS THEN
        RAISE NOTICE 'Migration error: %', SQLERRM;
        RAISE NOTICE 'Rolling back migration.';
        RAISE;
END;
$$;
