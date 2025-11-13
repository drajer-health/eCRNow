-- ==================================================================
-- client_details.sql
-- Purpose: Create client_details_v2 and migrate data from client_details
-- Note: Constraints handled separately
-- ==================================================================


DO
$$
DECLARE
    old_table TEXT := 'client_details';
    new_table TEXT := 'client_details_v2';
    row_count_old BIGINT;
    row_count_new BIGINT;
    missing_cols INT;
    col_list TEXT := '';
    select_list TEXT := '';
    col RECORD;
BEGIN
    ------------------------------------------------------------------
    -- 1. Check if old table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_name = old_table AND table_schema = 'public'
    ) THEN
        RAISE NOTICE 'Old table "%" does not exist. Exiting.', old_table;
        RETURN;
    END IF;

    ------------------------------------------------------------------
    -- 2. Create new table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_name = new_table AND table_schema = 'public'
    ) THEN
        EXECUTE format('
            CREATE TABLE %I (
                id SERIAL PRIMARY KEY,
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
                is_covid INT DEFAULT 0,
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
        ', new_table);
        RAISE NOTICE 'New table "%" created successfully.', new_table;
    ELSE
        RAISE NOTICE 'New table "%" already exists.', new_table;
    END IF;

    ------------------------------------------------------------------
    -- 3. Determine matching columns
    ------------------------------------------------------------------
    SELECT COUNT(*) INTO missing_cols
    FROM information_schema.columns c1
    WHERE c1.table_name = old_table
    AND NOT EXISTS (
        SELECT 1
        FROM information_schema.columns c2
        WHERE c2.table_name = new_table
        AND c2.column_name = c1.column_name
    );

    IF missing_cols > 0 THEN
        RAISE NOTICE 'Some columns are missing between old and new tables.';
    END IF;

    ------------------------------------------------------------------
    -- 4. Build matching column list for migration
    ------------------------------------------------------------------
    FOR col IN
        SELECT c1.column_name
        FROM information_schema.columns c1
        JOIN information_schema.columns c2
          ON c1.column_name = c2.column_name
        WHERE c1.table_name = old_table
          AND c2.table_name = new_table
          AND c1.table_schema = 'public'
          AND c2.table_schema = 'public'
        ORDER BY c1.ordinal_position
    LOOP
        col_list := col_list || format('%I, ', col.column_name);
        select_list := select_list || format('%I, ', col.column_name);
    END LOOP;

    IF col_list = '' THEN
        RAISE NOTICE 'No matching columns found between "%" and "%".', old_table, new_table;
        RETURN;
    END IF;

    col_list := left(col_list, length(col_list) - 2);
    select_list := left(select_list, length(select_list) - 2);

    ------------------------------------------------------------------
    -- 5. Perform data migration
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', old_table) INTO row_count_old;

    EXECUTE format('
        INSERT INTO %I (%s)
        SELECT %s FROM %I;
    ', new_table, col_list, select_list, old_table);

    EXECUTE format('SELECT COUNT(*) FROM %I', new_table) INTO row_count_new;

    RAISE NOTICE 'Data migrated from "%" to "%" successfully.', old_table, new_table;
    RAISE NOTICE 'Old table rows: %, New table rows: %', row_count_old, row_count_new;

END
$$;
