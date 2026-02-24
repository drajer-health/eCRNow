-- ==================================================================
-- launch_details.sql
-- Purpose: Create launch_details_v2 and migrate data from launch_details
-- Note: Constraints handled separately
-- ==================================================================
DO
$$
DECLARE
    old_table TEXT := 'launch_details';
    new_table TEXT := 'launch_details_v2';
    row_count_old BIGINT;
    row_count_new BIGINT;
    missing_cols INT;
    col_list TEXT := '';
    select_list TEXT := '';
    col RECORD;
    missing_in_new TEXT;
    old_col_type TEXT;
    new_col_type TEXT;
BEGIN
    ------------------------------------------------------------------
    -- 1 Check if old table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = old_table
    ) THEN
        RAISE EXCEPTION ' Source table "%" does not exist. Migration aborted.', old_table;
    END IF;

    ------------------------------------------------------------------
    -- 2️ Create new table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = new_table
    ) THEN
        RAISE NOTICE 'Creating table "%"...', new_table;

        EXECUTE format('
            CREATE TABLE %I (
                id SERIAL PRIMARY KEY,
                client_id VARCHAR(255),
                client_secret VARCHAR(8000),
                ehr_server_url VARCHAR(255),
                auth_url VARCHAR(8000),
                token_url VARCHAR(8000),
                access_token TEXT,
                user_id VARCHAR(255),
                expiry INT DEFAULT 0,
                scope TEXT,
                last_updated_ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
                start_date TIMESTAMP,
                end_date TIMESTAMP,
                token_expiry_date TIMESTAMP,
                refresh_token TEXT,
                launch_patient_id VARCHAR(255),
                fhir_version VARCHAR(255),
                encounter_id VARCHAR(255),
                provider_uuid VARCHAR(255),
                status VARCHAR(8000),
                aa_id VARCHAR(255),
                set_id VARCHAR(255),
                ver_number VARCHAR(255),
                direct_host VARCHAR(255),
                direct_user VARCHAR(255),
                direct_pwd VARCHAR(255),
                smtp_url VARCHAR(255),
                smtp_port VARCHAR(255),
                imap_url VARCHAR(255),
                imap_port VARCHAR(255),
                direct_recipient VARCHAR(255),
                rest_api_url VARCHAR(255),
                is_covid19 INT DEFAULT 0,
                is_emergent_reporting_enabled INT DEFAULT 0,
                is_full_ecr INT DEFAULT 1,
                rrprocessing_createdocRef INT DEFAULT 0,
                rrprocessing_invokerestapi INT DEFAULT 0,
                rrprocessing_both INT DEFAULT 0,
                rr_rest_api_url VARCHAR(255),
                rr_doc_ref_mime_type VARCHAR(255),
                launch_id VARCHAR(255),
                launch_state VARCHAR(255),
                redirect_uri VARCHAR(255),
                auth_code VARCHAR(255),
                is_system_launch INT DEFAULT 1,
                is_multi_tenant_system_launch INT DEFAULT 0,
                is_user_account_launch INT DEFAULT 0,
                debug_fhir_query_and_eicr INT DEFAULT 0,
                require_aud INT DEFAULT 0,
                x_request_id VARCHAR(255),
                validation_mode INT DEFAULT 0,
                launch_type VARCHAR(255),
                processing_status VARCHAR(255)
            );
        ', new_table);

        RAISE NOTICE 'Table "%" created successfully.', new_table;
    ELSE
        RAISE NOTICE ' Table "%" already exists. Proceeding with validation...', new_table;
    END IF;

    ------------------------------------------------------------------
    -- 3 Validate column consistency
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

    -- Collect missing column names into a comma-separated list
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
          RAISE EXCEPTION 'Column mismatch detected in : % column(s) missing in "%": % please add misssing in order to migrate ',
               missing_cols, old_table, missing_in_new;
       ELSE
           RAISE NOTICE 'All columns from "%" exist in "%".', old_table, new_table;
       END IF;

    ------------------------------------------------------------------
    -- 4️ Check for data in old table
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', old_table) INTO row_count_old;
    IF row_count_old = 0 THEN
        RAISE NOTICE 'ℹ No data found in "%". Migration skipped.', old_table;
        RETURN;
    END IF;

    ------------------------------------------------------------------
    -- 5️ Build column lists dynamically
    ------------------------------------------------------------------
    FOR col IN
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table
        ORDER BY ordinal_position
    LOOP
        SELECT data_type INTO old_col_type
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table AND column_name = col.column_name;

        SELECT data_type INTO new_col_type
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = new_table AND column_name = col.column_name;

        IF new_col_type = 'boolean' AND old_col_type IN ('integer', 'smallint', 'character varying', 'text') THEN
            select_list := select_list || format('(%I = 1), ', col.column_name);
        ELSE
            select_list := select_list || format('%I, ', col.column_name);
        END IF;

        col_list := col_list || format('%I, ', col.column_name);
    END LOOP;

    col_list := regexp_replace(col_list, ',\s*$', '');
    select_list := regexp_replace(select_list, ',\s*$', '');

    ------------------------------------------------------------------
    -- 6️ Migrate data old → new
    ------------------------------------------------------------------
    RAISE NOTICE '⬆ Inserting data into "%"...', new_table;
    EXECUTE format('INSERT INTO %I (%s) SELECT %s FROM %I;', new_table, col_list, select_list, old_table);

    ------------------------------------------------------------------
    -- 7️ Validate row counts
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', new_table) INTO row_count_new;

    IF row_count_new != row_count_old THEN
        RAISE EXCEPTION 'Row count mismatch! old=% new=% — rolling back.', row_count_old, row_count_new;
    ELSE
        RAISE NOTICE 'Migration successful: % rows copied from "%" → "%".', row_count_new, old_table, new_table;
    END IF;

EXCEPTION
    WHEN OTHERS THEN
        RAISE NOTICE 'Error during migration: %', SQLERRM;
        RAISE;
END;
$$;
