-- ==================================================================
-- launch_details.sql (UPDATED VERSION)
-- Purpose: Create launch_details_v2 and migrate data from launch_details
-- ==================================================================

DO
$$
DECLARE
    old_table TEXT := 'launch_details';
    new_table TEXT := 'launch_details_v2';
    row_count_old BIGINT := 0;
    row_count_new_before BIGINT := 0;
    row_count_new_after BIGINT := 0;
    missing_cols INT := 0;
    missing_in_new TEXT := '';
    col_list TEXT := '';
    select_list TEXT := '';
    col RECORD;
    old_col_type TEXT;
    new_col_type TEXT;
BEGIN
    ------------------------------------------------------------------
    -- 0. If old table does NOT exist → create new table (if needed) then EXIT
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = old_table
    ) THEN

        RAISE NOTICE 'Source table "%" does not exist. Creating "%" (if needed) and skipping migration.',
            old_table, new_table;

        -- Create v2 table if not exists
        IF NOT EXISTS (
            SELECT 1 FROM information_schema.tables
            WHERE table_schema = 'public' AND table_name = new_table
        ) THEN

            RAISE NOTICE 'Creating table "%"...', new_table;

            EXECUTE format($fmt$
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
            $fmt$, new_table);

            RAISE NOTICE 'Table "%" created successfully.', new_table;

        ELSE
            RAISE NOTICE 'Table "%" already exists. Nothing to create.', new_table;
        END IF;

        RETURN;
    END IF;


    ------------------------------------------------------------------
    -- 1. Old table exists → ensure new table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = new_table
    ) THEN

        RAISE NOTICE 'Creating table "%"...', new_table;

        EXECUTE format($fmt$
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
        $fmt$, new_table);

        RAISE NOTICE 'Table "%" created.', new_table;

    ELSE
        RAISE NOTICE 'Table "%" already exists. Proceeding...', new_table;
    END IF;


    ------------------------------------------------------------------
    -- 2. Validate column compatibility
    ------------------------------------------------------------------
    SELECT COUNT(*) INTO missing_cols
    FROM (
        SELECT column_name FROM information_schema.columns WHERE table_name = new_table
        EXCEPT
        SELECT column_name FROM information_schema.columns WHERE table_name = old_table
    ) diff;

    SELECT COALESCE(string_agg(column_name, ', '), 'None')
    INTO missing_in_new
    FROM (
        SELECT column_name FROM information_schema.columns WHERE table_name = new_table
        EXCEPT
        SELECT column_name FROM information_schema.columns WHERE table_name = old_table
    ) diff;

    IF missing_cols > 0 THEN
        RAISE EXCEPTION 'Column mismatch: % column(s) missing in "%": %',
            missing_cols, old_table, missing_in_new;
    ELSE
        RAISE NOTICE 'Column compatibility OK.';
    END IF;


    ------------------------------------------------------------------
    -- 3. Count rows in old table
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', old_table)
    INTO row_count_old;

    IF row_count_old = 0 THEN
        RAISE NOTICE 'No data to migrate. Skipping.';
        RETURN;
    END IF;


    ------------------------------------------------------------------
    -- 4. Build dynamic column lists
    ------------------------------------------------------------------
    col_list := '';
    select_list := '';

    FOR col IN
        SELECT column_name
        FROM information_schema.columns
        WHERE table_name = old_table
        ORDER BY ordinal_position
    LOOP
        SELECT data_type INTO old_col_type
        FROM information_schema.columns
        WHERE table_name = old_table AND column_name = col.column_name;

        SELECT data_type INTO new_col_type
        FROM information_schema.columns
        WHERE table_name = new_table AND column_name = col.column_name;

        IF new_col_type = 'boolean'
            AND old_col_type IN ('integer', 'smallint', 'character varying', 'text')
        THEN
            select_list := select_list || format('(%I = 1), ', col.column_name);
        ELSE
            select_list := select_list || format('%I, ', col.column_name);
        END IF;

        col_list := col_list || format('%I, ', col.column_name);
    END LOOP;

    col_list := regexp_replace(col_list, ',\s*$', '');
    select_list := regexp_replace(select_list, ',\s*$', '');


    ------------------------------------------------------------------
    -- 5. Insert data
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', new_table)
    INTO row_count_new_before;

    EXECUTE format(
        'INSERT INTO %I (%s) SELECT %s FROM %I',
        new_table, col_list, select_list, old_table
    );

    EXECUTE format('SELECT COUNT(*) FROM %I', new_table)
    INTO row_count_new_after;


    ------------------------------------------------------------------
    -- 6. Verify migrated rows
    ------------------------------------------------------------------
    RAISE NOTICE 'Inserted % rows into "%".',
        (row_count_new_after - row_count_new_before), new_table;

END;
$$;
