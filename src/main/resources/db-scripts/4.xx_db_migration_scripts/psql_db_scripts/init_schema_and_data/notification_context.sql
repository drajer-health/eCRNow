-- ==================================================================
-- notification_context.sql (UPDATED VERSION)
-- Purpose: Create notification_context_v2 and migrate data
--          from notification_context
-- ==================================================================

DO
$$
DECLARE
    old_table TEXT := 'notification_context';
    new_table TEXT := 'notification_context_v2';
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
                    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
                    trigger_event VARCHAR(8000) NOT NULL,
                    fhir_server_base_url VARCHAR(8000) NOT NULL,
                    patient_id VARCHAR(8000) NOT NULL,
                    notification_resource_id VARCHAR(8000) NOT NULL,
                    notification_resource_type VARCHAR(8000) NOT NULL,
                    relaunch_notification_data TEXT NULL,
                    notification_processing_status VARCHAR(8000) NULL,
                    notification_data TEXT NOT NULL,
                    correlation_id VARCHAR(8000) NULL,
                    x_request_id VARCHAR(8000) NULL,
                    throttle_context VARCHAR(8000) NULL,
                    ehr_access_token TEXT NULL,
                    ehr_access_token_expiry_duration INT NULL,
                    token_expiry_date TIMESTAMP NULL,
                    encounter_start_time TIMESTAMP NULL,
                    encounter_end_time TIMESTAMP NULL,
                    encounter_class VARCHAR(255) NULL,
                    ehr_launch_context VARCHAR(255) NULL,
                    last_updated_ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
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
                id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
                trigger_event VARCHAR(8000) NOT NULL,
                fhir_server_base_url VARCHAR(8000) NOT NULL,
                patient_id VARCHAR(8000) NOT NULL,
                notification_resource_id VARCHAR(8000) NOT NULL,
                notification_resource_type VARCHAR(8000) NOT NULL,
                relaunch_notification_data TEXT NULL,
                notification_processing_status VARCHAR(8000) NULL,
                notification_data TEXT NOT NULL,
                correlation_id VARCHAR(8000) NULL,
                x_request_id VARCHAR(8000) NULL,
                throttle_context VARCHAR(8000) NULL,
                ehr_access_token TEXT NULL,
                ehr_access_token_expiry_duration INT NULL,
                token_expiry_date TIMESTAMP NULL,
                encounter_start_time TIMESTAMP NULL,
                encounter_end_time TIMESTAMP NULL,
                encounter_class VARCHAR(255) NULL,
                ehr_launch_context VARCHAR(255) NULL,
                last_updated_ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
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

        -- No boolean conversions needed for notification_context
        select_list := select_list || format('%I, ', col.column_name);
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
