-- ==================================================================
-- eicr.sql
-- Purpose: Create eicr_v2 and migrate data from eicr
-- Note: Constraints handled separately
-- ==================================================================

DO
$$
DECLARE
    old_table TEXT := 'eicr';
    new_table TEXT := 'eicr_v2';
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
    -- 0. If old table DOES NOT exist -> create new table (if needed) and exit
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
                    id SERIAL PRIMARY KEY,
                    x_req_id VARCHAR(8000),
                    x_correlation_id VARCHAR(8000),
                    eicr_doc_id VARCHAR(8000),
                    set_id VARCHAR(8000),
                    doc_version INTEGER,
                    eicr_data TEXT,
                    initiating_action TEXT,
                    response_type VARCHAR(8000),
                    response_type_display VARCHAR(8000),
                    response_x_request_id VARCHAR(8000),
                    response_doc_id VARCHAR(8000),
                    rr_data TEXT,
                    fhir_server_url VARCHAR(8000),
                    launch_patient_id VARCHAR(8000),
                    launch_details_id INTEGER,
                    encounter_id VARCHAR(8000),
                    provider_uuid VARCHAR(8000),
                    ehr_doc_ref_id VARCHAR(8000),
                    eicr_proc_status VARCHAR(8000),
                    rr_proc_status VARCHAR(8000),
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
                id SERIAL PRIMARY KEY,
                x_req_id VARCHAR(8000),
                x_correlation_id VARCHAR(8000),
                eicr_doc_id VARCHAR(8000),
                set_id VARCHAR(8000),
                doc_version INTEGER,
                eicr_data TEXT,
                initiating_action TEXT,
                response_type VARCHAR(8000),
                response_type_display VARCHAR(8000),
                response_x_request_id VARCHAR(8000),
                response_doc_id VARCHAR(8000),
                rr_data TEXT,
                fhir_server_url VARCHAR(8000),
                launch_patient_id VARCHAR(8000),
                launch_details_id INTEGER,
                encounter_id VARCHAR(8000),
                provider_uuid VARCHAR(8000),
                ehr_doc_ref_id VARCHAR(8000),
                eicr_proc_status VARCHAR(8000),
                rr_proc_status VARCHAR(8000),
                last_updated_ts TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
            );
        $fmt$, new_table);

        RAISE NOTICE 'New table "%" created.', new_table;
    ELSE
        RAISE NOTICE 'New table "%" already exists. Proceeding with validation...', new_table;
    END IF;

    ------------------------------------------------------------------
    -- 2. Verify column compatibility: columns present in new_table but missing in old_table
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
        RAISE NOTICE 'Column compatibility check passed: no extra columns in "%" that are missing from "%".', new_table, old_table;
    END IF;

    ------------------------------------------------------------------
    -- 3. Build matching column list (intersection) for migration
    --    Exclude 'id' column from copy
    ------------------------------------------------------------------
    FOR col IN
        SELECT c1.column_name
        FROM information_schema.columns c1
        JOIN information_schema.columns c2
          ON c1.column_name = c2.column_name
        WHERE c1.table_schema = 'public' AND c2.table_schema = 'public'
          AND c1.table_name = old_table
          AND c2.table_name = new_table
          AND c1.column_name <> 'id'
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
    -- 4. Perform data migration (skip duplicates based on eicr_doc_id)
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I WHERE eicr_doc_id IS NOT NULL;', old_table)
    INTO row_count_old;

    EXECUTE format('SELECT COUNT(*) FROM %I;', new_table)
    INTO row_count_new_before;

    IF row_count_old = 0 THEN
        RAISE NOTICE 'No rows with non-null eicr_doc_id to migrate from "%".', old_table;
        RETURN;
    END IF;

    RAISE NOTICE 'Attempting to migrate % qualifying rows from "%" to "%"...', row_count_old, old_table, new_table;

    EXECUTE format(
        'INSERT INTO %I (%s)
         SELECT %s FROM %I e
         WHERE e.eicr_doc_id IS NOT NULL
           AND NOT EXISTS (
               SELECT 1 FROM %I n WHERE n.eicr_doc_id = e.eicr_doc_id
           );',
        new_table, col_list, select_list, old_table, new_table
    );

    EXECUTE format('SELECT COUNT(*) FROM %I;', new_table)
    INTO row_count_new_after;

    IF (row_count_new_after - row_count_new_before) = 0 THEN
        RAISE NOTICE 'No new rows were inserted (all eicr_doc_id values already present in "%").', new_table;
    ELSE
        RAISE NOTICE 'Inserted % new rows into "%".', (row_count_new_after - row_count_new_before), new_table;
    END IF;

    RAISE NOTICE 'Migration summary: old_rows_with_eicr_doc_id=%, new_before=%, new_after=%',
        row_count_old, row_count_new_before, row_count_new_after;

EXCEPTION
    WHEN OTHERS THEN
        RAISE NOTICE 'Error during migration: %', SQLERRM;
        RAISE NOTICE 'Rolling back migration for "%".', old_table;
        RAISE;
END;
$$;
