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
    row_count_old BIGINT;
    row_count_new BIGINT;
    missing_cols INT;
    col_list TEXT := '';
    select_list TEXT := '';
    missing_in_new TEXT;
    col RECORD;
BEGIN
    ------------------------------------------------------------------
    -- 1. Check if old table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_name = old_table AND table_schema = 'public'
    ) THEN
        RAISE EXCEPTION 'Old table "%" does not exist. Migration aborted.', old_table;
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
        ', new_table);
        RAISE NOTICE 'New table "%" created successfully.', new_table;
    ELSE
        RAISE NOTICE 'New table "%" already exists. Proceeding with validation...', new_table;
    END IF;

    ------------------------------------------------------------------
    -- 3. Verify column compatibility between old and new tables
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
        RAISE EXCEPTION 'Column mismatch detected: % column(s) missing in "%": % — please add missing columns before migration.',
            missing_cols, old_table, missing_in_new;
    ELSE
        RAISE NOTICE 'All columns from "%" exist in "%".', old_table, new_table;
    END IF;

    ------------------------------------------------------------------
    -- 4. Build matching column list
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
          AND c1.column_name NOT IN ('id')
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
    -- 5. Perform data migration (skip duplicates based on eicr_doc_id)
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I WHERE eicr_doc_id IS NOT NULL', old_table)
    INTO row_count_old;

    IF row_count_old > 0 THEN
        EXECUTE format('
            INSERT INTO %I (%s)
            SELECT %s FROM %I e
            WHERE e.eicr_doc_id IS NOT NULL
            AND NOT EXISTS (
                SELECT 1 FROM %I n WHERE n.eicr_doc_id = e.eicr_doc_id
            );
        ', new_table, col_list, select_list, old_table, new_table);

        EXECUTE format('SELECT COUNT(*) FROM %I', new_table)
        INTO row_count_new;

        RAISE NOTICE 'Data migrated successfully from "%" to "%".', old_table, new_table;
        RAISE NOTICE 'Old Count: %, New Count: %', row_count_old, row_count_new;
    ELSE
        RAISE NOTICE 'No data to migrate for "%".', old_table;
    END IF;

EXCEPTION
    WHEN OTHERS THEN
        RAISE NOTICE 'Error during migration: %', SQLERRM;
        RAISE;
END
$$;
