-- ==================================================================
-- ph_messages.sql
-- Purpose: Create ph_messages_v2 and migrate data from ph_messages
-- Note: Constraints and FKs handled separately
-- ==================================================================

DO
$$
DECLARE
    old_table TEXT := 'ph_messages';
    new_table TEXT := 'ph_messages_v2';
    row_count_old BIGINT;
    row_count_new BIGINT;
    missing_cols INT;
    col_list TEXT := '';
    select_list TEXT := '';
    col RECORD;
BEGIN
    ------------------------------------------------------------------
    -- 1. Ensure old table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = old_table
    ) THEN
        RAISE EXCEPTION 'Old table "%" does not exist. Migration aborted.', old_table;
    END IF;

    ------------------------------------------------------------------
    -- 2. Create new table if not exists (no constraints)
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = new_table
    ) THEN
        RAISE NOTICE 'Creating new table "%" ...', new_table;

        EXECUTE format($fmt$
            CREATE TABLE %I (
                id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
                fhir_server_base_url VARCHAR(8000) NOT NULL,
                patient_id VARCHAR(8000) NOT NULL,
                encounter_id VARCHAR(8000) NOT NULL,
                notified_resource_id VARCHAR(8000) NOT NULL,
                notified_resource_type VARCHAR(8000) NOT NULL,
                kar_unique_id VARCHAR(8000),
                notification_id VARCHAR(8000) NOT NULL,
                correlation_id VARCHAR(8000),
                x_request_id VARCHAR(8000),
                submitted_fhir_data TEXT,
                submitted_cda_data TEXT,
                submitted_message_type VARCHAR(8000),
                submitted_data_id VARCHAR(8000),
                submitted_version_number INTEGER,
                submitted_message_id VARCHAR(8000),
                submission_message_status VARCHAR(8000),
                submission_time TIMESTAMP,
                fhir_response_data TEXT,
                cda_response_data TEXT,
                failure_response_data TEXT,
                response_message_type VARCHAR(8000),
                response_data_id VARCHAR(8000),
                response_message_id VARCHAR(8000),
                response_processing_instruction TEXT,
                response_processing_status TEXT,
                response_received_time TIMESTAMP,
                ehr_doc_ref_id VARCHAR(8000),
                initiating_action TEXT,
                trigger_match_status TEXT,
                patient_linker_id VARCHAR(8000),
                last_updated_ts TIMESTAMP NOT NULL
            );
        $fmt$, new_table);

        -- Indexes
        EXECUTE format('CREATE INDEX IF NOT EXISTS idx_not_res_id_v2 ON %I (notified_resource_id);', new_table);
        EXECUTE format('CREATE INDEX IF NOT EXISTS idx_kar_id_v2 ON %I (kar_unique_id);', new_table);

        RAISE NOTICE 'Table "%" created successfully.', new_table;
    ELSE
        RAISE NOTICE 'Table "%" already exists. Proceeding with validation...', new_table;
    END IF;

    ------------------------------------------------------------------
    -- 3. Verify column compatibility
    ------------------------------------------------------------------
    SELECT COUNT(*) INTO missing_cols
    FROM (
        SELECT column_name FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table
        EXCEPT
        SELECT column_name FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = new_table
    ) diff;

    IF missing_cols > 0 THEN
        RAISE EXCEPTION 'Column mismatch: % missing column(s).', missing_cols;
    ELSE
        RAISE NOTICE 'All columns from "%" exist in "%".', old_table, new_table;
    END IF;

    ------------------------------------------------------------------
    -- 4. Data migration
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I;', old_table) INTO row_count_old;
    IF row_count_old = 0 THEN
        RAISE NOTICE 'No rows to migrate from "%".', old_table;
        RETURN;
    END IF;

    -- Build column list dynamically
    FOR col IN
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table
        ORDER BY ordinal_position
    LOOP
        IF EXISTS (
            SELECT 1 FROM information_schema.columns
            WHERE table_schema = 'public' AND table_name = new_table AND column_name = col.column_name
        ) THEN
            col_list := col_list || format('%I, ', col.column_name);
            select_list := select_list || format('%I, ', col.column_name);
        END IF;
    END LOOP;

    col_list := regexp_replace(col_list, ',\s*$', '');
    select_list := regexp_replace(select_list, ',\s*$', '');

    RAISE NOTICE 'Migrating % rows from "%" to "%"...', row_count_old, old_table, new_table;

    EXECUTE format('INSERT INTO %I (%s) SELECT %s FROM %I;', new_table, col_list, select_list, old_table);

    EXECUTE format('SELECT COUNT(*) FROM %I;', new_table) INTO row_count_new;

    IF row_count_new != row_count_old THEN
        RAISE EXCEPTION 'Row count mismatch (old=% new=%).', row_count_old, row_count_new;
    ELSE
        RAISE NOTICE 'Data migration complete: % rows copied.', row_count_new;
    END IF;

EXCEPTION
    WHEN OTHERS THEN
        RAISE NOTICE 'Error: %', SQLERRM;
        RAISE NOTICE 'Rolling back.';
        RAISE;
END;
$$;
