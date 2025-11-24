
-- ==================================================================
-- kar_execution_state.sql
-- Purpose: Create kar_execution_state_v2 and migrate data from kar_execution_state
-- Note: Constraints handled separately
-- ==================================================================




DO
$$
DECLARE
    old_table TEXT := 'kar_execution_state';
    new_table TEXT := 'kar_execution_state_v2';
    row_count_old BIGINT;
    row_count_new BIGINT;
    missing_cols INT;
    col_list TEXT := '';
    select_list TEXT := '';
    col RECORD;
    old_col_type TEXT;
    new_col_type TEXT;
    missing_in_new TEXT;
BEGIN
    ------------------------------------------------------------------
    -- 1. Check if old table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = old_table
    ) THEN
        RAISE NOTICE 'Old table "%" does not exist. Skipping migration.', old_table;
        RETURN;
    END IF;

    ------------------------------------------------------------------
    -- 2. Create new table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = new_table
    ) THEN
        RAISE NOTICE 'Creating new table "%"...', new_table;

        EXECUTE format('CREATE TABLE %I (
            id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
            nc_id UUID NOT NULL,
            nc_fk UUID NULL,
            hs_fhir_server_url VARCHAR(8000) NOT NULL,
            kar_unique_id VARCHAR(8000),
            action_status VARCHAR(8000),
            CONSTRAINT uc_kar_execution_state_v2 UNIQUE (nc_id, kar_unique_id)
        );', new_table);

        RAISE NOTICE 'Table "%" created successfully.', new_table;
    ELSE
        RAISE NOTICE 'Table "%" already exists. Proceeding with validation...', new_table;
    END IF;

    ------------------------------------------------------------------
    -- 3. Validate column consistency between old and new tables
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
    -- 4. Check if old table has data
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', old_table) INTO row_count_old;
    IF row_count_old = 0 THEN
        RAISE NOTICE 'No rows to migrate from "%".', old_table;
        RETURN;
    ELSE
        RAISE NOTICE 'Found % rows in "%". Starting migration...', row_count_old, old_table;
    END IF;

    ------------------------------------------------------------------
    -- 5. Build dynamic column list with UUID casting if needed
    ------------------------------------------------------------------
    col_list := '';
    select_list := '';

    FOR col IN
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table
        ORDER BY ordinal_position
    LOOP
        -- Determine type mappings
        SELECT data_type INTO old_col_type
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table AND column_name = col.column_name;

        SELECT data_type INTO new_col_type
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = new_table AND column_name = col.column_name;

        IF new_col_type = 'uuid' AND old_col_type IN ('character varying', 'text') THEN
            select_list := select_list || format('(%I)::uuid, ', col.column_name);
        ELSE
            select_list := select_list || format('%I, ', col.column_name);
        END IF;

        col_list := col_list || format('%I, ', col.column_name);
    END LOOP;

    -- Clean up trailing commas
    col_list := regexp_replace(col_list, ',\s*$', '');
    select_list := regexp_replace(select_list, ',\s*$', '');

    ------------------------------------------------------------------
    -- 6. Perform data migration
    ------------------------------------------------------------------
    RAISE NOTICE 'Migrating data from "%" to "%"...', old_table, new_table;
    EXECUTE format('INSERT INTO %I (%s) SELECT %s FROM %I;', new_table, col_list, select_list, old_table);

    ------------------------------------------------------------------
    -- 7. Validate row counts
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', new_table) INTO row_count_new;
    IF row_count_new = row_count_old THEN
        RAISE NOTICE 'Data migration successful. % rows copied.', row_count_new;
    ELSE
        RAISE NOTICE 'Row count mismatch! Old=% New=%. Please verify.', row_count_old, row_count_new;
    END IF;

END;
$$;
