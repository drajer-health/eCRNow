-- ==================================================================
-- kar_execution_state.sql
-- Purpose: Create kar_execution_state_v2 and safely migrate data
-- ==================================================================

DO
$$
DECLARE
    old_table TEXT := 'kar_execution_state';
    new_table TEXT := 'kar_execution_state_v2';
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
    -- 0. If old table does NOT exist → create new table (if needed) and EXIT
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = old_table
    ) THEN

        RAISE NOTICE 'Old table "%" does not exist. Creating "%" (if required) and skipping migration.',
            old_table, new_table;

        -- Create new table if missing
        IF NOT EXISTS (
            SELECT 1 FROM information_schema.tables
            WHERE table_schema = 'public' AND table_name = new_table
        ) THEN
            RAISE NOTICE 'Creating new table "%"...', new_table;

            EXECUTE format($fmt$
                CREATE TABLE %I (
                    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
                    nc_id UUID NOT NULL,
                    nc_fk UUID NULL,
                    hs_fhir_server_url VARCHAR(8000) NOT NULL,
                    kar_unique_id VARCHAR(8000),
                    action_status VARCHAR(8000)
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
        RAISE NOTICE 'New table "%" missing. Creating it...', new_table;

        EXECUTE format($fmt$
            CREATE TABLE %I (
                id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
                nc_id UUID NOT NULL,
                nc_fk UUID NULL,
                hs_fhir_server_url VARCHAR(8000) NOT NULL,
                kar_unique_id VARCHAR(8000),
                action_status VARCHAR(8000)
            );
        $fmt$, new_table);

        RAISE NOTICE 'Table "%" created.', new_table;
    ELSE
        RAISE NOTICE 'Table "%" already exists. Proceeding...', new_table;
    END IF;

    ------------------------------------------------------------------
    -- 2. Verify column compatibility
    ------------------------------------------------------------------
    SELECT COUNT(*) INTO missing_cols
    FROM (
        SELECT column_name
        FROM information_schema.columns
        WHERE table_name = new_table
        EXCEPT
        SELECT column_name
        FROM information_schema.columns
        WHERE table_name = old_table
    ) diff;

    SELECT COALESCE(string_agg(column_name, ', '), 'None')
    INTO missing_in_new
    FROM (
        SELECT column_name
        FROM information_schema.columns
        WHERE table_name = new_table
        EXCEPT
        SELECT column_name
        FROM information_schema.columns
        WHERE table_name = old_table
    ) diff;

    IF missing_cols > 0 THEN
        RAISE EXCEPTION
            'Column mismatch: % column(s) missing in "%": %',
            missing_cols, old_table, missing_in_new;
    ELSE
        RAISE NOTICE 'Column validation OK.';
    END IF;

    ------------------------------------------------------------------
    -- 3. Count rows in old table
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', old_table)
        INTO row_count_old;

    IF row_count_old = 0 THEN
        RAISE NOTICE 'Old table "%" has no rows. Skipping migration.', old_table;
        RETURN;
    END IF;

    RAISE NOTICE 'Starting migration of % rows...', row_count_old;

    ------------------------------------------------------------------
    -- 4. Build dynamic column list with UUID casting if needed
    ------------------------------------------------------------------
    FOR col IN
        SELECT column_name
        FROM information_schema.columns
        WHERE table_name = old_table
        ORDER BY ordinal_position
    LOOP
        -- check if column also exists in new table
        IF EXISTS (
            SELECT 1 FROM information_schema.columns
            WHERE table_name = new_table AND column_name = col.column_name
        ) THEN

            SELECT data_type INTO old_col_type
            FROM information_schema.columns
            WHERE table_name = old_table AND column_name = col.column_name;

            SELECT data_type INTO new_col_type
            FROM information_schema.columns
            WHERE table_name = new_table AND column_name = col.column_name;

            IF new_col_type = 'uuid' AND old_col_type IN ('character varying', 'text') THEN
                select_list := select_list || format('(%I)::uuid, ', col.column_name);
            ELSE
                select_list := select_list || format('%I, ', col.column_name);
            END IF;

            col_list := col_list || format('%I, ', col.column_name);
        END IF;
    END LOOP;

    col_list := regexp_replace(col_list, ',\s*$', '');
    select_list := regexp_replace(select_list, ',\s*$', '');

    ------------------------------------------------------------------
    -- 5. Migration execution
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', new_table)
        INTO row_count_new_before;

    EXECUTE format(
        'INSERT INTO %I (%s) SELECT %s FROM %I;',
        new_table, col_list, select_list, old_table
    );

    EXECUTE format('SELECT COUNT(*) FROM %I', new_table)
        INTO row_count_new_after;

    RAISE NOTICE 'Migration complete. Rows inserted: %',
        (row_count_new_after - row_count_new_before);

EXCEPTION
    WHEN OTHERS THEN
        RAISE NOTICE 'Error: %', SQLERRM;
        RAISE;
END;
$$;
