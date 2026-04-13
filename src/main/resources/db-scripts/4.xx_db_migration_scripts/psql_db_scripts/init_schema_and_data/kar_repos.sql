-- ==================================================================
-- kar_repos.sql (UPDATED VERSION)
-- Purpose: Create kar_repos_v2 and migrate data from kar_repos
-- ==================================================================

-- Create sequence for Hibernate ID generation
CREATE SEQUENCE IF NOT EXISTS public.kar_repos_v2_seq
    START WITH 1
    INCREMENT BY 50
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

DO
$$
DECLARE
    old_table TEXT := 'kar_repos';
    new_table TEXT := 'kar_repos_v2';
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
    -- 0. If old table does NOT exist → create new table (if needed) & EXIT
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = old_table
    ) THEN

        RAISE NOTICE 'Old table "%" does not exist. Creating "%" (if needed) and skipping migration.',
            old_table, new_table;

        -- Create new table if not exists
        IF NOT EXISTS (
            SELECT 1 FROM information_schema.tables
            WHERE table_schema = 'public' AND table_name = new_table
        ) THEN

            RAISE NOTICE 'Creating new table "%"...', new_table;

            EXECUTE format($fmt$
                CREATE TABLE %I (
                    id INTEGER DEFAULT nextval('kar_repos_v2_seq') PRIMARY KEY,
                    repo_fhir_url VARCHAR(8000) NOT NULL,
                    repo_name VARCHAR(255) NOT NULL,
                    repo_status INT
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

        RAISE NOTICE 'Creating new table "%"...', new_table;

        EXECUTE format($fmt$
            CREATE TABLE %I (
                id INTEGER DEFAULT nextval('kar_repos_v2_seq') PRIMARY KEY,
                repo_fhir_url VARCHAR(8000) NOT NULL,
                repo_name VARCHAR(255) NOT NULL,
                repo_status INT
            );
        $fmt$, new_table);

        RAISE NOTICE 'Table "%" created.', new_table;

    ELSE
        RAISE NOTICE 'Table "%" already exists. Proceeding with migration...', new_table;
    END IF;


    ------------------------------------------------------------------
    -- 2. Verify column compatibility (new table must not have extra columns)
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
        RAISE EXCEPTION 'Column mismatch: % column(s) missing in "%": %',
            missing_cols, old_table, missing_in_new;
    ELSE
        RAISE NOTICE 'Column compatibility OK.';
    END IF;


    ------------------------------------------------------------------
    -- 3. Count old table rows
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', old_table)
    INTO row_count_old;

    IF row_count_old = 0 THEN
        RAISE NOTICE 'No rows to migrate from "%".', old_table;
        RETURN;
    END IF;

    RAISE NOTICE 'Found % rows in "%". Starting migration...', row_count_old, old_table;


    ------------------------------------------------------------------
    -- 4. Build column list (dynamic)
    ------------------------------------------------------------------
    col_list := '';
    select_list := '';

    FOR col IN
        SELECT column_name
        FROM information_schema.columns
        WHERE table_name = old_table
        ORDER BY ordinal_position
    LOOP
        IF EXISTS (
            SELECT 1 FROM information_schema.columns
            WHERE table_name = new_table
              AND column_name = col.column_name
        ) THEN
            col_list := col_list || format('%I, ', col.column_name);
            select_list := select_list || format('%I, ', col.column_name);
        END IF;
    END LOOP;

    col_list := regexp_replace(col_list, ',\s*$', '');
    select_list := regexp_replace(select_list, ',\s*$', '');


    ------------------------------------------------------------------
    -- 5. Insert data
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', new_table)
    INTO row_count_new_before;

    EXECUTE format(
        'INSERT INTO %I (%s) SELECT %s FROM %I;',
        new_table, col_list, select_list, old_table
    );

    EXECUTE format('SELECT COUNT(*) FROM %I', new_table)
    INTO row_count_new_after;

    RAISE NOTICE 'Data migrated: % new rows inserted.',
        (row_count_new_after - row_count_new_before);

END;
$$;
