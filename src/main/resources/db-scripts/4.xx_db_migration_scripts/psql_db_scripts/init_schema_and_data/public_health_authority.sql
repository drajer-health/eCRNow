DO
$$
DECLARE
    old_table TEXT := 'public_health_authority';
    new_table TEXT := 'public_health_authority_v2';
    row_count_old BIGINT;
    row_count_new BIGINT;
    missing_cols INT;
    col_list TEXT := '';
    select_list TEXT := '';
    col RECORD;
    missing_in_new TEXT;
    data_transfer_success BOOLEAN := FALSE;
BEGIN

    ------------------------------------------------------------------
    -- 1. Ensure old table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = old_table
    ) THEN
        RAISE NOTICE 'Old table "%" does not exist. Skipping migration.', old_table;
        RETURN;
    END IF;

    ------------------------------------------------------------------
    -- 2. Create new table (WITHOUT constraints or indexes)
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = new_table
    ) THEN
        RAISE NOTICE 'Creating new table "%"...', new_table;

        EXECUTE format($fmt$
            CREATE TABLE %I (
                id SERIAL PRIMARY KEY,
                clientId VARCHAR(8000) NOT NULL,
                clientSecret TEXT NULL,
                username VARCHAR(8000) NULL,
                password VARCHAR(8000) NULL,
                fhir_server_base_url VARCHAR(255) NOT NULL,
                fhir_version VARCHAR(255) NULL,
                token_url VARCHAR(8000) NULL,
                scopes TEXT NOT NULL,
                require_aud INT DEFAULT 0,
                auth_type VARCHAR(8000) NOT NULL,
                backend_auth_key_alias VARCHAR(8000) NULL,
                backend_auth_alg VARCHAR(8000) NULL,
                backend_auth_kid VARCHAR(8000) NULL,
                last_updated_ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
            );
        $fmt$, new_table);

        RAISE NOTICE 'Table "%" created successfully (without constraints).', new_table;
    ELSE
        RAISE NOTICE 'Table "%" already exists. Proceeding with validation...', new_table;
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

    -- Collect missing column names
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
        RAISE EXCEPTION
            'Column mismatch detected: % column(s) missing in "%": % — please add missing columns before migration.',
            missing_cols, old_table, missing_in_new;
    ELSE
        RAISE NOTICE 'All columns from "%" exist in "%".', old_table, new_table;
    END IF;


    ------------------------------------------------------------------
    -- 4. Build dynamic column list
    ------------------------------------------------------------------
    FOR col IN
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table
        ORDER BY ordinal_position
    LOOP
        IF EXISTS (
            SELECT 1
            FROM information_schema.columns
            WHERE table_schema = 'public'
              AND table_name = new_table
              AND column_name = col.column_name
        ) THEN
            col_list := col_list || format('%I, ', col.column_name);
            select_list := select_list || format('%I, ', col.column_name);
        END IF;
    END LOOP;

    col_list := regexp_replace(col_list, ',\s*$', '');
    select_list := regexp_replace(select_list, ',\s*$', '');

    ------------------------------------------------------------------
    -- 5. Count rows in old table
    ------------------------------------------------------------------
    EXECUTE format('SELECT COUNT(*) FROM %I', old_table) INTO row_count_old;

    IF row_count_old = 0 THEN
        RAISE NOTICE 'No rows to migrate from "%".', old_table;
        data_transfer_success := TRUE;
        RETURN;
    END IF;

    RAISE NOTICE 'Migrating % rows from "%" to "%"...',
        row_count_old, old_table, new_table;

    ------------------------------------------------------------------
    -- 6. Perform data migration
    ------------------------------------------------------------------
    BEGIN
        EXECUTE format('INSERT INTO %I (%s) SELECT %s FROM %I;',
                       new_table, col_list, select_list, old_table);

        EXECUTE format('SELECT COUNT(*) FROM %I', new_table) INTO row_count_new;

        IF row_count_new >= row_count_old THEN
            data_transfer_success := TRUE;
            RAISE NOTICE 'Migration successful. % rows copied.', row_count_new;
        ELSE
            data_transfer_success := FALSE;
            RAISE NOTICE 'Row count mismatch. Old=%, New=%',
                row_count_old, row_count_new;
        END IF;

    EXCEPTION WHEN OTHERS THEN
        data_transfer_success := FALSE;
        RAISE NOTICE 'Error during migration: %', SQLERRM;
    END;


    ------------------------------------------------------------------
    -- 7. Log migration outcome
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = 'migration_log'
    ) THEN
        CREATE TABLE migration_log (
            table_name VARCHAR(100),
            data_transfer BOOLEAN
        );
        RAISE NOTICE 'Created migration_log table.';
    END IF;

    INSERT INTO migration_log (table_name, data_transfer)
    VALUES (old_table, data_transfer_success);

END
$$;
