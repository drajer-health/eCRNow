-- =====================================================================
-- Script : public_health_authority.sql (UPDATED VERSION)
-- Purpose: Create public_health_authority_v2 and migrate data from
--          public_health_authority (if exists), with full validation.
-- =====================================================================

-- Create sequence for Hibernate ID generation
CREATE SEQUENCE IF NOT EXISTS public.public_health_authority_v2_seq
    START WITH 1
    INCREMENT BY 50
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

DO
$$
DECLARE
    old_table TEXT := 'public_health_authority';
    new_table TEXT := 'public_health_authority_v2';
    row_count_old BIGINT := 0;
    row_count_new_before BIGINT := 0;
    row_count_new_after BIGINT := 0;
    missing_cols INT := 0;
    missing_in_new TEXT := '';
    col_list TEXT := '';
    select_list TEXT := '';
    col RECORD;
    data_transfer_success BOOLEAN := FALSE;
BEGIN

    ------------------------------------------------------------------
    -- 0. If OLD table does NOT exist → create NEW table (if needed) then EXIT
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = old_table
    ) THEN

        RAISE NOTICE 'Source table "%" does not exist. Creating "%" (if needed) and skipping migration.',
            old_table, new_table;

        -- Create v2 table if not exists
        IF NOT EXISTS (
            SELECT 1 FROM information_schema.tables
            WHERE table_schema = 'public'
              AND table_name = new_table
        ) THEN

            RAISE NOTICE 'Creating table "%" ...', new_table;

            EXECUTE format($fmt$
                CREATE TABLE %I (
                    id INTEGER DEFAULT nextval('public_health_authority_v2_seq') PRIMARY KEY,
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

            RAISE NOTICE 'Table "%" created successfully.', new_table;

        ELSE
            RAISE NOTICE 'Table "%" already exists.', new_table;
        END IF;

        RETURN;
    END IF;


    ------------------------------------------------------------------
    -- 1. Old table exists → ensure new table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = new_table
    ) THEN

        RAISE NOTICE 'Creating table "%" ...', new_table;

        EXECUTE format($fmt$
            CREATE TABLE %I (
                id INTEGER DEFAULT nextval('public_health_authority_v2_seq') PRIMARY KEY,
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

        RAISE NOTICE 'Table "%" created.', new_table;

    ELSE
        RAISE NOTICE 'Table "%" already exists. Proceeding...', new_table;
    END IF;


    ------------------------------------------------------------------
    -- 2. Validate column compatibility
    ------------------------------------------------------------------
    SELECT COUNT(*) INTO missing_cols
    FROM (
        SELECT column_name FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = new_table
        EXCEPT
        SELECT column_name FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table
    ) diff;

    SELECT COALESCE(string_agg(column_name, ', '), 'None')
    INTO missing_in_new
    FROM (
        SELECT column_name FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = new_table
        EXCEPT
        SELECT column_name FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = old_table
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
        RAISE NOTICE 'No data to migrate from "%". Skipping.', old_table;
        data_transfer_success := TRUE;
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
        WHERE table_schema = 'public' AND table_name = old_table
        ORDER BY ordinal_position
    LOOP
        IF EXISTS (
            SELECT 1 FROM information_schema.columns
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

    RAISE NOTICE 'Inserted % rows into "%".',
        (row_count_new_after - row_count_new_before), new_table;

    data_transfer_success := (row_count_new_after - row_count_new_before) = row_count_old;


    ------------------------------------------------------------------
    -- 6. Log migration outcome
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = 'migration_log'
    ) THEN
        CREATE TABLE migration_log (
            table_name VARCHAR(100),
            data_transfer BOOLEAN
        );
        RAISE NOTICE 'Created migration_log table.';
    END IF;

    INSERT INTO migration_log (table_name, data_transfer)
    VALUES (old_table, data_transfer_success);

END;
$$;
