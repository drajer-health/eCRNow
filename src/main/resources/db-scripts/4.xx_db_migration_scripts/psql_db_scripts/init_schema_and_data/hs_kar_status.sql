-- ==================================================================
-- hs_kar_status.sql
-- Purpose: Create hs_kar_status_v2 and migrate data from hs_kar_status
-- Note: Constraints handled separately
-- ==================================================================

-- Create sequence for Hibernate ID generation
CREATE SEQUENCE IF NOT EXISTS public.hs_kar_status_v2_seq
    START WITH 1
    INCREMENT BY 50
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

DO
$$
DECLARE
    old_table TEXT := 'hs_kar_status';
    new_table TEXT := 'hs_kar_status_v2';
    row_count_old BIGINT;
    row_count_new BIGINT;
    missing_cols INT;
    col_list TEXT := '';
    select_list TEXT := '';
    missing_in_new TEXT;
    col RECORD;
BEGIN
    ------------------------------------------------------------------
    -- 1. Check if old table exists (NO EXCEPTION IF MISSING)
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = old_table
    ) THEN
        RAISE NOTICE 'Old table "%" does not exist. Skipping migration.', old_table;

        -- Still ensure new table exists
        IF NOT EXISTS (
            SELECT 1 FROM information_schema.tables
            WHERE table_schema = 'public' AND table_name = new_table
        ) THEN
            RAISE NOTICE 'Creating new table "%" as it does not exist...', new_table;

            EXECUTE format($fmt$
                CREATE TABLE %I (
                    id INTEGER DEFAULT nextval('hs_kar_status_v2_seq') PRIMARY KEY,
                    hs_id INTEGER NOT NULL,
                    hs_fk INTEGER,
                    kar_id VARCHAR(255) NOT NULL,
                    kar_version VARCHAR(8000) NOT NULL,
                    map_versionid_karid VARCHAR(8000) NOT NULL,
                    is_activated INTEGER NOT NULL DEFAULT 0,
                    last_activation_date TIMESTAMP,
                    last_inactivation_date TIMESTAMP,
                    is_subscriptions_enabled INTEGER NOT NULL DEFAULT 0,
                    subscriptions TEXT,
                    is_only_covid INTEGER NOT NULL DEFAULT 0,
                    output_format VARCHAR(8000)
                );
            $fmt$, new_table);

            EXECUTE format('CREATE INDEX IF NOT EXISTS idx_hs_id_v2 ON %I (hs_id);', new_table);

            RAISE NOTICE 'Table "%" created successfully.', new_table;
        END IF;

        RETURN;
    END IF;

    ------------------------------------------------------------------
    -- 2. Create new table if not exists (no constraints)
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = new_table
    ) THEN
        RAISE NOTICE 'Creating new table "%"...', new_table;

        EXECUTE format($fmt$
            CREATE TABLE %I (
                id INTEGER DEFAULT nextval('hs_kar_status_v2_seq') PRIMARY KEY,
                hs_id INTEGER NOT NULL,
                hs_fk INTEGER,
                kar_id VARCHAR(255) NOT NULL,
                kar_version VARCHAR(8000) NOT NULL,
                map_versionid_karid VARCHAR(8000) NOT NULL,
                is_activated INTEGER NOT NULL DEFAULT 0,
                last_activation_date TIMESTAMP,
                last_inactivation_date TIMESTAMP,
                is_subscriptions_enabled INTEGER NOT NULL DEFAULT 0,
                subscriptions TEXT,
                is_only_covid INTEGER NOT NULL DEFAULT 0,
                output_format VARCHAR(8000)
            );
        $fmt$, new_table);

        EXECUTE format('CREATE INDEX IF NOT EXISTS idx_hs_id_v2 ON %I (hs_id);', new_table);

        RAISE NOTICE 'Table "%" created successfully.', new_table;
    ELSE
        RAISE NOTICE 'Table "%" already exists. Proceeding with validation...', new_table;
    END IF;

    ------------------------------------------------------------------
    -- 3. Verify column compatibility
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
        RAISE EXCEPTION 'Column mismatch: % column(s) missing in "%": % — please fix before migration.',
            missing_cols, old_table, missing_in_new;
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
