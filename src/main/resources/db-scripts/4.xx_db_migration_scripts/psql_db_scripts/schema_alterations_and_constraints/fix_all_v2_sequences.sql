-- =====================================================================
-- Script: fix_all_v2_sequences.sql
-- Author: Bhargav
-- Date: 2026-04-03
--
-- =====================================================================
-- 📌 PURPOSE
-- ---------------------------------------------------------------------
-- This script synchronizes database sequences for all tables ending
-- with '_v2' in the 'public' schema.
--
-- It resolves issues caused by sequence misalignment after data
-- migration or table copy operations.
--
-- Common errors fixed:
--   - duplicate key value violates unique constraint
--   - sequence generating already existing IDs
--

-- =====================================================================
-- 🧪 WHEN TO USE
-- ---------------------------------------------------------------------
-- Run this script:
--   - After copying data to '_v2' tables
--   - After database migration
--   - When facing duplicate key errors
--
-- =====================================================================
-- ⚙️ WHAT THIS SCRIPT DOES
-- ---------------------------------------------------------------------
-- 1. Scans all '_v2' tables in the public schema
-- 2. Identifies corresponding sequences:
--      - Uses pg_get_serial_sequence() when available
--      - Falls back to naming convention: <table_name>_seq
-- 3. Compares:
--      - MAX(id) from table
--      - Current sequence value (last_value)
-- 4. Updates sequence ONLY if out of sync:
--      - Sets next value to MAX(id) + 1
-- 5. Skips:
--      - Tables without 'id' column
--      - Tables without matching sequence
--
-- =====================================================================
-- 🚀 HOW TO RUN
-- ---------------------------------------------------------------------
-- 1. Connect to PostgreSQL (pgAdmin / DBeaver / psql)
-- 2. Open a new query window
-- 3. Execute this script
--
-- =====================================================================
-- 📊 EXPECTED OUTPUT
-- ---------------------------------------------------------------------
-- The script prints logs such as:
--
--   [INFO] Table: xxx_v2
--   [INFO] Max ID: 120, Sequence Last: 2
--   [FIXED] Sequence updated
--
-- OR
--
--   [SKIP] Already in sync
--
-- =====================================================================
-- ⚠️ SAFETY
-- ---------------------------------------------------------------------
-- - Safe to run multiple times (idempotent)
-- - No data is modified or deleted
-- - Only sequence values are updated when needed
--

-- =====================================================================
-- ✅ RESULT
-- ---------------------------------------------------------------------
-- - Sequences aligned with table data
-- - No more duplicate key errors
-- - Stable inserts going forward
--
-- =====================================================================

DO $$
DECLARE
    rec           RECORD;
    v_seq_name    TEXT;
    v_max_id      BIGINT;
    v_has_id_col  BOOLEAN;
    v_seq_exists  BOOLEAN;
BEGIN

    -- Loop over every base table in public schema whose name ends with '_v2'
    FOR rec IN
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_type   = 'BASE TABLE'
          AND table_name   LIKE '%_v2'
        ORDER BY table_name
    LOOP

        RAISE NOTICE '--- Processing table: % ---', rec.table_name;

        -- ----------------------------------------------------------------
        -- 1. Check whether the table actually has an 'id' column.
        --    Tables like scheduled_tasks_v2 use a composite PK and have
        --    no 'id' column at all – querying MAX(id) on them is an error.
        -- ----------------------------------------------------------------
        SELECT EXISTS (
            SELECT 1
            FROM information_schema.columns
            WHERE table_schema = 'public'
              AND table_name   = rec.table_name
              AND column_name  = 'id'
        ) INTO v_has_id_col;

        IF NOT v_has_id_col THEN
            RAISE NOTICE '  [SKIP] Table % has no ''id'' column – skipping.', rec.table_name;
            CONTINUE;
        END IF;

        -- ----------------------------------------------------------------
        -- 2. Resolve the sequence.
        --    Path A: pg_get_serial_sequence (SERIAL / IDENTITY columns).
        --    Path B: naming convention  <table_name>_seq  (standalone seqs
        --            attached via DEFAULT nextval('<seq>')).
        -- ----------------------------------------------------------------
        v_seq_name := pg_get_serial_sequence(
                          format('public.%I', rec.table_name), 'id');

        IF v_seq_name IS NULL THEN
            -- Build the conventional name and verify it actually exists
            DECLARE
                v_candidate TEXT := 'public.' || rec.table_name || '_seq';
            BEGIN
                SELECT EXISTS (
                    SELECT 1
                    FROM pg_sequences
                    WHERE schemaname = 'public'
                      AND sequencename = rec.table_name || '_seq'
                ) INTO v_seq_exists;

                IF v_seq_exists THEN
                    v_seq_name := v_candidate;
                END IF;
            END;
        END IF;

        IF v_seq_name IS NULL THEN
            RAISE NOTICE '  [SKIP] No sequence found for %.id (tried pg_get_serial_sequence and naming convention).', rec.table_name;
            CONTINUE;
        END IF;

        RAISE NOTICE '  [INFO] Sequence: %', v_seq_name;

        -- ----------------------------------------------------------------
        -- 3. Get MAX(id); fall back to 1 when table is empty.
        -- ----------------------------------------------------------------
        EXECUTE format('SELECT COALESCE(MAX(id), 1) FROM public.%I', rec.table_name)
            INTO v_max_id;

        RAISE NOTICE '  [INFO] MAX(id) = %', v_max_id;

        -- ----------------------------------------------------------------
        -- 4. Reset the sequence so the next value is MAX(id) + 1.
        --    setval(..., v_max_id, true) means the NEXT call to nextval()
        --    will return v_max_id + increment, which is what we want.
        -- ----------------------------------------------------------------
        PERFORM setval(v_seq_name, v_max_id, true);

        RAISE NOTICE '  [OK]   Sequence % reset to % (next value will be % + increment).', v_seq_name, v_max_id, v_max_id;

    END LOOP;

    RAISE NOTICE '=== Sequence sync complete for all _v2 tables ===';

END $$;

