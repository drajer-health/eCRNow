-- =====================================================================
-- Script: kar_execution_state.sql
-- Purpose: Add foreign key constraint between hs_kar_status_v2 and healthcare_setting_v2
-- Author: Bhargav
-- Date: 2025-11-13
-- =====================================================================



DO
$$
DECLARE
    v_constraint_name CONSTANT TEXT := 'fk_kar_execstate_v2_notificationcontext';
BEGIN
    RAISE NOTICE '--- Starting schema alterations for table: public.kar_execution_state_v2 ---';

    ------------------------------------------------------------------
    -- 1. Add foreign key constraint if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM information_schema.table_constraints
        WHERE constraint_name = v_constraint_name
          AND table_schema = 'public'
          AND table_name = 'kar_execution_state_v2'
    ) THEN
        RAISE NOTICE '[Action] Adding foreign key constraint: %', v_constraint_name;

        ALTER TABLE public.kar_execution_state_v2
        ADD CONSTRAINT fk_kar_execstate_v2_notificationcontext
        FOREIGN KEY (nc_fk)
        REFERENCES public.notification_context(id)
        ON DELETE SET NULL
        ON UPDATE CASCADE;

        RAISE NOTICE '[Success] Foreign key % added successfully.', v_constraint_name;
    ELSE
        RAISE NOTICE '[Skip] Foreign key % already exists.', v_constraint_name;
    END IF;

    ------------------------------------------------------------------
    -- 2. Create indexes if not exists
    ------------------------------------------------------------------
    RAISE NOTICE '[Action] Ensuring performance indexes exist...';

    CREATE INDEX IF NOT EXISTS idx_kar_execstate_v2_nc_fk
        ON public.kar_execution_state_v2 (nc_fk);

    CREATE INDEX IF NOT EXISTS idx_kar_execstate_v2_nc_id
        ON public.kar_execution_state_v2 (nc_id);

    CREATE INDEX IF NOT EXISTS idx_kar_execstate_v2_kar_unique_id
        ON public.kar_execution_state_v2 (kar_unique_id);

    RAISE NOTICE '[Success] Indexes verified or created successfully.';

    ------------------------------------------------------------------
    -- 3. Validation of constraint (optional but recommended)
    ------------------------------------------------------------------
    IF EXISTS (
        SELECT 1
        FROM information_schema.table_constraints
        WHERE constraint_name = v_constraint_name
          AND table_schema = 'public'
          AND table_name = 'kar_execution_state_v2'
    ) THEN
        RAISE NOTICE '[Validate] Constraint % exists and active.', v_constraint_name;
    ELSE
        RAISE WARNING '[Warning] Constraint % not found after creation.', v_constraint_name;
    END IF;

    ------------------------------------------------------------------
    -- 4. Completion message
    ------------------------------------------------------------------
    RAISE NOTICE '[Done ] Schema alteration successfully completed for kar_execution_state_v2.';

EXCEPTION
    WHEN OTHERS THEN
        RAISE WARNING '[ ERROR] Failure during schema alteration for kar_execution_state_v2: %', SQLERRM;
        RAISE; -- rethrow to ensure deployment fails clearly
END;
$$;
