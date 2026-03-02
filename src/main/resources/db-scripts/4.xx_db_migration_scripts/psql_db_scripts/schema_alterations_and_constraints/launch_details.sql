-- =====================================================================
-- Script: launch_details.sql
-- Purpose: Add unique constraint on (ehr_server_url, launch_patient_id, encounter_id)
--          in launch_details_v2 table if not already present
-- Author: Bhargav k
-- Date: 2025-11-13
-- =====================================================================

DO
$$
BEGIN
    ------------------------------------------------------------------
    -- 1. Add Unique Constraint (if not exists)
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM information_schema.table_constraints
        WHERE table_name = 'launch_details_v2'
          AND constraint_name = 'uc_launch_details_v2'
    ) THEN
        ALTER TABLE public.launch_details_v2
        ADD CONSTRAINT uc_launch_details_v2
        UNIQUE (ehr_server_url, launch_patient_id, encounter_id);

        RAISE NOTICE 'Unique constraint uc_launch_details_v2 added successfully on (ehr_server_url, launch_patient_id, encounter_id).';
    ELSE
        RAISE NOTICE 'Unique constraint uc_launch_details_v2 already exists. Skipping.';
    END IF;

    ------------------------------------------------------------------
    -- 2. Completion Message
    ------------------------------------------------------------------
    RAISE NOTICE 'Schema update completed for launch_details_v2.';

EXCEPTION
    WHEN OTHERS THEN
        RAISE WARNING 'Error applying unique constraint uc_launch_details_v2: %', SQLERRM;
        RAISE;
END;
$$;
