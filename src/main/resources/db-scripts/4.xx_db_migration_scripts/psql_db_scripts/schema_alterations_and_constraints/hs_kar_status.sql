-- =====================================================================
-- Script: hs_kar_status.sql
-- Purpose: Add foreign key constraint between hs_kar_status_v2 and healthcare_setting_v2
-- Author: Bhargav
-- Date: 2025-11-13
-- =====================================================================

DO
$$
BEGIN
    -- Check if the constraint already exists
    IF NOT EXISTS (
        SELECT 1
        FROM information_schema.table_constraints
        WHERE constraint_type = 'FOREIGN KEY'
          AND table_name = 'hs_kar_status_v2'
          AND constraint_name = 'fk_hs_kar_status_healthcare_setting'
    ) THEN

        -- Add the foreign key constraint safely
        ALTER TABLE public.hs_kar_status_v2
        ADD CONSTRAINT fk_hs_kar_status_healthcare_setting
        FOREIGN KEY (hs_fk)
        REFERENCES public.healthcare_setting_v2(id)
        ON DELETE SET NULL
        ON UPDATE CASCADE;

        RAISE NOTICE 'Constraint fk_hs_kar_status_healthcare_setting added successfully.';

    ELSE
        RAISE NOTICE 'Constraint fk_hs_kar_status_healthcare_setting already exists. Skipping.';
    END IF;
END
$$;

-- =====================================================================
-- End of Script
-- =====================================================================
