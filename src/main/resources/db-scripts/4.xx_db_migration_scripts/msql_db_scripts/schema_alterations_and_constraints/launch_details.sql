-- =====================================================================
-- Script: launch_details.sql (MSSQL Version)
-- Purpose: Add unique constraint on (ehr_server_url, launch_patient_id, encounter_id)
--          in launch_details_v2 table if not already present
-- Author: Bhargav K
-- Date: 2025-11-13
-- =====================================================================

BEGIN TRY
    PRINT '--- Starting schema update for launch_details_v2 ---';

    -- 1. Add Unique Constraint if not exists
    IF NOT EXISTS (
        SELECT 1
        FROM sys.key_constraints
        WHERE name = 'uc_launch_details_v2'
    )
    BEGIN
        ALTER TABLE dbo.launch_details_v2
        ADD CONSTRAINT uc_launch_details_v2
        UNIQUE (ehr_server_url, launch_patient_id, encounter_id);

        PRINT 'Unique constraint uc_launch_details_v2 added successfully.';
    END
    ELSE
    BEGIN
        PRINT 'Unique constraint uc_launch_details_v2 already exists. Skipping.';
    END;

    PRINT 'Schema update completed for launch_details_v2.';
END TRY
BEGIN CATCH
    PRINT 'Error applying unique constraint uc_launch_details_v2: ' + ERROR_MESSAGE();
    THROW;
END CATCH;
