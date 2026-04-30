-----------------------------------------------------------------------
-- Script: healthcare_settings.sql
-- Purpose: Add unique constraint on fhir_server_base_url in healthcare_setting_v2
-- Author: Bhargav
-- Date: 2025-11-13
-----------------------------------------------------------------------


BEGIN TRY
    BEGIN TRANSACTION;

    PRINT '-----------------------------------------------------------';
    PRINT 'Applying constraints and indexes for healthcare_setting_v2...';

    ------------------------------------------------------------------
    -- 1. Add UNIQUE constraint
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM sys.key_constraints
        WHERE name = 'uq_fhir_server_base_url_v2'
    )
    BEGIN
        ALTER TABLE dbo.[healthcare_setting_v2]
        ADD CONSTRAINT uq_fhir_server_base_url_v2 UNIQUE (fhir_server_base_url);
        PRINT 'Unique constraint uq_fhir_server_base_url_v2 added.';
    END
    ELSE
        PRINT 'Unique constraint uq_fhir_server_base_url_new already exists.';



    COMMIT TRANSACTION;
END TRY
BEGIN CATCH
    PRINT 'ERROR: ' + ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    THROW;
END CATCH;
