-----------------------------------------------------------------------
-- Script: client_details.sql
-- Purpose: Add unique constraint on fhir_server_base_url in client_details_v2
-- Author: Bhargav
-- Date: 2025-11-13
-----------------------------------------------------------------------

BEGIN TRY

    -- Add unique constraint only if it does not already exist
    IF NOT EXISTS (
        SELECT 1
        FROM sys.key_constraints kc
        WHERE kc.[name] = 'uq_fhir_server_base_url_v2'
          AND kc.[type] = 'UQ'
    )
    BEGIN
        ALTER TABLE client_details_v2
        ADD CONSTRAINT uq_fhir_server_base_url_v2
        UNIQUE (fhir_server_base_url);
    END

    PRINT 'Unique constraint check completed for client_details_v2.';

END TRY
BEGIN CATCH
    THROW; -- rethrow for SQL/Flyway logs
END CATCH;
