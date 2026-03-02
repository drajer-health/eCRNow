-- =====================================================================
-- Script: public_health_authority.sql (MSSQL Version)
-- Purpose: Add unique constraint on fhir_server_base_url
--          in public_health_authority_v2 table if not already present
-- Author: Bhargav
-- Date: 2025-11-13
-- =====================================================================

BEGIN TRY
    PRINT '--- Starting schema update for public_health_authority_v2 ---';

    ------------------------------------------------------------------
    -- 1. Add Unique Constraint if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM sys.key_constraints
        WHERE name = 'uc_public_health_authority_v2'
    )
    BEGIN
        ALTER TABLE dbo.public_health_authority_v2
        ADD CONSTRAINT uc_public_health_authority_v2
        UNIQUE (fhir_server_base_url);

        PRINT 'Unique constraint uc_public_health_authority_v2 added successfully on fhir_server_base_url.';
    END
    ELSE
    BEGIN
        PRINT 'Unique constraint uc_public_health_authority_v2 already exists. Skipping.';
    END;

    PRINT 'Schema update completed for public_health_authority_v2.';
END TRY
BEGIN CATCH
    PRINT 'Error applying unique constraint uc_public_health_authority_v2: ' + ERROR_MESSAGE();
    THROW;
END CATCH;
