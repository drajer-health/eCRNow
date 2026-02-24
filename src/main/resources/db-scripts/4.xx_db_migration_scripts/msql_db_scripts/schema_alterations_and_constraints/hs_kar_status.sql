-----------------------------------------------------------------------
-- Script: hs_kar_status.sql
-- Purpose: Add foreign key constraint between hs_kar_status_v2 
--          and healthcare_setting_v2
-- Author: Bhargav
-- Date: 2025-11-13
-----------------------------------------------------------------------

BEGIN TRY

    -- Check if the foreign key constraint already exists
    IF NOT EXISTS (
        SELECT 1
        FROM sys.foreign_keys
        WHERE name = 'fk_hs_kar_status_healthcare_setting'
    )
    BEGIN
        ALTER TABLE hs_kar_status_v2
        ADD CONSTRAINT fk_hs_kar_status_healthcare_setting
            FOREIGN KEY (hs_fk)
            REFERENCES healthcare_setting_v2(id)
            ON DELETE SET NULL;
            -- Note: SQL Server does NOT support ON UPDATE CASCADE
            -- unless the PK is created with it, and it must match PK definition.
    END

    PRINT 'Foreign key constraint check completed for hs_kar_status_v2.';

END TRY
BEGIN CATCH
    THROW; -- Re-throw for SQL Server/Flyway logs
END CATCH;

-----------------------------------------------------------------------
-- End of Script
-----------------------------------------------------------------------
