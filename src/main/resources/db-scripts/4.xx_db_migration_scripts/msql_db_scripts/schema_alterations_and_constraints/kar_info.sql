-----------------------------------------------------------------------
-- Script: kar_info.sql
-- Purpose: Add foreign key constraint between kar_info_v2 and kar_repos_v2
-- Author: Bhargav
-- Date: 2025-11-13
-----------------------------------------------------------------------

BEGIN TRY

    -- Check if the foreign key already exists
    IF NOT EXISTS (
        SELECT 1
        FROM sys.foreign_keys
        WHERE name = 'fk_kar_info_v2_repo_id'
    )
    BEGIN
        ALTER TABLE kar_info_v2
        ADD CONSTRAINT fk_kar_info_v2_repo_id
            FOREIGN KEY (repo_id)
            REFERENCES kar_repos_v2(id)
            ON DELETE SET NULL;
            -- NOTE: SQL Server does NOT support ON UPDATE CASCADE
    END

    PRINT 'Foreign key validation completed for kar_info_v2.';

END TRY
BEGIN CATCH
    THROW;  -- rethrow error for Flyway logs
END CATCH;
