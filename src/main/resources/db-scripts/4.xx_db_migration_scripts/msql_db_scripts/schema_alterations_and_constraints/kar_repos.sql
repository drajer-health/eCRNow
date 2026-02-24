-- ==================================================================
-- Script: repos.sql (MSSQL Version)
-- Purpose: Add unique constraint on repo_name column in kar_repos_v2
-- Author: Bhargav
-- Date: 2025-11-13
-- ==================================================================

BEGIN TRY
    BEGIN TRANSACTION;

    -- Check if the unique constraint exists
    IF NOT EXISTS (
        SELECT 1
        FROM sys.key_constraints
        WHERE name = 'uq_kar_repos_repo_name_v2'
    )
    BEGIN
        ALTER TABLE dbo.kar_repos_v2
        ADD CONSTRAINT uq_kar_repos_repo_name_v2 UNIQUE (repo_name);

        PRINT 'Unique constraint uq_kar_repos_repo_name_v2 added successfully.';
    END
    ELSE
    BEGIN
        PRINT 'Unique constraint uq_kar_repos_repo_name_v2 already exists. Skipping.';
    END;

    COMMIT TRANSACTION;
END TRY
BEGIN CATCH
    PRINT 'ERROR: ' + ERROR_MESSAGE();

    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    THROW;
END CATCH;