-- ==================================================================
-- Script: repos.sql (MSSQL Version)
-- Purpose: Add unique constraint on repo_name column in kar_repos_v2
-- Author: Bhargav
-- Date: 2025-11-13
-- ==================================================================

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
BEGIN TRY
    BEGIN TRANSACTION;

    PRINT '-----------------------------------------------------------';
    PRINT 'Applying constraints and indexes for kar_repos_v2...';

    ------------------------------------------------------------------
    -- 1. Add UNIQUE constraint
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM sys.key_constraints
        WHERE name = 'uq_fhir_server_base_url_new'
    )
    BEGIN
        ALTER TABLE dbo.[kar_repos_v2]
        ADD CONSTRAINT uq_repo_name_new UNIQUE (repo_name);
        PRINT 'Unique constraint uq_repo_name_new added.';
    END
    ELSE
        PRINT 'Unique constraint uq_repo_name_new already exists.';



    COMMIT TRANSACTION;
END TRY
BEGIN CATCH
    PRINT 'ERROR: ' + ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    THROW;
END CATCH;
