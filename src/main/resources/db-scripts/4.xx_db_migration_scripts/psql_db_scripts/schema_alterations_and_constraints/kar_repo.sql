-- ==================================================================
-- Script: repos.sql
-- Purpose: Add unique constraint on repo_name column in kar_repos_v2
-- Author: Bhargav
-- Date: 2025-11-13
-- ==================================================================

DO $$
BEGIN
    -- Add unique constraint if not already present
    IF NOT EXISTS (
        SELECT 1
        FROM information_schema.table_constraints
        WHERE constraint_name = 'uq_kar_repos_repo_name_v2'
          AND table_name = 'kar_repos_v2'
    ) THEN
        ALTER TABLE public.kar_repos_v2
        ADD CONSTRAINT uq_kar_repos_repo_name_v2 UNIQUE (repo_name);

        RAISE NOTICE 'Unique constraint uq_kar_repos_repo_name_v2 added successfully.';
    ELSE
        RAISE NOTICE 'Unique constraint uq_kar_repos_repo_name_v2 already exists, skipping.';
    END IF;
END $$;
