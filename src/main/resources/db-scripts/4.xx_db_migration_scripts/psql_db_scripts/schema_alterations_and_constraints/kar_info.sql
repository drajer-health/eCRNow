-- =====================================================================
-- Script: kar_info.sql
-- Purpose: Add foreign key constraint between kar_info_v2 and kar_repos_v2
-- Author: Bhargav
-- Date: 2025-11-13
-- =====================================================================

DO
$$
BEGIN
    RAISE NOTICE '--- Starting foreign key addition for table: kar_info_v2 ---';

    IF NOT EXISTS (
        SELECT 1
        FROM information_schema.table_constraints
        WHERE constraint_name = 'fk_kar_info_v2_repo_id'
          AND table_name = 'kar_info_v2'
    ) THEN
        RAISE NOTICE '[Action] Adding foreign key constraint: fk_kar_info_v2_repo_id';

        ALTER TABLE public.kar_info_v2
        ADD CONSTRAINT fk_kar_info_v2_repo_id
        FOREIGN KEY (repo_id)
        REFERENCES public.kar_repos_v2(id)
        ON DELETE SET NULL
        ON UPDATE CASCADE;

        RAISE NOTICE '[Success] Foreign key fk_kar_info_v2_repo_id added successfully.';
    ELSE
        RAISE NOTICE '[Skip] Foreign key fk_kar_info_v2_repo_id already exists.';
    END IF;
END;
$$;
