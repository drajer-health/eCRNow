-- =====================================================================
-- Script: public_health_authority.sql
-- Purpose: Add unique constraint on fhir_server_base_url in public_health_authority_v2
-- Author: Bhargav
-- Date: 2025-11-13
-- =====================================================================

DO
$$
BEGIN
    ------------------------------------------------------------------
    -- 1. Add Unique Constraint if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM information_schema.table_constraints
        WHERE table_name = 'public_health_authority_v2'
          AND constraint_name = 'uc_public_health_authority_v2'
    ) THEN
        ALTER TABLE public.public_health_authority_v2
        ADD CONSTRAINT uc_public_health_authority_v2
        UNIQUE (fhir_server_base_url);

        RAISE NOTICE 'Unique constraint uc_public_health_authority_v2 added successfully on fhir_server_base_url.';
    ELSE
        RAISE NOTICE 'Unique constraint uc_public_health_authority_v2 already exists. Skipping.';
    END IF;

    ------------------------------------------------------------------
    -- 2. Completion Message
    ------------------------------------------------------------------
    RAISE NOTICE 'Schema update completed for public_health_authority_v2.';

EXCEPTION
    WHEN OTHERS THEN
        RAISE WARNING 'Error applying unique constraint uc_public_health_authority_v2: %', SQLERRM;
        RAISE;
END;
$$;
