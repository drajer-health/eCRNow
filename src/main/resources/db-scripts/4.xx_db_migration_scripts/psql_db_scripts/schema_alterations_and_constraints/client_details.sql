-- =====================================================================
-- Script: client_details_add_unique_fhir_server_base_url.sql
-- Purpose: Add unique constraint on fhir_server_base_url column in client_details_v2
-- Author: Bhargav
-- Date: 2025-11-13
-- =====================================================================

DO
$$
BEGIN
    RAISE NOTICE '--- Starting unique constraint addition for table: client_details_v2 ---';

    IF NOT EXISTS (
        SELECT 1
        FROM information_schema.table_constraints
        WHERE constraint_name = 'uq_client_details_v2_fhir_server_base_url'
          AND table_name = 'client_details_v2'
    ) THEN
        RAISE NOTICE '[Action] Adding unique constraint: uq_client_details_v2_fhir_server_base_url';

        ALTER TABLE public.client_details_v2
        ADD CONSTRAINT uq_client_details_v2_fhir_server_base_url
        UNIQUE (fhir_server_base_url);

        RAISE NOTICE '[Success] Unique constraint uq_client_details_v2_fhir_server_base_url added successfully.';
    ELSE
        RAISE NOTICE '[Skip] Unique constraint uq_client_details_v2_fhir_server_base_url already exists.';
    END IF;

    RAISE NOTICE '[Done ✅] Unique constraint setup completed for client_details_v2.';
END;
$$;
