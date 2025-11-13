-- =====================================================================
-- Script: healthcare_setting.sql
-- Purpose: Add unique constraint on fhir_server_base_url column in client_details_v2
-- Author: Bhargav
-- Date: 2025-11-13
-- =====================================================================



DO
$$
DECLARE
    duplicate_count INT;
BEGIN
    SELECT COUNT(*) INTO duplicate_count
    FROM (
        SELECT fhir_server_base_url
        FROM healthcare_setting_v2
        GROUP BY fhir_server_base_url
        HAVING COUNT(*) > 1
    ) d;

    IF duplicate_count > 0 THEN
        RAISE EXCEPTION 'Found % duplicate fhir_server_base_url values. Resolve duplicates before adding constraint.', duplicate_count;
    ELSE
        RAISE NOTICE 'No duplicates found. Proceeding with constraint creation.';
    END IF;
END;
$$;
------------------------------------------------------------------
-- 2. Add UNIQUE constraint and index
------------------------------------------------------------------
ALTER TABLE healthcare_setting_v2
    ADD CONSTRAINT uq_fhir_server_base_url_v2
    UNIQUE (fhir_server_base_url);

CREATE INDEX IF NOT EXISTS idx_fsb_url_v2
    ON healthcare_setting_v2 (fhir_server_base_url);
