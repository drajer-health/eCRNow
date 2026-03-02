-- =====================================================================
-- Script: notification_context.sql
-- Purpose: Add unique constraint to notification_context_v2
--          and fix duplicate trigger_event values before applying constraint
-- Author: Bhargav
-- Date: 2025-11-13
-- =====================================================================

DO
$$
DECLARE
    duplicate_count INTEGER;
BEGIN
    ------------------------------------------------------------------
    -- 1. Check for duplicate entries before adding constraint
    ------------------------------------------------------------------
    SELECT COUNT(*) INTO duplicate_count
    FROM (
        SELECT COUNT(*) AS dup_count
        FROM notification_context_v2
        GROUP BY fhir_server_base_url, patient_id,
                 notification_resource_id, notification_resource_type, trigger_event
        HAVING COUNT(*) > 1
    ) AS duplicates;

    IF duplicate_count > 0 THEN
        RAISE NOTICE 'Found % duplicate groups in notification_context_v2. Attempting to fix...', duplicate_count;

        -- Enable uuid-ossp extension
        EXECUTE 'CREATE EXTENSION IF NOT EXISTS "uuid-ossp"';

        -- Update duplicates with unique trigger_event values
        UPDATE notification_context_v2
        SET trigger_event = trigger_event || '_old_' || uuid_generate_v4()::TEXT
        WHERE id IN (
            SELECT id FROM (
                SELECT id,
                       ROW_NUMBER() OVER (
                           PARTITION BY fhir_server_base_url, patient_id,
                                        notification_resource_id, notification_resource_type, trigger_event
                           ORDER BY id
                       ) AS row_num
                FROM notification_context_v2
            ) AS dups
            WHERE row_num > 1
        );

        RAISE NOTICE 'Duplicate trigger_event values fixed successfully.';
    ELSE
        RAISE NOTICE 'No duplicate records found in notification_context_v2.';
    END IF;

    ------------------------------------------------------------------
    -- 2. Add Unique Constraint
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.table_constraints
        WHERE table_schema = 'public'
          AND table_name = 'notification_context_v2'
          AND constraint_name = 'uc_notification_context_v2'
    ) THEN
        ALTER TABLE public.notification_context_v2
        ADD CONSTRAINT uc_notification_context_v2 UNIQUE (
            fhir_server_base_url,
            patient_id,
            notification_resource_id,
            notification_resource_type,
            trigger_event
        );
        RAISE NOTICE 'Unique constraint uc_notification_context_v2 added successfully.';
    ELSE
        RAISE NOTICE 'Unique constraint uc_notification_context_v2 already exists.';
    END IF;

    ------------------------------------------------------------------
    -- 3. Completion Message
    ------------------------------------------------------------------
    RAISE NOTICE 'Schema update completed for notification_context_v2.';

EXCEPTION
    WHEN OTHERS THEN
        RAISE WARNING 'Error while applying unique constraint: %', SQLERRM;
        RAISE;
END;
$$;
