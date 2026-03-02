-----------------------------------------------------------------------
-- Script: notification_context.sql (SQL Server version)
-- Purpose: Fix duplicate trigger_event values and add unique constraint
-- Author: Bhargav
-- Date: 2025-11-13
-----------------------------------------------------------------------

BEGIN TRY

    --------------------------------------------------------------------
    -- 1. Fix duplicate trigger_event values if duplicates exist
    --------------------------------------------------------------------
    IF EXISTS (
        SELECT 1
        FROM (
            SELECT COUNT(*) AS dup_count
            FROM notification_context_v2
            GROUP BY fhir_server_base_url, patient_id,
                     notification_resource_id, notification_resource_type, trigger_event
            HAVING COUNT(*) > 1
        ) AS duplicates
    )
    BEGIN
        PRINT 'Duplicate trigger_event groups found. Fixing...';

        ;WITH dups AS (
            SELECT
                id,
                ROW_NUMBER() OVER (
                    PARTITION BY fhir_server_base_url, patient_id,
                                 notification_resource_id, notification_resource_type, trigger_event
                    ORDER BY id
                ) AS rn
            FROM notification_context_v2
        )
        UPDATE nc
        SET nc.trigger_event = nc.trigger_event + '_old_' + CAST(NEWID() AS VARCHAR(50))
        FROM notification_context_v2 nc
        INNER JOIN dups d ON nc.id = d.id
        WHERE d.rn > 1;

        PRINT 'Duplicate records fixed.';
    END
    ELSE
    BEGIN
        PRINT 'No duplicate records found.';
    END


    --------------------------------------------------------------------
    -- 2. Add Unique Constraint
    --------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM sys.key_constraints kc
        WHERE kc.[name] = 'uc_notification_context_v2'
          AND kc.[type] = 'UQ'
    )
    BEGIN
        ALTER TABLE notification_context_v2
        ADD CONSTRAINT uc_notification_context_v2 UNIQUE (
            fhir_server_base_url,
            patient_id,
            notification_resource_id,
            notification_resource_type,
            trigger_event
        );

        PRINT 'Unique constraint uc_notification_context_v2 added.';
    END
    ELSE
    BEGIN
        PRINT 'Unique constraint uc_notification_context_v2 already exists.';
    END


    --------------------------------------------------------------------
    -- 3. Completion message
    --------------------------------------------------------------------
    PRINT 'Schema update completed successfully.';

END TRY
BEGIN CATCH
    PRINT 'Error during schema update: ' + ERROR_MESSAGE();
    THROW; -- rethrow the error for debugging or SQL Agent logs
END CATCH;
