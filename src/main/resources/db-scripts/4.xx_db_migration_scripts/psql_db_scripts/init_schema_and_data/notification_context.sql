DO
$$
DECLARE
    old_count BIGINT;
    new_count BIGINT;
    data_transfer_success BOOLEAN := FALSE;
BEGIN
    ------------------------------------------------------------------
    -- 1. Create notification_context_v2 table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_name = 'notification_context_v2'
    ) THEN
        CREATE TABLE notification_context_v2 (
            id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
            trigger_event VARCHAR(8000) NOT NULL,
            fhir_server_base_url VARCHAR(8000) NOT NULL,
            patient_id VARCHAR(8000) NOT NULL,
            notification_resource_id VARCHAR(8000) NOT NULL,
            notification_resource_type VARCHAR(8000) NOT NULL,
            relaunch_notification_data TEXT NULL,
            notification_processing_status VARCHAR(8000) NULL,
            notification_data TEXT NOT NULL,
            correlation_id VARCHAR(8000) NULL,
            x_request_id VARCHAR(8000) NULL,
            throttle_context VARCHAR(8000) NULL,
            ehr_access_token TEXT NULL,
            ehr_access_token_expiry_duration INT NULL,
            token_expiry_date TIMESTAMP NULL,
            encounter_start_time TIMESTAMP NULL,
            encounter_end_time TIMESTAMP NULL,
            encounter_class VARCHAR(255) NULL,
            ehr_launch_context VARCHAR(255) NULL,
            last_updated_ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
        );
        RAISE NOTICE '✅ notification_context_v2 table created.';
    ELSE
        RAISE NOTICE 'ℹ notification_context_v2 table already exists.';
    END IF;

    ------------------------------------------------------------------
    -- 2. Create migration_log table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_name = 'migration_log'
    ) THEN
        CREATE TABLE migration_log (
            table_name VARCHAR(100),
            data_transfer BOOLEAN
        );
        RAISE NOTICE '✅ migration_log table created.';
    ELSE
        RAISE NOTICE 'ℹ migration_log table already exists.';
    END IF;

    ------------------------------------------------------------------
    -- 3. Migrate data from old to new table
    ------------------------------------------------------------------
    IF EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_name = 'notification_context'
    ) THEN
        SELECT COUNT(*) INTO old_count FROM notification_context;

        IF old_count > 0 THEN
            BEGIN
                INSERT INTO notification_context_v2 (
                    id, trigger_event, fhir_server_base_url, patient_id,
                    notification_resource_id, notification_resource_type,
                    relaunch_notification_data, notification_processing_status,
                    notification_data, correlation_id, x_request_id, throttle_context,
                    ehr_access_token, ehr_access_token_expiry_duration,
                    token_expiry_date, encounter_start_time, encounter_end_time,
                    encounter_class, ehr_launch_context, last_updated_ts
                )
                SELECT
                    id, trigger_event, fhir_server_base_url, patient_id,
                    notification_resource_id, notification_resource_type,
                    relaunch_notification_data, notification_processing_status,
                    notification_data, correlation_id, x_request_id, throttle_context,
                    ehr_access_token, ehr_access_token_expiry_duration,
                    token_expiry_date, encounter_start_time, encounter_end_time,
                    encounter_class, ehr_launch_context, last_updated_ts
                FROM notification_context
                WHERE NOT EXISTS (
                    SELECT 1 FROM notification_context_v2 v
                    WHERE v.fhir_server_base_url = notification_context.fhir_server_base_url
                      AND v.patient_id = notification_context.patient_id
                      AND v.notification_resource_id = notification_context.notification_resource_id
                      AND v.notification_resource_type = notification_context.notification_resource_type
                      AND v.trigger_event = notification_context.trigger_event
                );

                SELECT COUNT(*) INTO new_count FROM notification_context_v2;

                IF new_count >= old_count THEN
                    data_transfer_success := TRUE;
                    RAISE NOTICE '✅ Migration successful for notification_context.';
                ELSE
                    data_transfer_success := FALSE;
                    RAISE NOTICE '⚠ Row count mismatch! Migration may be incomplete.';
                END IF;

            EXCEPTION WHEN OTHERS THEN
                data_transfer_success := FALSE;
                RAISE NOTICE ' Error during migration: %', SQLERRM;
            END;
        ELSE
            RAISE NOTICE 'ℹ No data to migrate from notification_context.';
        END IF;
    ELSE
        RAISE NOTICE '⚠ Source table notification_context does not exist.';
    END IF;

    ------------------------------------------------------------------
    -- 4. Log result
    ------------------------------------------------------------------
    INSERT INTO migration_log(table_name, data_transfer)
    VALUES ('notification_context', data_transfer_success);
END
$$;
