DO $$
DECLARE
  duplicate_count INTEGER;
BEGIN
  -- ================================
  -- Step 1: Add columns if not exist
  -- ================================

  -- Add encounter_class column
  IF NOT EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_name = 'notification_context'
      AND column_name = 'encounter_class'
  ) THEN
    ALTER TABLE notification_context
    ADD COLUMN encounter_class VARCHAR(255);
  END IF;

  -- Add relaunch_notification_data column
  IF NOT EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_name = 'notification_context'
      AND column_name = 'relaunch_notification_data'
  ) THEN
    ALTER TABLE notification_context
    ADD COLUMN relaunch_notification_data VARCHAR(255);
  END IF;

  -- Add ehr_launch_context column
  IF NOT EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_name = 'notification_context'
      AND column_name = 'ehr_launch_context'
  ) THEN
    ALTER TABLE notification_context
    ADD COLUMN ehr_launch_context VARCHAR(255);
  END IF;

  -- ================================================
  -- Step 2: Check for and resolve duplicate rows
  -- ================================================

  SELECT COUNT(*) INTO duplicate_count
  FROM (
    SELECT COUNT(*) AS dup_count
    FROM notification_context
    GROUP BY fhir_server_base_url, patient_id, notification_resource_id, notification_resource_type, trigger_event
    HAVING COUNT(*) > 1
  ) AS duplicates;

  IF duplicate_count > 0 THEN
    -- Enable uuid-ossp extension
    EXECUTE 'CREATE EXTENSION IF NOT EXISTS "uuid-ossp"';

    -- Fix duplicate trigger_event values
    UPDATE notification_context
    SET trigger_event = trigger_event || '_old_' || uuid_generate_v4()::TEXT
    WHERE id IN (
      SELECT id FROM (
        SELECT id,
               ROW_NUMBER() OVER (
                 PARTITION BY fhir_server_base_url, patient_id,
                              notification_resource_id, notification_resource_type, trigger_event
                 ORDER BY id
               ) AS row_num
        FROM notification_context
      ) AS dups
      WHERE row_num > 1
    );
  END IF;

  -- ================================================
  -- Step 3: Add unique constraint if not exists
  -- ================================================

  IF NOT EXISTS (
    SELECT 1
    FROM pg_constraint
    WHERE conname = 'unique_notification_context'
      AND conrelid = 'notification_context'::regclass
  ) THEN
    EXECUTE '
      ALTER TABLE notification_context
      ADD CONSTRAINT unique_notification_context
      UNIQUE (
        fhir_server_base_url,
        patient_id,
        notification_resource_id,
        notification_resource_type,
        trigger_event
      )
    ';
  END IF;
END
$$;