DO $$
BEGIN
  -- Add 'encounter_class' column if it does not exist
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'notification_context'
      AND column_name = 'encounter_class'
  ) THEN
    ALTER TABLE notification_context
    ADD COLUMN encounter_class VARCHAR(255);
  END IF;

  -- Add 'relaunch_notification_data' column if it does not exist
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'notification_context'
      AND column_name = 'relaunch_notification_data'
  ) THEN
    ALTER TABLE notification_context
    ADD COLUMN relaunch_notification_data TEXT;
  END IF;

  -- Add unique constraint if it does not exist
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname = 'unique_notification_context'
      AND conrelid = 'notification_context'::regclass
  ) THEN
    ALTER TABLE notification_context
    ADD CONSTRAINT unique_notification_context
    UNIQUE (
      fhir_server_base_url,
      patient_id,
      notification_resource_id,
      notification_resource_type
    );
  END IF;
END
$$;
