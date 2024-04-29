ALTER TABLE notification_context
  ADD COLUMN encounter_class VARCHAR(255),
  ADD COLUMN relaunch_notification_data TEXT;

ALTER TABLE notification_context
  ADD CONSTRAINT unique_notification_context UNIQUE (fhir_server_base_url, patient_id, notification_resource_id, notification_resource_type);
