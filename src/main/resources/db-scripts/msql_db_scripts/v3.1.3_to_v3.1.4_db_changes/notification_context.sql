ALTER TABLE notification_context
  ADD CONSTRAINT unique_notification_context UNIQUE (fhir_server_base_url, patient_id, notification_resource_id, notification_resource_type, trigger_event);

ALTER TABLE notification_context
ADD ehr_launch_context VARCHAR(255) NULL;