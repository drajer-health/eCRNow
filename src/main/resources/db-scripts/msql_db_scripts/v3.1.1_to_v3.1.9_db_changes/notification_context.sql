-- Add 'encounter_class' column
ALTER TABLE notification_context
ADD encounter_class NVARCHAR(255);

ALTER TABLE notification_context
ADD relaunch_notification_data text;

// Avoid the having duplicates
ALTER TABLE notification_context
ADD CONSTRAINT unique_notification_context UNIQUE (fhir_server_base_url, patient_id, notification_resource_id, notification_resource_type, trigger_event);
