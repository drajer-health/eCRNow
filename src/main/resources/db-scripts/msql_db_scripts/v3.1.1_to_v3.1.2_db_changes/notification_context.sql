ALTER TABLE notification_context
ADD encounter_class NVARCHAR(255),
    relaunch_notification_data text;

ALTER TABLE notification_context
ADD CONSTRAINT unique_notification_context UNIQUE (fhir_server_base_url, patient_id, notification_resource_id, notification_resource_type);