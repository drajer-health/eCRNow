CREATE TABLE IF NOT EXISTS notification_context
(
    id uuid NOT NULL,
    ehr_access_token varchar(255) not null,
    token_expiry_date timestamp,
    ehr_access_token_expiry_duration integer,
    encounter_class varchar(255),
    encounter_end_time timestamp,
    encounter_start_time timestamp,
    fhir_server_base_url varchar(255) not null,
    last_updated_ts timestamp NOT NULL,
    notification_data varchar(255) not null,
    notification_processing_status varchar(255),
    notification_resource_id varchar(255) not null,
    notification_resource_type varchar(255) not null,
    patient_id varchar(255) not null,
    throttle_context varchar(255),
    trigger_event varchar(255) NOT NULL,
    correlation_id varchar(255),
    x_request_id varchar(255),
    PRIMARY KEY (id),
    UNIQUE (fhir_server_base_url, patient_id, notification_resource_id, notification_resource_type)
);