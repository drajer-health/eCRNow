CREATE TABLE IF NOT EXISTS kar_execution_state
(
    id uuid NOT NULL,
    action_status varchar(255),
    hs_fhir_server_url varchar(255) NOT NULL,
    kar_unique_id varchar(255) NOT NULL,
    nc_id uuid NOT NULL,
    nc_fk UUID,
    PRIMARY KEY (id),
    CONSTRAINT nc_FK FOREIGN KEY (nc_fk) REFERENCES notification_context(id)
);