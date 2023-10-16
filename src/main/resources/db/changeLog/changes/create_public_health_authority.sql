CREATE TABLE IF NOT EXISTS public_health_authority
(
    id INT PRIMARY KEY,
    auth_type varchar(255) NOT NULL,
    clientId varchar(255) NOT NULL,
    clientSecret varchar(255),
    fhir_server_base_url varchar(255) NOT NULL,
    fhir_version varchar(255),
    last_updated_ts timestamp NOT NULL,
    password varchar(255),
    require_aud bit NOT NULL,
    scopes varchar(255) NOT NULL,
    token_url varchar(255),
    username varchar(255),
    UNIQUE (fhir_server_base_url)
);