ALTER TABLE healthcare_setting
ADD debug_enabled INTEGER DEFAULT 1;

ALTER TABLE healthcare_setting
  ALTER COLUMN is_xdr SET DEFAULT 0;

ALTER TABLE healthcare_setting
  ADD COLUMN smtp_tls_version VARCHAR(256) DEFAULT 'TLSv1.2';


ALTER TABLE healthcare_setting
ADD COLUMN backend_auth_alg TEXT NULL;

ALTER TABLE healthcare_setting
ADD COLUMN backend_auth_kid TEXT NULL;


ALTER TABLE healthcare_setting
  ADD COLUMN direct_endpoint_cert_alias TEXT;

ALTER TABLE healthcare_setting
    ADD smtp_auth_enabled INTEGER DEFAULT 0;


ALTER TABLE healthcare_setting
    ADD smtp_ssl_enabled INTEGER DEFAULT 0;



  //if run below update query if data already exist
  UPDATE healthcare_setting SET is_xdr = 0 WHERE is_xdr IS NULL;
  UPDATE healthcare_setting SET debug_enabled = 1 WHERE debug_enabled IS NULL;
  UPDATE healthcare_setting SET smtp_auth_enabled = 0 WHERE smtp_auth_enabled IS NULL;
   UPDATE healthcare_setting SET smtp_ssl_enabled = 0 WHERE smtp_ssl_enabled IS NULL;