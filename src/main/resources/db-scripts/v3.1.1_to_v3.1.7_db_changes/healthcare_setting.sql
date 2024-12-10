ALTER TABLE healthcare_setting
ADD debug_enabled INTEGER DEFAULT 1;

ALTER TABLE healthcare_setting
  ALTER COLUMN is_xdr SET DEFAULT 0;

ALTER TABLE healthcare_setting
  ADD COLUMN smtp_tls_version VARCHAR(256) DEFAULT 'TLSv1.2',


ALTER TABLE healthcare_setting
ADD COLUMN backend_auth_alg TEXT NULL;

ALTER TABLE healthcare_setting
ADD COLUMN backend_auth_kid TEXT NULL;
