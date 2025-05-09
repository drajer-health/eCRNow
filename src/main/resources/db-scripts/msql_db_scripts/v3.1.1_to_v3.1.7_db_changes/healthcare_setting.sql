ALTER TABLE healthcare_setting
ADD debug_enabled INT DEFAULT 1;


ALTER TABLE healthcare_setting
ADD CONSTRAINT DF_healthcare_setting_debug_enabled DEFAULT 1 FOR debug_enabled;

ALTER TABLE healthcare_setting
ADD CONSTRAINT DF_healthcare_setting_is_xdr DEFAULT 0 FOR is_xdr;


ALTER TABLE healthcare_setting
ADD smtp_tls_version NVARCHAR(256) DEFAULT 'TLSv1.2';

ALTER TABLE healthcare_setting
ADD backend_auth_alg text NULL;

ALTER TABLE healthcare_setting
ADD backend_auth_kid text NULL;


//if run below update query if data already exist
UPDATE healthcare_setting SET debug_enabled = 1 WHERE debug_enabled IS NULL;
UPDATE healthcare_setting SET is_xdr = 0 WHERE is_xdr IS NULL;
