ALTER TABLE healthcare_setting
ADD debug_enabled INT DEFAULT 1;

ALTER TABLE healthcare_setting
ADD CONSTRAINT DF_healthcare_setting_is_xdr DEFAULT 0 FOR is_xdr;

//if run below update query if data already exist
UPDATE healthcare_setting SET debug_enabled = 1 WHERE debug_enabled IS NULL;
UPDATE healthcare_setting SET is_xdr = 0 WHERE is_xdr IS NULL;


