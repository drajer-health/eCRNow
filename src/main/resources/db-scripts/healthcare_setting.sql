ALTER TABLE healthcare_setting
  ADD COLUMN smtp_tls_version VARCHAR(256) DEFAULT 'TLSv1.2',
   ADD debug_enabled INTEGER DEFAULT 1;

ALTER TABLE healthcare_setting
  ALTER COLUMN is_xdr SET DEFAULT 0;
