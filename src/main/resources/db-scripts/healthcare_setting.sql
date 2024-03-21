ALTER TABLE healthcare_setting
  ADD COLUMN smtp_tls_version VARCHAR(256),
   ADD debug_enabled BIT DEFAULT CAST(1 AS BIT);

ALTER TABLE healthcare_setting
  ALTER COLUMN is_xdr SET DEFAULT 0;
