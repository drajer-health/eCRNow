ALTER TABLE healthcare_setting
ADD debug_enabled INTEGER DEFAULT 1;

ALTER TABLE healthcare_setting
  ALTER COLUMN is_xdr SET DEFAULT 0;
