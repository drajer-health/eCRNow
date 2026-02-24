DO $$
BEGIN
  -- Add debug_enabled column if not exists
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'debug_enabled'
  ) THEN
    ALTER TABLE healthcare_setting ADD COLUMN debug_enabled INTEGER DEFAULT 1;
  END IF;

  -- Set default for is_xdr if column exists
  IF EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'is_xdr'
  ) THEN
    BEGIN
      ALTER TABLE healthcare_setting ALTER COLUMN is_xdr SET DEFAULT 0;
    EXCEPTION WHEN others THEN
      RAISE NOTICE 'Could not set default for is_xdr. It may already have one.';
    END;
  END IF;

  -- Add smtp_tls_version column if not exists
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'smtp_tls_version'
  ) THEN
    ALTER TABLE healthcare_setting ADD COLUMN smtp_tls_version VARCHAR(256) DEFAULT 'TLSv1.2';
  END IF;

  -- Add backend_auth_alg column if not exists
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'backend_auth_alg'
  ) THEN
    ALTER TABLE healthcare_setting ADD COLUMN backend_auth_alg TEXT;
  END IF;

  -- Add backend_auth_kid column if not exists
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'backend_auth_kid'
  ) THEN
    ALTER TABLE healthcare_setting ADD COLUMN backend_auth_kid TEXT;
  END IF;

  -- Update existing null values
  UPDATE healthcare_setting SET debug_enabled = 1 WHERE debug_enabled IS NULL;
  UPDATE healthcare_setting SET is_xdr = 0 WHERE is_xdr IS NULL;
END
$$;
