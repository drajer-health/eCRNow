DO $$
BEGIN
  -- Add debug_enabled column if it doesn't exist
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'debug_enabled'
  ) THEN
    ALTER TABLE healthcare_setting ADD COLUMN debug_enabled INTEGER DEFAULT 1;
  END IF;

  -- Set default for is_xdr if the column exists
  IF EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'is_xdr'
  ) THEN
    BEGIN
      ALTER TABLE healthcare_setting ALTER COLUMN is_xdr SET DEFAULT 0;
    EXCEPTION WHEN OTHERS THEN
      RAISE NOTICE 'Could not alter default for is_xdr. It may already be set.';
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

  -- Add direct_endpoint_cert_alias column if not exists
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'direct_endpoint_cert_alias'
  ) THEN
    ALTER TABLE healthcare_setting ADD COLUMN direct_endpoint_cert_alias TEXT;
  END IF;

  -- Add smtp_auth_enabled column if not exists
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'smtp_auth_enabled'
  ) THEN
    ALTER TABLE healthcare_setting ADD COLUMN smtp_auth_enabled INTEGER DEFAULT 0;
  END IF;

  -- Add smtp_ssl_enabled column if not exists
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'smtp_ssl_enabled'
  ) THEN
    ALTER TABLE healthcare_setting ADD COLUMN smtp_ssl_enabled INTEGER DEFAULT 0;
  END IF;

  -- Update nulls to default values
  UPDATE healthcare_setting SET is_xdr = 0 WHERE is_xdr IS NULL;
  UPDATE healthcare_setting SET debug_enabled = 1 WHERE debug_enabled IS NULL;
  UPDATE healthcare_setting SET smtp_auth_enabled = 0 WHERE smtp_auth_enabled IS NULL;
  UPDATE healthcare_setting SET smtp_ssl_enabled = 0 WHERE smtp_ssl_enabled IS NULL;
END
$$;
