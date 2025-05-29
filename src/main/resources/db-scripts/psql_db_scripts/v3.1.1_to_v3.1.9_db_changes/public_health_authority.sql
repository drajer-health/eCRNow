DO $$
BEGIN
  -- Add backend_auth_key_alias column if it doesn't exist
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'public_health_authority' AND column_name = 'backend_auth_key_alias'
  ) THEN
    ALTER TABLE public_health_authority ADD COLUMN backend_auth_key_alias TEXT;
  END IF;

  -- Add backend_auth_alg column if it doesn't exist
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'public_health_authority' AND column_name = 'backend_auth_alg'
  ) THEN
    ALTER TABLE public_health_authority ADD COLUMN backend_auth_alg TEXT;
  END IF;

  -- Add backend_auth_kid column if it doesn't exist
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'public_health_authority' AND column_name = 'backend_auth_kid'
  ) THEN
    ALTER TABLE public_health_authority ADD COLUMN backend_auth_kid TEXT;
  END IF;
END
$$;
