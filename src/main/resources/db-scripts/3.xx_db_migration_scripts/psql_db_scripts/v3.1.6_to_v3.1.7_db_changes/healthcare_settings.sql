DO
$$
BEGIN
  -- Add backend_auth_kid column if not exists
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting'
      AND column_name = 'backend_auth_kid'
  ) THEN
    ALTER TABLE healthcare_setting ADD COLUMN backend_auth_kid TEXT;
  END IF;
END
$$;

Do
$$
BEGIN
IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'healthcare_setting' AND column_name = 'backend_auth_alg'
  ) THEN
    ALTER TABLE healthcare_setting ADD COLUMN backend_auth_alg TEXT;
  END IF;
  END $$