DO $$
BEGIN
  -- Add debug_enabled column if it does not exist
  IF NOT EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_name = 'healthcare_setting'
      AND column_name = 'debug_enabled'
  ) THEN
    ALTER TABLE healthcare_setting
    ADD COLUMN debug_enabled INTEGER DEFAULT 1;
  END IF;

  -- Update existing null debug_enabled values to 1
  UPDATE healthcare_setting
  SET debug_enabled = 1
  WHERE debug_enabled IS NULL;

  -- Set default value for is_xdr if column exists
  IF EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_name = 'healthcare_setting'
      AND column_name = 'is_xdr'
  ) THEN
    ALTER TABLE healthcare_setting
    ALTER COLUMN is_xdr SET DEFAULT 0;

    -- Update existing null is_xdr values to 0
    UPDATE healthcare_setting
    SET is_xdr = 0
    WHERE is_xdr IS NULL;
  END IF;
END
$$;
