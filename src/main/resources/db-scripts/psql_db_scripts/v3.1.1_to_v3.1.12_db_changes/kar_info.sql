DO $$
BEGIN
  -- Add column if it does not exist
  IF NOT EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_name = 'kar_info'
      AND column_name = 'kar_available'
  ) THEN
    ALTER TABLE kar_info
    ADD COLUMN kar_available INTEGER DEFAULT 0;
  END IF;

  -- Update existing NULL values (in case the column already existed without default)
  UPDATE kar_info
  SET kar_available = 0
  WHERE kar_available IS NULL;
END
$$;
