DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_indexes WHERE schemaname = 'public' AND indexname = 'idx_not_res_id'
  ) THEN
    EXECUTE 'CREATE INDEX  idx_not_res_id ON ph_messages (notified_resource_id)';
  END IF;

  IF NOT EXISTS (
    SELECT 1 FROM pg_indexes WHERE schemaname = 'public' AND indexname = 'idx_kar_id'
  ) THEN
    EXECUTE 'CREATE INDEX  idx_kar_id ON ph_messages (kar_unique_id)';
  END IF;
END
$$;