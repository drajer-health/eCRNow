DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_indexes
    WHERE tablename = 'scheduled_tasks' AND indexname = 'idx_exec_time'
  ) THEN
    EXECUTE 'CREATE INDEX idx_exec_time ON scheduled_tasks (execution_time)';
  END IF;
END
$$;
