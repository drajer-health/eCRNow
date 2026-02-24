DO
$$
BEGIN
    ------------------------------------------------------------------
    -- 1. Add index on notified_resource_id (if not exists)
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM pg_indexes
        WHERE schemaname = 'public'
          AND tablename = 'ph_messages_v2'
          AND indexname = 'idx_not_res_id_v2'
    ) THEN

        CREATE INDEX idx_not_res_id_v2
        ON public.ph_messages_v2 (notified_resource_id);

        RAISE NOTICE 'Index idx_not_res_id_v2 created successfully.';
    ELSE
        RAISE NOTICE 'Index idx_not_res_id_v2 already exists. Skipping.';
    END IF;


    ------------------------------------------------------------------
    -- 2. Add index on kar_unique_id (if not exists)
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM pg_indexes
        WHERE schemaname = 'public'
          AND tablename = 'ph_messages_v2'
          AND indexname = 'idx_kar_id_v2'
    ) THEN

        CREATE INDEX idx_kar_id_v2
        ON public.ph_messages_v2 (kar_unique_id);

        RAISE NOTICE 'Index idx_kar_id_v2 created successfully.';
    ELSE
        RAISE NOTICE 'Index idx_kar_id_v2 already exists. Skipping.';
    END IF;


    ------------------------------------------------------------------
    -- 3. Completion Message
    ------------------------------------------------------------------
    RAISE NOTICE 'Index update completed for ph_messages_v2.';

EXCEPTION
    WHEN OTHERS THEN
        RAISE WARNING 'Error applying indexes on ph_messages_v2: %', SQLERRM;
        RAISE;
END;
$$;