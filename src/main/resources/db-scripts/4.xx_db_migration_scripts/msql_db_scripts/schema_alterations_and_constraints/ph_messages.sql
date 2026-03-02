BEGIN TRY
    BEGIN TRANSACTION;

    PRINT '-----------------------------------------------------------';
    PRINT 'Starting index validation for dbo.ph_messages_v2...';

    ------------------------------------------------------------------
    -- 1. Index on notified_resource_id
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM sys.indexes
        WHERE name = 'idx_ph_messages_v2_not_res_id'
          AND object_id = OBJECT_ID('dbo.ph_messages_v2')
    )
    BEGIN
        CREATE INDEX idx_ph_messages_v2_not_res_id
        ON dbo.ph_messages_v2 (notified_resource_id);

        PRINT 'Index idx_ph_messages_v2_not_res_id created successfully.';
    END
    ELSE
        PRINT 'Index idx_ph_messages_v2_not_res_id already exists. Skipping.';


    ------------------------------------------------------------------
    -- 2. Index on kar_unique_id
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM sys.indexes
        WHERE name = 'idx_ph_messages_v2_kar_unique_id'
          AND object_id = OBJECT_ID('dbo.ph_messages_v2')
    )
    BEGIN
        CREATE INDEX idx_ph_messages_v2_kar_unique_id
        ON dbo.ph_messages_v2 (kar_unique_id);

        PRINT 'Index idx_ph_messages_v2_kar_unique_id created successfully.';
    END
    ELSE
        PRINT 'Index idx_ph_messages_v2_kar_unique_id already exists. Skipping.';


    ------------------------------------------------------------------
    -- 3. Completion
    ------------------------------------------------------------------
    PRINT 'Index update completed for dbo.ph_messages_v2.';
    COMMIT TRANSACTION;

END TRY
BEGIN CATCH
    PRINT 'ERROR applying indexes on dbo.ph_messages_v2: ' + ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    THROW;
END CATCH;