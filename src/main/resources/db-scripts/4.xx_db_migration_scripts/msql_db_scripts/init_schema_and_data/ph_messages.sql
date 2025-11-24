-- ==================================================================
-- ph_messages.sql
-- Purpose: Create ph_messages_v2 and migrate data from ph_messages
-- Note: Constraints and FKs handled separately
-- ==================================================================



BEGIN TRY
    BEGIN TRANSACTION;

    DECLARE
        @old_table NVARCHAR(128) = 'ph_messages',
        @new_table NVARCHAR(128) = 'ph_messages_v2',
        @row_count_old BIGINT,
        @row_count_new BIGINT,
        @missing_cols INT,
        @col_list NVARCHAR(MAX) = '',
        @select_list NVARCHAR(MAX) = '',
        @sql NVARCHAR(MAX);

    PRINT '-----------------------------------------------------------';
    PRINT '1. Checking if old table exists...';

    ------------------------------------------------------------------
    -- 1. Ensure old table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM INFORMATION_SCHEMA.TABLES
        WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @old_table
    )
    BEGIN
        THROW 50001, 'Old table does not exist. Migration aborted.', 1;
    END;

    ------------------------------------------------------------------
    -- 2. Create new table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM INFORMATION_SCHEMA.TABLES
        WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @new_table
    )
    BEGIN
        PRINT 'Creating new table...';

        SET @sql = N'
        CREATE TABLE dbo.' + QUOTENAME(@new_table) + N' (
            id UNIQUEIDENTIFIER DEFAULT NEWID() PRIMARY KEY,
            fhir_server_base_url VARCHAR(8000) NOT NULL,
            patient_id VARCHAR(8000) NOT NULL,
            encounter_id VARCHAR(8000) NOT NULL,
            notified_resource_id VARCHAR(8000) NOT NULL,
            notified_resource_type VARCHAR(8000) NOT NULL,
            kar_unique_id VARCHAR(8000),
            notification_id VARCHAR(8000) NOT NULL,
            correlation_id VARCHAR(8000),
            x_request_id VARCHAR(8000),
            submitted_fhir_data VARCHAR(MAX),
            submitted_cda_data VARCHAR(MAX),
            submitted_message_type VARCHAR(8000),
            submitted_data_id VARCHAR(8000),
            submitted_version_number INT,
            submitted_message_id VARCHAR(8000),
            submission_message_status VARCHAR(8000),
            submission_time DATETIME,
            fhir_response_data VARCHAR(MAX),
            cda_response_data VARCHAR(MAX),
            failure_response_data VARCHAR(MAX),
            response_message_type VARCHAR(8000),
            response_data_id VARCHAR(8000),
            response_message_id VARCHAR(8000),
            response_processing_instruction VARCHAR(MAX),
            response_processing_status VARCHAR(MAX),
            response_received_time DATETIME,
            ehr_doc_ref_id VARCHAR(8000),
            initiating_action VARCHAR(MAX),
            trigger_match_status VARCHAR(MAX),
            patient_linker_id VARCHAR(8000),
            last_updated_ts DATETIME NOT NULL
        );

        CREATE INDEX idx_not_res_id_new ON dbo.' + QUOTENAME(@new_table) + N' (notified_resource_id);
        CREATE INDEX idx_kar_id_new ON dbo.' + QUOTENAME(@new_table) + N' (kar_unique_id);
        ';
        EXEC sp_executesql @sql;

        PRINT 'New table created successfully.';
    END
    ELSE
    BEGIN
        PRINT 'Table already exists. Proceeding with validation...';
    END;




------------------------------------------------------------------
-- 3. Verify column compatibility (new table → old table)
------------------------------------------------------------------
DECLARE @missing_col_names NVARCHAR(MAX);

-- Count missing columns in old table compared to new table
SELECT @missing_cols = COUNT(*)
FROM (
    SELECT COLUMN_NAME
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @new_table
    EXCEPT
    SELECT COLUMN_NAME
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @old_table
) AS diff;

-- Collect missing column names into a comma-separated list
SELECT @missing_col_names = COALESCE(STRING_AGG(COLUMN_NAME, ', '), 'None')
FROM (
    SELECT COLUMN_NAME
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @new_table
    EXCEPT
    SELECT COLUMN_NAME
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @old_table
) AS diff;

IF @missing_cols > 0
BEGIN
    PRINT 'Column mismatch: The new table has columns missing in the old table.';
    PRINT 'Missing columns: ' + @missing_col_names;
    THROW 50002, 'Column mismatch detected. Migration aborted.', 1;
END
ELSE
    PRINT 'All columns in the new table exist in the old table.';


    ------------------------------------------------------------------
    -- 4. Count rows in old table
    ------------------------------------------------------------------
    SET @sql = N'SELECT @cnt_out = COUNT(*) FROM dbo.' + QUOTENAME(@old_table);
    EXEC sp_executesql @sql, N'@cnt_out BIGINT OUTPUT', @cnt_out = @row_count_old OUTPUT;

    IF @row_count_old = 0
    BEGIN
        PRINT 'No rows to migrate.';
        COMMIT TRANSACTION;
        RETURN;
    END;

    PRINT CONCAT('Found ', @row_count_old, ' rows. Proceeding with migration...');

    ------------------------------------------------------------------
    -- 5. Build dynamic column list
    ------------------------------------------------------------------
    SELECT @col_list = STRING_AGG(QUOTENAME(c.COLUMN_NAME), ', ')
    FROM INFORMATION_SCHEMA.COLUMNS c
    WHERE c.TABLE_SCHEMA = 'dbo' AND c.TABLE_NAME = @old_table
    AND c.COLUMN_NAME IN (
        SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS
        WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @new_table
    );

    SET @select_list = @col_list;

    ------------------------------------------------------------------
    -- 6. Copy data dynamically
    ------------------------------------------------------------------
    PRINT 'Migrating data...';

    SET @sql = N'
    INSERT INTO dbo.' + QUOTENAME(@new_table) + N' (' + @col_list + N')
    SELECT ' + @select_list + N'
    FROM dbo.' + QUOTENAME(@old_table) + N';
    ';

    EXEC sp_executesql @sql;

    ------------------------------------------------------------------
    -- 7. Validate migrated row count
    ------------------------------------------------------------------
    SET @sql = N'SELECT @cnt_out = COUNT(*) FROM dbo.' + QUOTENAME(@new_table);
    EXEC sp_executesql @sql, N'@cnt_out BIGINT OUTPUT', @cnt_out = @row_count_new OUTPUT;

    IF @row_count_new <> @row_count_old
        THROW 50003, 'Row count mismatch after migration.', 1;
    ELSE
        PRINT CONCAT('Migration successful: ', @row_count_new, ' rows copied.');

    COMMIT TRANSACTION;
END TRY
BEGIN CATCH
    PRINT 'ERROR: ' + ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    THROW;
END CATCH;
