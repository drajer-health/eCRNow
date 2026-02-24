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
        @old_table_exists BIT = 0,
        @row_count_old BIGINT = 0,
        @row_count_new BIGINT = 0,
        @missing_cols INT = 0,
        @missing_col_names NVARCHAR(MAX),
        @col_list NVARCHAR(MAX) = '',
        @select_list NVARCHAR(MAX) = '',
        @sql NVARCHAR(MAX);

    PRINT '-----------------------------------------------------------';
    PRINT 'Checking if old table exists...';

    ------------------------------------------------------------------
    -- 1. Check old table existence
    ------------------------------------------------------------------
    IF EXISTS (
        SELECT 1 FROM INFORMATION_SCHEMA.TABLES
        WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @old_table
    )
        SET @old_table_exists = 1;
    ELSE
        PRINT 'Old table does NOT exist. Will create table only, skipping migration.';


    ------------------------------------------------------------------
    -- 2. Create new table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM INFORMATION_SCHEMA.TABLES
        WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @new_table
    )
    BEGIN
        PRINT 'Creating new table dbo.ph_messages_v2...';

        SET @sql = N'
            CREATE TABLE dbo.' + QUOTENAME(@new_table) + N' (
                id UNIQUEIDENTIFIER DEFAULT NEWID() PRIMARY KEY,
                fhir_server_base_url VARCHAR(8000) NOT NULL,
                patient_id VARCHAR(8000) NOT NULL,
                encounter_id VARCHAR(8000) NOT NULL,
                notified_resource_id VARCHAR(8000) NOT NULL,
                notified_resource_type VARCHAR(8000) NOT NULL,
                kar_unique_id VARCHAR(8000) NULL,
                notification_id VARCHAR(8000) NOT NULL,
                correlation_id VARCHAR(8000) NULL,
                x_request_id VARCHAR(8000) NULL,
                submitted_fhir_data VARCHAR(MAX) NULL,
                submitted_cda_data VARCHAR(MAX) NULL,
                submitted_message_type VARCHAR(8000) NULL,
                submitted_data_id VARCHAR(8000) NULL,
                submitted_version_number INT NULL,
                submitted_message_id VARCHAR(8000) NULL,
                submission_message_status VARCHAR(8000) NULL,
                submission_time DATETIME2(6) NULL,
                fhir_response_data VARCHAR(MAX) NULL,
                cda_response_data VARCHAR(MAX) NULL,
                failure_response_data VARCHAR(MAX) NULL,
                response_message_type VARCHAR(8000) NULL,
                response_data_id VARCHAR(8000) NULL,
                response_message_id VARCHAR(8000) NULL,
                response_processing_instruction VARCHAR(MAX) NULL,
                response_processing_status VARCHAR(MAX) NULL,
                response_received_time DATETIME2(6) NULL,
                ehr_doc_ref_id VARCHAR(8000) NULL,
                initiating_action VARCHAR(MAX) NULL,
                trigger_match_status VARCHAR(MAX) NULL,
                patient_linker_id VARCHAR(8000) NULL,
                last_updated_ts DATETIME2(6) NOT NULL DEFAULT GETDATE()
            );

            CREATE INDEX idx_not_res_id_v2 ON dbo.' + QUOTENAME(@new_table) + N' (notified_resource_id);
            CREATE INDEX idx_kar_id_v2 ON dbo.' + QUOTENAME(@new_table) + N' (kar_unique_id);
        ';
        EXEC sp_executesql @sql;

        PRINT 'New table ph_messages_v2 created successfully.';
    END
    ELSE
        PRINT 'New table already exists. Skipping creation.';


    ------------------------------------------------------------------
    -- 3. If old table does NOT exist → skip migration
    ------------------------------------------------------------------
    IF @old_table_exists = 0
    BEGIN
        PRINT 'Skipping migration because old table does not exist.';
        COMMIT TRANSACTION;
        RETURN;
    END;


    ------------------------------------------------------------------
    -- 4. Column compatibility check
    ------------------------------------------------------------------
    SELECT @missing_cols = COUNT(*)
    FROM (
        SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = @new_table

        EXCEPT

        SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = @old_table
    ) AS diff;

    SELECT @missing_col_names =
        COALESCE(STRING_AGG(COLUMN_NAME, ', '), 'None')
    FROM (
        SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = @new_table

        EXCEPT

        SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = @old_table
    ) AS diff;

    IF @missing_cols > 0
    BEGIN
        PRINT 'Column mismatch detected.';
        PRINT 'Missing in old table: ' + @missing_col_names;
        THROW 50002, 'Column mismatch detected. Migration aborted.', 1;
    END;

    PRINT 'Column validation successful.';


    ------------------------------------------------------------------
    -- 5. Count rows in old table
    ------------------------------------------------------------------
    SET @sql = N'SELECT @count = COUNT(*) FROM dbo.' + QUOTENAME(@old_table);
    EXEC sp_executesql @sql, N'@count BIGINT OUTPUT', @count = @row_count_old OUTPUT;

    IF @row_count_old = 0
    BEGIN
        PRINT 'Old table has zero rows. Nothing to migrate.';
        COMMIT TRANSACTION;
        RETURN;
    END;

    PRINT CONCAT('Found ', @row_count_old, ' rows for migration.');


    ------------------------------------------------------------------
    -- 6. Build dynamic shared column list
    ------------------------------------------------------------------
    SELECT @col_list = STRING_AGG(QUOTENAME(c.COLUMN_NAME), ', ')
    FROM INFORMATION_SCHEMA.COLUMNS c
    WHERE c.TABLE_NAME = @old_table
      AND c.COLUMN_NAME IN (
            SELECT COLUMN_NAME
            FROM INFORMATION_SCHEMA.COLUMNS
            WHERE TABLE_NAME = @new_table
      );

    SET @select_list = @col_list;


    ------------------------------------------------------------------
    -- 7. Copy data
    ------------------------------------------------------------------
    PRINT 'Migrating data...';

    SET @sql = N'
        INSERT INTO dbo.' + QUOTENAME(@new_table) + N'
        (' + @col_list + ')
        SELECT ' + @select_list + '
        FROM dbo.' + QUOTENAME(@old_table) + N';
    ';

    EXEC sp_executesql @sql;


    ------------------------------------------------------------------
    -- 8. Validate migration
    ------------------------------------------------------------------
    SET @sql = N'SELECT @count = COUNT(*) FROM dbo.' + QUOTENAME(@new_table);
    EXEC sp_executesql @sql, N'@count BIGINT OUTPUT', @count = @row_count_new OUTPUT;

    IF @row_count_new <> @row_count_old
        THROW 50003, 'Row count mismatch after migration.', 1;

    PRINT CONCAT('Migration successful: ', @row_count_new, ' rows copied.');


    COMMIT TRANSACTION;

END TRY
BEGIN CATCH
    PRINT 'ERROR: ' + ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    THROW;
END CATCH;
