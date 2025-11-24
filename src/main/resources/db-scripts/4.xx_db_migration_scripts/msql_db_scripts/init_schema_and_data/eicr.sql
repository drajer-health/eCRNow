-- ==================================================================
-- eicr.sql
-- Purpose: Create eicr_v2 and migrate data from eicr
-- Note: Constraints handled separately
-- ==================================================================


BEGIN TRY
    BEGIN TRANSACTION;

    DECLARE
        @old_table NVARCHAR(128) = 'eicr',
        @new_table NVARCHAR(128) = 'eicr_v2',
        @row_count_old BIGINT,
        @row_count_new BIGINT,
        @missing_cols INT,
        @col_list NVARCHAR(MAX) = '',
        @select_list NVARCHAR(MAX) = '',
        @sql NVARCHAR(MAX);

    PRINT '-----------------------------------------------------------';
    PRINT '1️ Checking if old table exists...';

    ------------------------------------------------------------------
    -- 1. Ensure old table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM INFORMATION_SCHEMA.TABLES
        WHERE TABLE_SCHEMA = 'dbo'
          AND TABLE_NAME = @old_table
    )
    BEGIN
        THROW 50001, ' Old table does not exist. Migration aborted.', 1;
    END;

    ------------------------------------------------------------------
    -- 2. Create new table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM INFORMATION_SCHEMA.TABLES
        WHERE TABLE_SCHEMA = 'dbo'
          AND TABLE_NAME = @new_table
    )
    BEGIN
        PRINT ' Creating new table...';

        SET @sql = N'
        CREATE TABLE dbo.' + QUOTENAME(@new_table) + N' (
            id INT IDENTITY(1,1) PRIMARY KEY,
            x_req_id VARCHAR(8000) NULL,
            x_correlation_id VARCHAR(8000) NULL,
            eicr_doc_id VARCHAR(8000) NULL,
            set_id VARCHAR(8000) NULL,
            doc_version INT NULL,
            eicr_data VARCHAR(MAX) NULL,
            initiating_action VARCHAR(MAX) NULL,
            response_type VARCHAR(8000) NULL,
            response_type_display VARCHAR(8000) NULL,
            response_x_request_id VARCHAR(8000) NULL,
            response_doc_id VARCHAR(8000) NULL,
            rr_data VARCHAR(MAX) NULL,
            fhir_server_url VARCHAR(8000) NULL,
            launch_patient_id VARCHAR(8000) NULL,
            launch_details_id INT NULL,
            encounter_id VARCHAR(8000) NULL,
            provider_uuid VARCHAR(8000) NULL,
            ehr_doc_ref_id VARCHAR(8000) NULL,
            eicr_proc_status VARCHAR(8000) NULL,
            rr_proc_status VARCHAR(8000) NULL,
            last_updated_ts DATETIME NOT NULL DEFAULT GETDATE()
        );

        CREATE INDEX idx_eicr_doc_id ON dbo.' + QUOTENAME(@new_table) + N' (eicr_doc_id);
        ';

        EXEC sp_executesql @sql;

        PRINT ' New table created successfully.';
    END
    ELSE
    BEGIN
        PRINT 'ℹ Table already exists. Proceeding with validation...';
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
    PRINT ' Counting rows in old table...';

    SET @sql = N'SELECT @cnt_out = COUNT(*) FROM dbo.' + QUOTENAME(@old_table) +
               N' WHERE eicr_doc_id IS NOT NULL;';
    EXEC sp_executesql @sql, N'@cnt_out BIGINT OUTPUT', @cnt_out = @row_count_old OUTPUT;

    IF @row_count_old = 0
    BEGIN
        PRINT 'ℹ No rows to migrate. Exiting.';
        COMMIT TRANSACTION;
        RETURN;
    END;

    PRINT CONCAT('➡ Found ', @row_count_old, ' rows to migrate.');

    ------------------------------------------------------------------
    -- 5. Build dynamic column list
    ------------------------------------------------------------------
    PRINT ' Building column list dynamically...';

    SELECT @col_list = STRING_AGG(QUOTENAME(c.COLUMN_NAME), ', ')
    FROM INFORMATION_SCHEMA.COLUMNS c
    WHERE c.TABLE_SCHEMA = 'dbo'
      AND c.TABLE_NAME = @old_table
      AND c.COLUMN_NAME IN (
          SELECT COLUMN_NAME
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE TABLE_SCHEMA = 'dbo'
            AND TABLE_NAME = @new_table
      );

    SET @select_list = @col_list;

    PRINT ' Column mapping prepared.';

    ------------------------------------------------------------------
    -- 6. Copy data dynamically
    ------------------------------------------------------------------
    PRINT ' Migrating data...';

    SET @sql = N'
    SET IDENTITY_INSERT dbo.' + QUOTENAME(@new_table) + N' ON;

    INSERT INTO dbo.' + QUOTENAME(@new_table) + N' (' + @col_list + N')
    SELECT ' + @select_list + N'
    FROM dbo.' + QUOTENAME(@old_table) + N';

    SET IDENTITY_INSERT dbo.' + QUOTENAME(@new_table) + N' OFF;
    ';
    EXEC sp_executesql @sql;

    PRINT ' Data migration completed.';

    ------------------------------------------------------------------
    -- 7. Validate migrated row count
    ------------------------------------------------------------------
    PRINT ' Validating migrated row count...';

    SET @sql = N'SELECT @cnt_out = COUNT(*) FROM dbo.' + QUOTENAME(@new_table);
    EXEC sp_executesql @sql, N'@cnt_out BIGINT OUTPUT', @cnt_out = @row_count_new OUTPUT;

    IF @row_count_new <> @row_count_old
        THROW 50003, ' Row count mismatch after migration.', 1;
    ELSE
        PRINT CONCAT(' Migration successful: ', @row_count_new, ' rows copied successfully.');

    ------------------------------------------------------------------
    -- 8. Commit transaction
    ------------------------------------------------------------------
    COMMIT TRANSACTION;
    PRINT ' Transaction committed successfully. Migration complete.';

END TRY
BEGIN CATCH
    PRINT ' ERROR: ' + ERROR_MESSAGE();
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;
    THROW;
END CATCH;
