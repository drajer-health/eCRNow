-- ==================================================================
-- eicr.sql (UPDATED to match client_details.sql structure)
-- Purpose: Always create eicr_v2.
-- If old table exists → migrate data.
-- If old table does NOT exist → skip migration.
-- ==================================================================

BEGIN TRY
    BEGIN TRANSACTION;

    DECLARE
        @old_table NVARCHAR(128) = 'eicr',
        @new_table NVARCHAR(128) = 'eicr_v2',
        @old_table_exists BIT = 0,
        @row_count_old BIGINT = 0,
        @row_count_new BIGINT = 0,
        @missing_cols INT = 0,
        @col_list NVARCHAR(MAX) = '',
        @select_list NVARCHAR(MAX) = '',
        @missing_col_names NVARCHAR(MAX),
        @sql NVARCHAR(MAX);

    PRINT '-----------------------------------------------------------';
    PRINT 'Checking if old table exists...';

    ------------------------------------------------------------------
    -- 1. Check old table existence
    ------------------------------------------------------------------
    IF EXISTS (
        SELECT 1
        FROM INFORMATION_SCHEMA.TABLES
        WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @old_table
    )
        SET @old_table_exists = 1;
    ELSE
        PRINT 'Old table does NOT exist. Will create new table only, skipping migration.';


    ------------------------------------------------------------------
    -- 2. Create new table ALWAYS (if not exists)
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
               last_updated_ts datetime2(6) NOT NULL DEFAULT GETDATE()

            );

        ';

        EXEC sp_executesql @sql;

        PRINT 'New table created successfully.';
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
    -- 4. Column compatibility check (new → old)
    ------------------------------------------------------------------
    SELECT @missing_cols = COUNT(*)
    FROM (
        SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = @new_table
        EXCEPT
        SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = @old_table
    ) AS diff;

    SELECT @missing_col_names = COALESCE(STRING_AGG(COLUMN_NAME, ', '), 'None')
    FROM (
        SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = @new_table
        EXCEPT
        SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = @old_table
    ) AS diff;

    IF @missing_cols > 0
    BEGIN
        PRINT 'Column mismatch: ' + @missing_col_names;
        THROW 50002, 'Column mismatch detected. Migration aborted.', 1;
    END

    PRINT 'Column validation successful.';


    ------------------------------------------------------------------
    -- 5. Count rows in old table
    ------------------------------------------------------------------
    SET @sql = N'SELECT @cnt = COUNT(*) FROM dbo.' + QUOTENAME(@old_table) +
               N' WHERE eicr_doc_id IS NOT NULL;';
    EXEC sp_executesql @sql, N'@cnt BIGINT OUTPUT', @cnt = @row_count_old OUTPUT;

    IF @row_count_old = 0
    BEGIN
        PRINT 'Old table has 0 rows. Nothing to migrate.';
        COMMIT TRANSACTION;
        RETURN;
    END;

    PRINT CONCAT('Found ', @row_count_old, ' rows for migration.');


    ------------------------------------------------------------------
    -- 6. Build dynamic column list
    ------------------------------------------------------------------
    SELECT @col_list = STRING_AGG(QUOTENAME(COLUMN_NAME), ', ')
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_NAME = @old_table
      AND COLUMN_NAME IN (
          SELECT COLUMN_NAME
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE TABLE_NAME = @new_table
      );

    SET @select_list = @col_list;


    ------------------------------------------------------------------
    -- 7. Copy data
    ------------------------------------------------------------------
    PRINT 'Migrating data...';

    SET @sql = '
        SET IDENTITY_INSERT dbo.' + QUOTENAME(@new_table) + ' ON;

        INSERT INTO dbo.' + QUOTENAME(@new_table) + ' (' + @col_list + ')
        SELECT ' + @select_list + ' FROM dbo.' + QUOTENAME(@old_table) + '
        WHERE eicr_doc_id IS NOT NULL;

        SET IDENTITY_INSERT dbo.' + QUOTENAME(@new_table) + ' OFF;
    ';

    EXEC sp_executesql @sql;


    ------------------------------------------------------------------
    -- 8. Validate row count
    ------------------------------------------------------------------
    SET @sql = N'SELECT @cnt = COUNT(*) FROM dbo.' + QUOTENAME(@new_table);
    EXEC sp_executesql @sql, N'@cnt BIGINT OUTPUT', @cnt = @row_count_new OUTPUT;

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
