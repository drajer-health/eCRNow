-- ==================================================================
-- scheduled_tasks.sql
-- Purpose: Create scheduled_tasks_v2 and migrate data from scheduled_tasks
-- Note: Constraints handled separately
-- ==================================================================

BEGIN TRY
    BEGIN TRANSACTION;

    DECLARE
        @old_table NVARCHAR(128) = 'scheduled_tasks',
        @new_table NVARCHAR(128) = 'scheduled_tasks_v2',
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
        PRINT 'Creating new table dbo.scheduled_tasks_v2...';

        SET @sql = N'
            CREATE TABLE dbo.' + QUOTENAME(@new_table) + N' (
                task_instance VARCHAR(255) NOT NULL,
                task_name VARCHAR(255) NOT NULL,
                task_data VARBINARY(MAX) NULL,
                execution_time DATETIME2(6) NULL,
                picked INT DEFAULT 0,
                picked_by VARCHAR(255) NULL,
                last_success DATETIME2(6) NULL,
                last_failure DATETIME2(6) NULL,
                consecutive_failures INT NULL,
                last_heartbeat DATETIME2(6) NULL,
                version INT NOT NULL,
                CONSTRAINT pk_' + @new_table + N' PRIMARY KEY (task_instance, task_name)
            );

            CREATE INDEX idx_exec_time_new ON dbo.' + QUOTENAME(@new_table) + N' (execution_time);
        ';
        EXEC sp_executesql @sql;

        PRINT 'New table scheduled_tasks_v2 created successfully.';
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
        (' + @col_list + N')
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
