-- ==================================================================
-- kar_info.sql
-- Purpose: Create kar_info_v2 and migrate data from kar_info
-- Note: Constraints handled separately
-- ==================================================================

BEGIN TRY
    BEGIN TRANSACTION;

    DECLARE
        @old_table NVARCHAR(128) = 'kar_info',
        @new_table NVARCHAR(128) = 'kar_info_v2',
        @row_count_old BIGINT,
        @row_count_new BIGINT,
        @missing_cols INT,
        @missing_col_names NVARCHAR(MAX),
        @col_list NVARCHAR(MAX) = '',
        @select_list NVARCHAR(MAX) = '',
        @sql NVARCHAR(MAX);

    PRINT '-----------------------------------------------------------';
    PRINT ' Starting migration from kar_info → kar_info_v2';


    ------------------------------------------------------------------
    -- 1. Ensure old table exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM INFORMATION_SCHEMA.TABLES
        WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @old_table
    )
    BEGIN
        THROW 50001, ' Old table kar_info does not exist. Migration aborted.', 1;
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
        PRINT ' Creating new table dbo.kar_info_v2...';

        SET @sql = N'
            CREATE TABLE dbo.' + QUOTENAME(@new_table) + N' (
                id INT IDENTITY(1,1) PRIMARY KEY,
                kar_id VARCHAR(8000) NOT NULL,
                kar_name VARCHAR(8000) NOT NULL,
                kar_publisher VARCHAR(8000) NOT NULL,
                kar_version VARCHAR(8000) NOT NULL,
                kar_available INT DEFAULT 0 NULL
            );
        ';
        EXEC sp_executesql @sql;

        PRINT ' New table kar_info_v2 created successfully.';
    END
    ELSE
    BEGIN
        PRINT ' Table kar_info_v2 already exists. Proceeding with validation...';
    END;


    ------------------------------------------------------------------
    -- 3. Verify column compatibility (new table → old table)
    ------------------------------------------------------------------
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

    SELECT @missing_col_names =
        COALESCE(STRING_AGG(COLUMN_NAME, ', '), 'None')
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
        PRINT ' Column mismatch: The new table has columns missing in the old table.';
        PRINT ' Missing columns: ' + @missing_col_names;
        THROW 50002, 'Column mismatch detected. Migration aborted.', 1;
    END
    ELSE
        PRINT ' All columns in kar_info_v2 exist in the old table kar_info.';


    ------------------------------------------------------------------
    -- 4. Count rows in old table
    ------------------------------------------------------------------
    SET @sql = N'SELECT @cnt_out = COUNT(*) FROM dbo.' + QUOTENAME(@old_table)
              + N' WHERE kar_id IS NOT NULL;';

    EXEC sp_executesql @sql,
        N'@cnt_out BIGINT OUTPUT',
        @cnt_out = @row_count_old OUTPUT;

    IF @row_count_old = 0
    BEGIN
        PRINT ' No rows found in old table. Migration skipped.';
        COMMIT TRANSACTION;
        RETURN;
    END;

    PRINT CONCAT(' Found ', @row_count_old, ' rows to migrate. Proceeding...');


    ------------------------------------------------------------------
    -- 5. Build dynamic shared column list
    ------------------------------------------------------------------
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


    ------------------------------------------------------------------
    -- 6. Copy data
    ------------------------------------------------------------------
    PRINT ' Migrating data...';

    SET @sql = N'
        SET IDENTITY_INSERT dbo.' + QUOTENAME(@new_table) + N' ON;

        INSERT INTO dbo.' + QUOTENAME(@new_table) + N' (' + @col_list + N')
        SELECT ' + @select_list + N'
        FROM dbo.' + QUOTENAME(@old_table) + N'
        WHERE kar_id IS NOT NULL;

        SET IDENTITY_INSERT dbo.' + QUOTENAME(@new_table) + N' OFF;
    ';

    EXEC sp_executesql @sql;


    ------------------------------------------------------------------
    -- 7. Validate migration result
    ------------------------------------------------------------------
    SET @sql = N'SELECT @cnt_out = COUNT(*) FROM dbo.' + QUOTENAME(@new_table) + N';';

    EXEC sp_executesql @sql,
        N'@cnt_out BIGINT OUTPUT',
        @cnt_out = @row_count_new OUTPUT;

    IF @row_count_new >= @row_count_old
        PRINT CONCAT(' Migration successful: ', @row_count_new, ' rows now in kar_info_v2.');
    ELSE
        THROW 50003, ' Row count mismatch after migration.', 1;


    COMMIT TRANSACTION;
END TRY
BEGIN CATCH
    PRINT ' ERROR: ' + ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    THROW;
END CATCH;
