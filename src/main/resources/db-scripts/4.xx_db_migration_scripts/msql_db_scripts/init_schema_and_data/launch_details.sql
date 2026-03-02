-- ==================================================================
-- launch_details.sql
-- Purpose: Create launch_details_v2 and migrate data from launch_details
-- Note: Constraints handled separately
-- ==================================================================

BEGIN TRY
    BEGIN TRANSACTION;

    DECLARE
        @old_table NVARCHAR(128) = 'launch_details',
        @new_table NVARCHAR(128) = 'launch_details_v2',
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
        PRINT 'Creating new table dbo.launch_details_v2...';

        SET @sql = N'
            CREATE TABLE dbo.' + QUOTENAME(@new_table) + N' (
                id INT IDENTITY(1,1) PRIMARY KEY,
                client_id VARCHAR(255) NULL,
                client_secret VARCHAR(8000) NULL,
                ehr_server_url VARCHAR(255) NULL,
                auth_url VARCHAR(8000) NULL,
                token_url VARCHAR(8000) NULL,
                access_token VARCHAR(MAX) NULL,
                user_id VARCHAR(255) NULL,
                expiry INT DEFAULT 0,
                scope VARCHAR(MAX) NULL,
                last_updated_ts DATETIME2(6) NOT NULL DEFAULT GETDATE(),
               start_date DATETIME2(6)  NULL,
                end_date DATETIME2(6)  NULL,
                token_expiry_date DATETIME2(6)  NULL,
                refresh_token VARCHAR(MAX) NULL,
                launch_patient_id VARCHAR(255) NULL,
                fhir_version VARCHAR(255) NULL,
                encounter_id VARCHAR(255) NULL,
                provider_uuid VARCHAR(255) NULL,
                status VARCHAR(8000) NULL,
                aa_id VARCHAR(255) NULL,
                set_id VARCHAR(255) NULL,
                ver_number INT NULL,
                direct_host VARCHAR(255) NULL,
                direct_user VARCHAR(255) NULL,
                direct_pwd VARCHAR(255) NULL,
                smtp_url VARCHAR(255) NULL,
                smtp_port VARCHAR(255) NULL,
                imap_url VARCHAR(255) NULL,
                imap_port VARCHAR(255) NULL,
                direct_recipient VARCHAR(255) NULL,
                rest_api_url VARCHAR(255) NULL,
                is_covid19 INT DEFAULT 0,
                is_emergent_reporting_enabled INT DEFAULT 0,
                is_full_ecr INT DEFAULT 1,
                rrprocessing_createdocRef INT DEFAULT 0,
                rrprocessing_invokerestapi INT DEFAULT 0,
                rrprocessing_both INT DEFAULT 0,
                rr_rest_api_url VARCHAR(255) NULL,
                rr_doc_ref_mime_type VARCHAR(255) NULL,
                launch_id VARCHAR(255) NULL,
                launch_state INT NULL,
                redirect_uri VARCHAR(255) NULL,
                auth_code VARCHAR(255) NULL,
                is_system_launch INT DEFAULT 1,
                is_multi_tenant_system_launch INT DEFAULT 0,
                is_user_account_launch INT DEFAULT 0,
                debug_fhir_query_and_eicr INT DEFAULT 0,
                require_aud INT DEFAULT 0,
                x_request_id VARCHAR(255) NULL,
                validation_mode INT DEFAULT 0,
                launch_type VARCHAR(255) NULL,
                processing_status VARCHAR(255) NULL
            );
        ';
        EXEC sp_executesql @sql;

        PRINT 'New table launch_details_v2 created successfully.';
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
        SET IDENTITY_INSERT dbo.' + QUOTENAME(@new_table) + N' ON;

        INSERT INTO dbo.' + QUOTENAME(@new_table) + N'
        (' + @col_list + ')
        SELECT ' + @select_list + '
        FROM dbo.' + QUOTENAME(@old_table) + N';

        SET IDENTITY_INSERT dbo.' + QUOTENAME(@new_table) + N' OFF;
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
