-- ==================================================================
-- healthcare_setting.sql
-- Purpose: Create healthcare_setting_v2 and migrate data from healthcare_setting
-- Note: Constraints handled separately
-- ==================================================================

-- Create sequence for Hibernate ID generation
IF NOT EXISTS (
    SELECT 1 FROM sys.sequences
    WHERE name = 'healthcare_setting_v2_SEQ' AND SCHEMA_ID = SCHEMA_ID('dbo')
)
BEGIN
    CREATE SEQUENCE dbo.healthcare_setting_v2_SEQ
        AS INT
        START WITH 1
        INCREMENT BY 50
        NO MINVALUE
        NO MAXVALUE
        NO CYCLE;
END

BEGIN TRY
    BEGIN TRANSACTION;

    DECLARE
        @old_table NVARCHAR(128) = 'healthcare_setting',
        @new_table NVARCHAR(128) = 'healthcare_setting_v2',
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
        SELECT 1 FROM INFORMATION_SCHEMA.TABLES
        WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @old_table
    )
        SET @old_table_exists = 1;
    ELSE
        PRINT 'Old table does NOT exist. Will create table only, skipping migration.';


    ------------------------------------------------------------------
    -- 2. Always create new table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM INFORMATION_SCHEMA.TABLES
        WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = @new_table
    )
    BEGIN
        PRINT 'Creating new table...';

        SET @sql = N'
            CREATE TABLE dbo.' + QUOTENAME(@new_table) + N' (
                id INT DEFAULT NEXT VALUE FOR dbo.healthcare_setting_v2_SEQ PRIMARY KEY,
                clientId VARCHAR(8000) NOT NULL,
                clientSecret VARCHAR(8000) NULL,
                fhir_server_base_url VARCHAR(255) NOT NULL,
                fhir_version VARCHAR(255) NULL,
                token_url VARCHAR(8000) NULL,
                scopes VARCHAR(MAX) NOT NULL,
                default_provider_id VARCHAR(8000) NULL,
                is_direct INT DEFAULT 0,
                is_xdr INT DEFAULT 0,
                is_restapi INT DEFAULT 0,
                is_fhir INT DEFAULT 0,
                direct_host VARCHAR(8000) NULL,
                direct_user VARCHAR(255) NULL,
                direct_pwd VARCHAR(255) NULL,
                smtp_port VARCHAR(255) NULL,
                smtp_url VARCHAR(255) NULL,
                imap_port VARCHAR(255) NULL,
                imap_url VARCHAR(255) NULL,
                pop_port VARCHAR(255) NULL,
                pop_url VARCHAR(255) NULL,
                direct_recipient_address VARCHAR(255) NULL,
                smtp_tls_version VARCHAR(255) NULL,
                xdr_recipient_address VARCHAR(255) NULL,
                rest_api_url VARCHAR(255) NULL,
                create_doc_ref_response INT DEFAULT 0,
                doc_ref_mime_type VARCHAR(255) NULL,
                response_rest_api_url VARCHAR(255) NULL,
                aa_id VARCHAR(255) NULL,
                encounter_start_time VARCHAR(255) NULL,
                encounter_end_time VARCHAR(255) NULL,
                kars_active VARCHAR(MAX) NULL,
                auth_type VARCHAR(8000) NOT NULL,
                ehr_access_token VARCHAR(MAX) NULL,
                ehr_access_token_expiry_duration INT NULL DEFAULT 0,
                token_expiry_date DATETIME2(6) NULL,
                require_aud INT DEFAULT 0,
                ehr_supports_subscriptions INT DEFAULT 0,
                trusted_third_party VARCHAR(8000) NULL,
                pha_url VARCHAR(8000) NULL,
                org_name VARCHAR(8000) NULL,
                org_id_system VARCHAR(8000) NULL,
                org_id VARCHAR(8000) NULL,
                off_hours_start INT NULL,
                off_hours_start_min INT NULL,
                off_hours_end INT NULL,
                off_hours_end_min INT NULL,
                off_hours_timezone VARCHAR(8000) NULL,
                off_hours_enabled INT DEFAULT 0,
                username VARCHAR(8000) NULL,
                password VARCHAR(8000) NULL,
                backend_auth_key_alias VARCHAR(8000) NULL,
                backend_auth_alg VARCHAR(8000) NULL,
                backend_auth_kid VARCHAR(8000) NULL,
                debug_enabled INT DEFAULT 0,
                direct_endpoint_cert_alias VARCHAR(8000) NULL,
                smtp_auth_enabled INT DEFAULT 0,
                smtp_ssl_enabled INT DEFAULT 0,
                last_updated_ts datetime2(6) NOT NULL DEFAULT GETDATE()

            );

            CREATE INDEX idx_fhir_server_base_url
            ON dbo.' + QUOTENAME(@new_table) + N' (fhir_server_base_url);
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
        PRINT 'Column mismatch detected.';
        PRINT 'Missing in old table: ' + @missing_col_names;
        THROW 50002, 'Column mismatch detected. Migration aborted.', 1;
    END

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
        SELECT ' + @select_list + ' FROM dbo.' + QUOTENAME(@old_table) + ';

        SET IDENTITY_INSERT dbo.' + QUOTENAME(@new_table) + ' OFF;
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
