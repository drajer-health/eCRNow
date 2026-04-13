-- ==================================================================

-- client_details.sql (UPDATED as per requirement)

-- Purpose: Always create client_details_v2.

-- If old table exists → migrate data.

-- If old table does NOT exist → skip migration (no error).

-- ==================================================================

IF NOT EXISTS (
    SELECT 1 FROM sys.sequences
    WHERE name = 'client_details_v2_SEQ' AND SCHEMA_ID = SCHEMA_ID('dbo')
)
BEGIN
    CREATE SEQUENCE dbo.client_details_v2_SEQ
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

        @old_table NVARCHAR(128) = 'client_details',

        @new_table NVARCHAR(128) = 'client_details_v2',

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

                   id INT DEFAULT NEXT VALUE FOR dbo.client_details_v2_SEQ PRIMARY KEY,

                   is_provider_launch INT DEFAULT 0 NOT NULL,

                   is_system_launch INT DEFAULT 1 NOT NULL,

                   is_multi_tenant_system_launch INT DEFAULT 0 NULL,

                   is_user_account_launch INT DEFAULT 0 NULL,

                   clientId VARCHAR(8000) NOT NULL,

                   clientSecret VARCHAR(MAX) NULL,

                   fhir_server_base_url VARCHAR(255) NOT NULL,

                   token_url VARCHAR(8000) NULL,

                   scopes VARCHAR(MAX) NOT NULL,

                   access_token VARCHAR(MAX) NULL,

                   token_expiry INT DEFAULT 0 NULL,

                   token_expiry_date DATETIME2(6) NULL,

                   is_direct INT DEFAULT 0 NOT NULL,

                   is_xdr INT DEFAULT 0 NOT NULL,

                   is_restapi INT DEFAULT 0 NOT NULL,

                   direct_host VARCHAR(8000) NULL,

                   direct_user VARCHAR(255) NULL,

                   direct_pwd VARCHAR(255) NULL,

                   smtp_url VARCHAR(255) NULL,

                   smtp_port VARCHAR(255) NULL,

                   imap_url VARCHAR(255) NULL,

                   imap_port VARCHAR(255) NULL,

                   direct_recipient_address VARCHAR(255) NULL,

                   xdr_recipient_address VARCHAR(255) NULL,

                   rest_api_url VARCHAR(255) NULL,

                   aa_id VARCHAR(255) NULL,

                   encounter_start_time VARCHAR(255) NULL,

                   encounter_end_time VARCHAR(255) NULL,

                   is_covid19 INT DEFAULT 0 NOT NULL,

                   is_full_ecr INT DEFAULT 1 NOT NULL,

                   is_emergent_reporting_enabled INT DEFAULT 0 NOT NULL,

                   rrprocessing_createdocRef INT DEFAULT 0 NOT NULL,

                   rrprocessing_invokerestapi INT DEFAULT 0 NOT NULL,

                   rrprocessing_both INT DEFAULT 0 NOT NULL,

                   rr_rest_api_url VARCHAR(255) NULL,

                   rr_doc_ref_mime_type VARCHAR(255) NULL,

                   debug_fhir_query_and_eicr INT DEFAULT 0 NOT NULL,

                   require_aud INT DEFAULT 0 NOT NULL,

                  last_updated_ts DATETIME NOT NULL DEFAULT GETDATE()


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

        PRINT 'Column mismatch: ' + @missing_col_names;

        THROW 50002, 'Column mismatch detected. Migration aborted.', 1;

    END


    PRINT 'Column validation successful.';


    ------------------------------------------------------------------

    -- 5. Count rows in old table

    ------------------------------------------------------------------

    SET @sql = N'SELECT @cnt = COUNT(*) FROM dbo.' + QUOTENAME(@old_table);

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

    -- 7. Copy data (SEQUENCE-based table — no IDENTITY_INSERT needed)

    ------------------------------------------------------------------

    PRINT 'Migrating data...';

    SET @sql = N'

        INSERT INTO dbo.' + QUOTENAME(@new_table) + ' (' + @col_list + ')

        SELECT ' + @select_list + ' FROM dbo.' + QUOTENAME(@old_table) + ';

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

