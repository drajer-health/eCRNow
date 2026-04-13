-- =====================================================================
-- COMPREHENSIVE FIX: ALL _v2 Tables Sequence Synchronization
-- FOR SQL SERVER (MSSQL)
-- =====================================================================
-- This is the MASTER FIX script for SQL Server databases
-- Run this to fix ALL _v2 table sequences at once
--
-- Fixes BOTH:
--   1. INCREMENT BY (must be 50 for Hibernate allocationSize=50)
--   2. RESTART WITH  (must be aligned to next pool boundary)
-- =====================================================================

PRINT '==========================================================';
PRINT 'SQL SERVER - Fix All _v2 Sequences';
PRINT 'Date: ' + CONVERT(VARCHAR, GETDATE(), 120);
PRINT '==========================================================';
PRINT '';

DECLARE
    @tableName NVARCHAR(255),
    @schemaName NVARCHAR(255),
    @fullTable NVARCHAR(500),
    @seqName NVARCHAR(255),
    @fullSeq NVARCHAR(500),
    @maxId BIGINT,
    @nextId BIGINT,
    @currentSeqValue BIGINT,
    @currentIncrement INT,
    @sql NVARCHAR(MAX),
    @tableCount INT = 0,
    @fixedCount INT = 0,
    @skippedCount INT = 0;

DECLARE table_cursor CURSOR FOR
SELECT
    t.name AS table_name,
    s.name AS schema_name
FROM sys.tables t
JOIN sys.schemas s ON t.schema_id = s.schema_id
WHERE t.name LIKE '%_v2'
ORDER BY t.name;

OPEN table_cursor;
FETCH NEXT FROM table_cursor INTO @tableName, @schemaName;

WHILE @@FETCH_STATUS = 0
BEGIN
    SET @tableCount = @tableCount + 1;
    PRINT '─────────────────────────────────────────────────────────';
    PRINT '[' + CAST(@tableCount AS VARCHAR) + '] Processing: ' + @schemaName + '.' + @tableName;

    SET @fullTable = QUOTENAME(@schemaName) + '.' + QUOTENAME(@tableName);

    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    -- Step 1: Check if 'id' column exists
    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    IF NOT EXISTS (
        SELECT 1
        FROM sys.columns
        WHERE object_id = OBJECT_ID(@fullTable)
          AND name = 'id'
    )
    BEGIN
        PRINT '  [SKIP] No id column found';
        SET @skippedCount = @skippedCount + 1;
        GOTO NEXT_TABLE;
    END

    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    -- Step 2: Find sequence (naming convention: table_v2_SEQ)
    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    SET @seqName = @tableName + '_SEQ';
    SET @fullSeq = QUOTENAME(@schemaName) + '.' + QUOTENAME(@seqName);

    IF NOT EXISTS (
        SELECT 1 FROM sys.sequences
        WHERE name = @seqName
          AND schema_id = SCHEMA_ID(@schemaName)
    )
    BEGIN
        PRINT '  [SKIP] No sequence found (' + @seqName + ')';
        SET @skippedCount = @skippedCount + 1;
        GOTO NEXT_TABLE;
    END

    PRINT '  [OK] Found sequence: ' + @seqName;

    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    -- Step 3: Check and fix INCREMENT BY
    --         MUST be 50 for Hibernate allocationSize=50
    --         With INCREMENT BY 1, Hibernate gets overlapping
    --         ID pools causing duplicate key errors!
    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    SELECT @currentIncrement = CAST(increment AS INT)
    FROM sys.sequences
    WHERE name = @seqName AND schema_id = SCHEMA_ID(@schemaName);

    IF @currentIncrement <> 50
    BEGIN
        PRINT '  [FIX] INCREMENT BY is ' + CAST(@currentIncrement AS VARCHAR) + ' -> must be 50';
        SET @sql = N'ALTER SEQUENCE ' + @fullSeq + N' INCREMENT BY 50';
        BEGIN TRY
            EXEC sp_executesql @sql;
            PRINT '  [OK] INCREMENT BY fixed to 50';
        END TRY
        BEGIN CATCH
            PRINT '  [ERROR] Failed to fix INCREMENT BY: ' + ERROR_MESSAGE();
        END CATCH
    END
    ELSE
        PRINT '  [OK] INCREMENT BY = 50 (correct)';

    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    -- Step 4: Get MAX(id) from table
    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    SET @sql = N'SELECT @maxIdOUT = ISNULL(MAX(id), 0) FROM ' + @fullTable;
    EXEC sp_executesql @sql, N'@maxIdOUT BIGINT OUTPUT', @maxId OUTPUT;
    PRINT '  [INFO] MAX(id) = ' + CAST(@maxId AS VARCHAR);

    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    -- Step 5: Get current sequence value
    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    SELECT @currentSeqValue = CAST(current_value AS BIGINT)
    FROM sys.sequences
    WHERE name = @seqName AND schema_id = SCHEMA_ID(@schemaName);
    PRINT '  [INFO] Current sequence value = ' + CAST(@currentSeqValue AS VARCHAR);

    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    -- Step 6: Calculate correct next value for Hibernate
    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    -- Hibernate pooled-lo optimizer with allocationSize=50:
    --   - Gets sequence value N
    --   - Allocates IDs: N, N+1, N+2, ..., N+49
    --   - Next call gets N+50
    --
    -- Formula: (((MAX - 1) / 50) + 1) * 50 + 1
    -- Examples:
    --   MAX=0  -> next=1   (pool: 1-50)
    --   MAX=2  -> next=51  (pool: 51-100)
    --   MAX=50 -> next=51  (pool: 51-100)
    --   MAX=51 -> next=101 (pool: 101-150)
    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    IF @maxId = 0
        SET @nextId = 1;
    ELSE
        SET @nextId = ((((@maxId - 1) / 50) + 1) * 50) + 1;

    PRINT '  [INFO] Calculated next value = ' + CAST(@nextId AS VARCHAR);

    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    -- Step 7: Check if update is needed
    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    IF @currentSeqValue = @nextId
    BEGIN
        PRINT '  [OK] ALREADY CORRECT - No update needed';
        PRINT '';
        GOTO NEXT_TABLE;
    END

    IF @currentSeqValue <= @maxId
    BEGIN
        PRINT '  [CRITICAL] Sequence BEHIND data - will generate duplicates!';
        PRINT '  [FIX] Fixing now...';
    END
    ELSE IF @currentSeqValue > @maxId AND @currentSeqValue >= @nextId
    BEGIN
        PRINT '  [WARN] Sequence ahead but not optimally aligned';
        PRINT '  [FIX] Updating for optimal Hibernate alignment...';
    END
    ELSE
    BEGIN
        PRINT '  [FIX] Aligning sequence...';
    END

    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    -- Step 8: Reset sequence
    -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    SET @sql = N'ALTER SEQUENCE ' + @fullSeq + N' RESTART WITH ' + CAST(@nextId AS VARCHAR);

    BEGIN TRY
        EXEC sp_executesql @sql;
        PRINT '  [OK] Sequence reset to ' + CAST(@nextId AS VARCHAR);
        SET @fixedCount = @fixedCount + 1;
    END TRY
    BEGIN CATCH
        PRINT '  [ERROR] Failed to reset sequence: ' + ERROR_MESSAGE();
    END CATCH

    PRINT '';

    NEXT_TABLE:
    FETCH NEXT FROM table_cursor INTO @tableName, @schemaName;
END

CLOSE table_cursor;
DEALLOCATE table_cursor;

-- =====================================================================
-- SUMMARY
-- =====================================================================
PRINT '==========================================================';
PRINT 'SUMMARY';
PRINT '==========================================================';
PRINT 'Tables processed: ' + CAST(@tableCount AS VARCHAR);
PRINT 'Sequences fixed:  ' + CAST(@fixedCount AS VARCHAR);
PRINT 'Skipped:          ' + CAST(@skippedCount AS VARCHAR);
PRINT '==========================================================';
PRINT '';

-- =====================================================================
-- VERIFICATION: Show final state of all _v2 sequences
-- =====================================================================
PRINT 'VERIFICATION - Current state of all _v2 sequences:';
PRINT '';

SELECT
    seq.name                          AS sequence_name,
    CAST(seq.current_value AS BIGINT) AS current_value,
    CAST(seq.increment AS INT)        AS increment_by,
    CASE
        WHEN CAST(seq.increment AS INT) <> 50
        THEN '** WRONG - must be 50!'
        ELSE 'OK'
    END                               AS increment_status
FROM sys.sequences seq
JOIN sys.schemas sch ON seq.schema_id = sch.schema_id
WHERE seq.name LIKE '%_v2_SEQ'
ORDER BY seq.name;

PRINT '';
PRINT 'Done. You can now safely restart your application.';
PRINT '==========================================================';

