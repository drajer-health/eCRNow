-----------------------------------------------------------------------
-- Script: kar_execution_state.sql
-- Purpose: Add FK + Indexes for kar_execution_state_v2
-- Author: Bhargav
-- Date: 2025-11-13
-----------------------------------------------------------------------

BEGIN TRY

    ------------------------------------------------------------------
    -- 1. Add foreign key if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1
        FROM sys.foreign_keys
        WHERE name = 'fk_kar_execstate_v2_notificationcontext'
    )
    BEGIN
        ALTER TABLE kar_execution_state_v2
        ADD CONSTRAINT fk_kar_execstate_v2_notificationcontext
            FOREIGN KEY (nc_fk)
            REFERENCES notification_context_v2(id)
            ON DELETE SET NULL;
            -- SQL Server does NOT support ON UPDATE CASCADE unless PK is created with it
    END


    ------------------------------------------------------------------
    -- 2. Create indexes if not exists
    ------------------------------------------------------------------

    -- Index on nc_fk
    IF NOT EXISTS (
        SELECT 1 FROM sys.indexes
        WHERE name = 'idx_kar_execstate_v2_nc_fk'
          AND object_id = OBJECT_ID('kar_execution_state_v2')
    )
    BEGIN
        CREATE INDEX idx_kar_execstate_v2_nc_fk
            ON kar_execution_state_v2 (nc_fk);
    END

    -- Index on nc_id
    IF NOT EXISTS (
        SELECT 1 FROM sys.indexes
        WHERE name = 'idx_kar_execstate_v2_nc_id'
          AND object_id = OBJECT_ID('kar_execution_state_v2')
    )
    BEGIN
        CREATE INDEX idx_kar_execstate_v2_nc_id
            ON kar_execution_state_v2 (nc_id);
    END

    -- Index on kar_unique_id
    IF NOT EXISTS (
        SELECT 1 FROM sys.indexes
        WHERE name = 'idx_kar_execstate_v2_kar_unique_id'
          AND object_id = OBJECT_ID('kar_execution_state_v2')
    )
    BEGIN
        CREATE INDEX idx_kar_execstate_v2_kar_unique_id
            ON kar_execution_state_v2 (kar_unique_id);
    END


    ------------------------------------------------------------------
    -- FINAL MESSAGE (only one print as requested)
    ------------------------------------------------------------------
    PRINT 'Schema update completed for kar_execution_state_v2.';

END TRY
BEGIN CATCH
    THROW;
END CATCH;
