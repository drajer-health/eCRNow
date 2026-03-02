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
    -- FINAL MESSAGE (only one print as requested)
    ------------------------------------------------------------------
    PRINT 'Schema update completed for kar_execution_state_v2.';

END TRY
BEGIN CATCH
    THROW;
END CATCH;
