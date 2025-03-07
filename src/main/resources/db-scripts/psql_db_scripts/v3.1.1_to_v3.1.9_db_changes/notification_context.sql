ALTER TABLE notification_context
  ADD COLUMN encounter_class VARCHAR(255);

ALTER TABLE notification_context
ADD COLUMN relaunch_notification_data VARCHAR(255);


-- ==============================================
--  CAUTION: Test in Local Before Running in Production!
-- ==============================================

-- Step 1: Add the unique constraint to the table (Only if no duplicates exist)
-- Run this query directly if you don't have duplicate data.

ALTER TABLE notification_context
  ADD CONSTRAINT unique_notification_context UNIQUE (fhir_server_base_url, patient_id, notification_resource_id, notification_resource_type, trigger_event);



  -- Step 2: If you have duplicate data, first run the following commands.


-- Enable the uuid-ossp extension to use uuid_generate_v4() (for generating UUIDs)

  CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Step 3: Update the duplicate rows with a random UUID appended to the trigger_event field
   UPDATE notification_context
  SET trigger_event = trigger_event || '_old_' || uuid_generate_v4()::TEXT where id in (

   SELECT id
      FROM (
          SELECT id,
                 ROW_NUMBER() OVER (
                     PARTITION BY fhir_server_base_url, patient_id, notification_resource_id,
                                  notification_resource_type, trigger_event
                     ORDER BY id
                 ) AS row_num
          FROM notification_context
      ) AS duplicates
      WHERE row_num > 1)




  -- Step 4: Finally, run the ALTER TABLE command to add the unique constraint

  	ALTER TABLE notification_context
    ADD CONSTRAINT unique_notification_context UNIQUE (fhir_server_base_url, patient_id, notification_resource_id, notification_resource_type, trigger_event);