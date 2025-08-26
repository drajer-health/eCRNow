-- Replace :planUrl with the PlanDefinition URL at runtime
SELECT *
FROM scheduled_tasks
WHERE task_instance LIKE CONCAT('%', :planDefinitionUrl, '%');

-- ========================================
-- Example Usage:
-- Search for us-ecr-specification plan
-- ========================================
SELECT *
FROM scheduled_tasks
WHERE task_instance LIKE '%http://ersd.aimsplatform.org/fhir/PlanDefinition/us-ecr-specification%';