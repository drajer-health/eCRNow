CREATE TABLE IF NOT EXISTS scheduled_tasks (
    task_instance VARCHAR(255),
    task_name VARCHAR(255),
    task_data BYTEA,
    execution_time TIMESTAMP,
    picked bit,
    picked_by VARCHAR(255),
    last_success TIMESTAMP,
    last_failure TIMESTAMP,
    consecutive_failures INT,
    last_heartbeat TIMESTAMP,
    version INT,
    PRIMARY KEY (task_instance, task_name)
);

