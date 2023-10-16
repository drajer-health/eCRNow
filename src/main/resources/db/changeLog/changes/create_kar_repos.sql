CREATE TABLE IF NOT EXISTS kar_repos (
    id INT PRIMARY KEY,
    repo_fhir_url varchar(255) NOT NULL,
    repo_name varchar(255) NOT NULL UNIQUE,
    repo_status bit
);
