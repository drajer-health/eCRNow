CREATE TABLE IF NOT EXISTS kar_repos
(
    id INT PRIMARY KEY,
    repo_fhir_url  VARCHAR(255) NOT NULL,
    repo_name VARCHAR(255) NOT NULL UNIQUE,
    repo_status integer
);
