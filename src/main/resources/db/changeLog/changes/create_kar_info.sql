CREATE TABLE IF NOT EXISTS kar_info (
        id INT PRIMARY KEY,
        kar_id varchar(255) not null,
        kar_name varchar(255) not null,
        kar_publisher varchar(255),
        kar_version varchar(255),
        repo_id int,
        UNIQUE (id)
    )