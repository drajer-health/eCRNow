CREATE TABLE IF NOT EXISTS kar_info (
        id INT PRIMARY KEY,
        kar_id varchar(255) not null,
        kar_name varchar(255) not null,
        kar_publisher bit,
        kar_version varchar(255) not null,
        UNIQUE (id)
    )