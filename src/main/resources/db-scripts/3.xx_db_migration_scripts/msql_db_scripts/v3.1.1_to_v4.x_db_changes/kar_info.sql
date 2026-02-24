ALTER TABLE kar_info
ADD kar_available INT DEFAULT 0;

//if run below update query if data already exist
UPDATE kar_info SET kar_available = 0 WHERE kar_available IS NULL;