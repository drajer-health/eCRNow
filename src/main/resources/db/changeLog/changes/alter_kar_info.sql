ALTER TABLE IF EXISTS kar_info
    ADD CONSTRAINT id_fk FOREIGN KEY (repo_id)
    REFERENCES kar_repos (id);