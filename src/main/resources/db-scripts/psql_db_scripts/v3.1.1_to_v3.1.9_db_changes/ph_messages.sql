
CREATE INDEX idx_not_res_id ON ph_messages (notified_resource_id);

CREATE INDEX idx_kar_id ON ph_messages (kar_unique_id);
