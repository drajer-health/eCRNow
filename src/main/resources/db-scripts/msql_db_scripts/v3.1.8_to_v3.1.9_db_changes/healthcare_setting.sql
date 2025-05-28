      ALTER TABLE healthcare_setting
      ADD direct_endpoint_cert_alias text;

    ALTER TABLE healthcare_setting
      ADD smtp_auth_enabled INT DEFAULT 0;

    ALTER TABLE healthcare_setting
      ADD smtp_ssl_enabled INT DEFAULT 0;


  //if run below update query if data already exist
    UPDATE healthcare_setting SET smtp_auth_enabled = 0 WHERE smtp_auth_enabled IS NULL;
     UPDATE healthcare_setting SET smtp_ssl_enabled = 0 WHERE smtp_ssl_enabled IS NULL;
