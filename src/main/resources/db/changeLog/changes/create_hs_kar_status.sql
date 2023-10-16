CREATE TABLE IF NOT EXISTS public.hs_kar_status
(
    id INT PRIMARY KEY,
    is_only_covid integer NOT NULL,
    hs_id integer NOT NULL,
    is_activated integer NOT NULL,
    kar_id varchar(255) NOT NULL,
    kar_version varchar(255) NOT NULL,
    last_activation_date timestamp,
    last_inactivation_date timestamp,
    output_format varchar(255),
    subscriptions text[],
    is_subscriptions_enabled integer NOT NULL,
    map_versionid_karid varchar(255) NOT NULL,
    hs_fk integer,
    CONSTRAINT healthcareSetting_fk FOREIGN KEY (hs_fk)
        REFERENCES healthcare_setting (id)
);