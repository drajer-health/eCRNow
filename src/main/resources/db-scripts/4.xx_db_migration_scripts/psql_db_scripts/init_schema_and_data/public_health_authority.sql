DO
$$
DECLARE
    old_count BIGINT;
    new_count BIGINT;
    data_transfer_success BOOLEAN := FALSE;
BEGIN
    ------------------------------------------------------------------
    -- 1. Create public_health_authority_v2 table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_name = 'public_health_authority_v2'
    ) THEN
        CREATE TABLE public_health_authority_v2 (
            id SERIAL PRIMARY KEY,
            clientId VARCHAR(8000) NOT NULL,
            clientSecret TEXT,
            username VARCHAR(8000),
            password VARCHAR(8000),
            fhir_server_base_url VARCHAR(255) NOT NULL,
            fhir_version VARCHAR(255),
            token_url VARCHAR(8000),
            scopes TEXT NOT NULL,
            require_aud INT DEFAULT 0,
            auth_type VARCHAR(8000) NOT NULL,
            backend_auth_key_alias VARCHAR(8000),
            backend_auth_alg VARCHAR(8000),
            backend_auth_kid VARCHAR(8000),
            last_updated_ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
        );
        RAISE NOTICE '✅ public_health_authority_v2 table created.';
    ELSE
        RAISE NOTICE 'ℹ public_health_authority_v2 table already exists.';
    END IF;

    ------------------------------------------------------------------
    -- 2. Create migration_log table if not exists
    ------------------------------------------------------------------
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_name = 'migration_log'
    ) THEN
        CREATE TABLE migration_log (
            table_name VARCHAR(100),
            data_transfer BOOLEAN
        );
        RAISE NOTICE '✅ migration_log table created.';
    ELSE
        RAISE NOTICE 'ℹ migration_log table already exists.';
    END IF;

    ------------------------------------------------------------------
    -- 3. Migrate data from old table
    ------------------------------------------------------------------
    IF EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_name = 'public_health_authority'
    ) THEN
        SELECT COUNT(*) INTO old_count FROM public_health_authority;

        IF old_count > 0 THEN
            BEGIN
                INSERT INTO public_health_authority_v2 (
                    id, clientId, clientSecret, username, password,
                    fhir_server_base_url, fhir_version, token_url,
                    scopes, require_aud, auth_type,
                    backend_auth_key_alias, backend_auth_alg, backend_auth_kid,
                    last_updated_ts
                )
                SELECT
                    id,
                    clientId,
                    clientSecret,
                    username,
                    password,
                    fhir_server_base_url,
                    fhir_version,
                    token_url,
                    scopes,
                    COALESCE(require_aud, 0),  -- safe migration handling
                    auth_type,
                    backend_auth_key_alias,
                    backend_auth_alg,
                    backend_auth_kid,
                    last_updated_ts
                FROM public_health_authority pha
                WHERE NOT EXISTS (
                    SELECT 1 FROM public_health_authority_v2 n
                    WHERE n.fhir_server_base_url = pha.fhir_server_base_url
                      AND n.clientId = pha.clientId
                      AND n.auth_type = pha.auth_type
                );

                SELECT COUNT(*) INTO new_count FROM public_health_authority_v2;

                IF new_count >= old_count THEN
                    data_transfer_success := TRUE;
                    RAISE NOTICE '✅ Data migrated successfully for public_health_authority.';
                ELSE
                    data_transfer_success := FALSE;
                    RAISE NOTICE '⚠ Row count mismatch during migration.';
                END IF;

            EXCEPTION WHEN OTHERS THEN
                data_transfer_success := FALSE;
                RAISE NOTICE '❌ Error during data migration: %', SQLERRM;
            END;
        ELSE
            RAISE NOTICE 'ℹ No data to migrate from public_health_authority.';
        END IF;
    ELSE
        RAISE NOTICE '⚠ Source table public_health_authority does not exist. Migration skipped.';
    END IF;

    ------------------------------------------------------------------
    -- 4. Log migration result
    ------------------------------------------------------------------
    INSERT INTO migration_log (table_name, data_transfer)
    VALUES ('public_health_authority', data_transfer_success);
END
$$;
