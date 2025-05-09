ALTER TABLE public_health_authority
  ADD COLUMN backend_auth_key_alias TEXT;

  ALTER TABLE public_health_authority
  ADD COLUMN backend_auth_alg TEXT NULL;

  ALTER TABLE public_health_authority
  ADD COLUMN backend_auth_kid TEXT NULL;
