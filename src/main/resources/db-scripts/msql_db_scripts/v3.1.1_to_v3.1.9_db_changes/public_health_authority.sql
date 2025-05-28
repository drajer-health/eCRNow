
ALTER TABLE public_health_authority
ADD backend_auth_key_alias text;

ALTER TABLE public_health_authority
ADD backend_auth_alg text NULL;

ALTER TABLE public_health_authority
ADD backend_auth_kid text NULL;