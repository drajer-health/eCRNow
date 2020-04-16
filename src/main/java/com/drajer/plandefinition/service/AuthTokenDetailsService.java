package com.drajer.plandefinition.service;

import com.drajer.plandefinition.model.AuthTokenDetails;

public interface AuthTokenDetailsService {

	AuthTokenDetails saveOrUpdate(AuthTokenDetails authDetails);
	
	AuthTokenDetails getAuthDetailsById(Integer id);
}
