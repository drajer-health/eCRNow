package com.drajer.plandefinition.dao;

import com.drajer.plandefinition.model.AuthTokenDetails;

public interface AuthTokenDetailsDao {
	
	AuthTokenDetails saveOrUpdate(AuthTokenDetails authDetails);
	
	AuthTokenDetails getAuthDetailsById(Integer id);

}
