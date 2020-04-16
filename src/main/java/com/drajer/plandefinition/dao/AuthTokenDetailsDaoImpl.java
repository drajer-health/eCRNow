package com.drajer.plandefinition.dao;

import org.springframework.stereotype.Repository;

import com.drajer.plandefinition.model.AuthTokenDetails;

@Repository
public class AuthTokenDetailsDaoImpl extends AbstractDao implements AuthTokenDetailsDao{

	public AuthTokenDetails saveOrUpdate(AuthTokenDetails authDetails) {
		getSession().saveOrUpdate(authDetails);
		return authDetails;
	}
	
	public AuthTokenDetails getAuthDetailsById(Integer id) {
		AuthTokenDetails authDetails = (AuthTokenDetails) getSession().get(AuthTokenDetails.class, id);
		return authDetails;
	}

}
