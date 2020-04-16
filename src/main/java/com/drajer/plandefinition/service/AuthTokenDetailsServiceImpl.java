package com.drajer.plandefinition.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.drajer.plandefinition.dao.AuthTokenDetailsDao;
import com.drajer.plandefinition.model.AuthTokenDetails;

@Service
@Transactional
public class AuthTokenDetailsServiceImpl implements AuthTokenDetailsService{

	@Autowired
	AuthTokenDetailsDao authDetailsDao;
	
	public AuthTokenDetails saveOrUpdate(AuthTokenDetails authDetails) {
		authDetailsDao.saveOrUpdate(authDetails);
		return authDetails;
	}

	public AuthTokenDetails getAuthDetailsById(Integer id) {
		return authDetailsDao.getAuthDetailsById(id);
	}

}
