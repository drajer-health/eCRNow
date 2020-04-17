package com.drajer.sof.dao.impl;

import org.springframework.stereotype.Repository;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.dao.LaunchDetailsDao;
import com.drajer.sof.model.LaunchDetails;

@Repository
public class LaunchDetailsDaoImpl extends AbstractDao implements LaunchDetailsDao{

	public LaunchDetails saveOrUpdate(LaunchDetails authDetails) {
		getSession().saveOrUpdate(authDetails);
		return authDetails;
	}
	
	public LaunchDetails getAuthDetailsById(Integer id) {
		LaunchDetails authDetails = (LaunchDetails) getSession().get(LaunchDetails.class, id);
		return authDetails;
	}

}
