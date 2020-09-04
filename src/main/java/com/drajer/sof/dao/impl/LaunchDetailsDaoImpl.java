package com.drajer.sof.dao.impl;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.dao.LaunchDetailsDao;
import com.drajer.sof.model.LaunchDetails;

@Repository
@Transactional
public class LaunchDetailsDaoImpl extends AbstractDao implements LaunchDetailsDao{

	public LaunchDetails saveOrUpdate(LaunchDetails authDetails) {
		getSession().saveOrUpdate(authDetails);
		return authDetails;
	}
	
	public LaunchDetails getAuthDetailsById(Integer id) {
		LaunchDetails authDetails = (LaunchDetails) getSession().get(LaunchDetails.class, id);
		return authDetails;
	}

	public LaunchDetails getLaunchDetailsByPatientAndEncounter(String patient, String encounter,String fhirServerUrl) {
		Criteria criteria = getSession().createCriteria(LaunchDetails.class);
		criteria.add(Restrictions.eq("ehrServerURL", fhirServerUrl));
		criteria.add(Restrictions.eq("launchPatientId", patient));
		criteria.add(Restrictions.eq("encounterId", encounter));
		LaunchDetails launchDetails = (LaunchDetails) criteria.uniqueResult();
		return launchDetails;
	}

	public LaunchDetails getLaunchDetailsByState(int state) {
		Criteria criteria = getSession().createCriteria(LaunchDetails.class);
		criteria.add(Restrictions.eq("launchState", state));
		LaunchDetails launchDetails = (LaunchDetails) criteria.uniqueResult();
		return launchDetails;
	}

}
