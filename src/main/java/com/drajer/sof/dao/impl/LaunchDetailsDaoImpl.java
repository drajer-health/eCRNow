package com.drajer.sof.dao.impl;

import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
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

	public LaunchDetails getLaunchDetailsByPatientAndEncounter(String patient, String encounter,String fhirServerUrl) {
		Criteria criteria = getSession().createCriteria(LaunchDetails.class);
		criteria.add(Restrictions.eq("ehrServerURL", fhirServerUrl));
		criteria.add(Restrictions.eq("launchPatientId", patient));
		criteria.add(Restrictions.eq("encounterId", encounter));
		LaunchDetails launchDetails = (LaunchDetails) criteria.uniqueResult();
		return launchDetails;
	}

	public List<LaunchDetails> getAllLaunchDetails() {
		Criteria criteria = getSession().createCriteria(LaunchDetails.class);
		List<LaunchDetails> launchDetailsList = criteria.addOrder(Order.desc("id")).list();
		return launchDetailsList;	
	}

}
