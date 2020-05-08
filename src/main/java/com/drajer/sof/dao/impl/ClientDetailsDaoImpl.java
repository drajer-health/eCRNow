package com.drajer.sof.dao.impl;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.dao.ClientDetailsDao;
import com.drajer.sof.model.ClientDetails;

@Repository
public class ClientDetailsDaoImpl extends AbstractDao implements ClientDetailsDao{

	public ClientDetails saveOrUpdate(ClientDetails clientDetails) {
		getSession().saveOrUpdate(clientDetails);
		return null;
	}

	public ClientDetails getClientDetailsById(Integer id) {
		ClientDetails clientDetails = (ClientDetails) getSession().get(ClientDetails.class, id);
		return clientDetails;
	}
	
	public ClientDetails getClientDetailsByUrl(String url) {
		Criteria criteria = getSession().createCriteria(ClientDetails.class);
		criteria.add(Restrictions.eq("fhirServerBaseURL", url));
		ClientDetails clientDetails = (ClientDetails) criteria.uniqueResult();
		return clientDetails;
	}

}
