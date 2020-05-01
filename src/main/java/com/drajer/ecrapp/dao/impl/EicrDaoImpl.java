package com.drajer.ecrapp.dao.impl;

import org.springframework.stereotype.Repository;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;

@Repository
public class EicrDaoImpl extends AbstractDao implements EicrDao{
	
	public Eicr saveOrUpdate(Eicr eicr) {
		getSession().saveOrUpdate(eicr);
		return eicr;
	}
	
	public Eicr getEicrById(Integer id) {
		Eicr eicr = (Eicr) getSession().get(Eicr.class, id);
		return eicr;
	}

	public ReportabilityResponse saveOrUpdate(ReportabilityResponse rr) {
		getSession().saveOrUpdate(rr);
		return rr;
	}
	
	public ReportabilityResponse getRRById(Integer id) {
		ReportabilityResponse rr = (ReportabilityResponse) getSession().get(ReportabilityResponse.class, id);
		return rr;
	}
}
