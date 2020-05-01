package com.drajer.sof.dao.impl;

import org.springframework.stereotype.Repository;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.dao.EicrDao;
import com.drajer.sof.model.Eicr;

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

}
