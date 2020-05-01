package com.drajer.sof.service.impl;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.drajer.sof.dao.EicrDao;
import com.drajer.sof.model.Eicr;
import com.drajer.sof.service.EicrService;

@Service
@Transactional
public class EicrServiceImpl implements EicrService {

	@Autowired
	EicrDao eicrDao;

	public Eicr saveOrUpdate(Eicr eicr) {
		eicrDao.saveOrUpdate(eicr);
		return eicr;
	}

	public Eicr getEicrById(Integer id) {
		return eicrDao.getEicrById(id);
	}
}
