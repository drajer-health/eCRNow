package com.drajer.plandefinition.service;

import org.hl7.fhir.r4.model.ValueSet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import com.drajer.plandefinition.dao.ValueSetDao;
import com.drajer.plandefinition.model.ValueSetModel;

import ca.uhn.fhir.parser.IParser;

@Service("valueSetServiceImpl")
public class ValueSetServiceImpl implements ValueSetService {
	
	@Autowired
	@Qualifier("jsonParser")
	IParser jsonParser;
	
	@Autowired
	@Qualifier("valueSetDaoImpl")
	ValueSetDao valueSetDao;
	
	@Override
	public void createValueSet(ValueSet valueSet) {
		ValueSetModel valueSetModel = new ValueSetModel();
		valueSetModel.setValueSetId(valueSet.getId());
		valueSetModel.setData(jsonParser.encodeResourceToString(valueSet));
		
		valueSetDao.createValueset(valueSetModel);
	}
}
