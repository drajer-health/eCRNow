package com.drajer.sof.service;

import java.util.Date;

import com.drajer.sof.model.FhirData;

public interface AbstractQueryService {

	public FhirData getData(String patId, String encId, Date start, Date end);
}
