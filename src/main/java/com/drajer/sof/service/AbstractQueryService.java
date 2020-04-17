package com.drajer.sof.service;

import java.util.Date;

import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;

public interface AbstractQueryService {

	public FhirData getData(LaunchDetails details, Date start, Date end);
}
