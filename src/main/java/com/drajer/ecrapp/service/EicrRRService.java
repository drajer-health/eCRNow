package com.drajer.ecrapp.service;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;

public interface EicrRRService {

	Eicr saveOrUpdate(Eicr eicr);
	
	Eicr getEicrById(Integer id);
	
	ReportabilityResponse saveOrUpdate(ReportabilityResponse rr);
	
	ReportabilityResponse getRRById(Integer id);
}
