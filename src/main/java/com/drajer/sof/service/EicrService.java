package com.drajer.sof.service;

import com.drajer.sof.model.Eicr;

public interface EicrService {

	Eicr saveOrUpdate(Eicr eicr);
	
	Eicr getEicrById(Integer id);
}
