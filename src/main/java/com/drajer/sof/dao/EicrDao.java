package com.drajer.sof.dao;

import com.drajer.sof.model.Eicr;

public interface EicrDao {

	Eicr saveOrUpdate(Eicr eicr);
	
	Eicr getEicrById(Integer id);
}
