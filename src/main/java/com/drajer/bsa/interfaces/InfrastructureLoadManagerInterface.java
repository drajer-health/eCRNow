package com.drajer.bsa.interfaces;

import com.drajer.bsa.model.BsaTypes.BsaJobType;

public interface InfrastructureLoadManagerInterface {
	
	public Boolean canExecuteJob(String throttleContext, BsaJobType type);

}
