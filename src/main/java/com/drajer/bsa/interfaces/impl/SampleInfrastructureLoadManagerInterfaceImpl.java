package com.drajer.bsa.interfaces.impl;

import javax.transaction.Transactional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.drajer.bsa.interfaces.InfrastructureLoadManagerInterface;
import com.drajer.bsa.model.BsaTypes.BsaJobType;

@Service
@Transactional
public class SampleInfrastructureLoadManagerInterfaceImpl implements InfrastructureLoadManagerInterface {

	private final Logger logger = LoggerFactory.getLogger(SampleInfrastructureLoadManagerInterfaceImpl.class);
	
	@Override
	public Boolean canExecuteJob(String throttleContext, BsaJobType type) {
		
		logger.info(" Permission requested for Job Type {} to be run with {} context", type, throttleContext);
		
		// Always return true by default.
		return true;
	}

}
