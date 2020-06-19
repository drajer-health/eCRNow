package com.drajer.cdafromR4;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;

public class CdaBodyGenerator {

private static final Logger logger = LoggerFactory.getLogger(CdaBodyGenerator.class);
	
	public static String generateCdaBody(R4FhirData data, LaunchDetails details) {
		
		StringBuilder eICR = new StringBuilder();
		
		return eICR.toString();
	}
	
}
