package com.drajer.cda;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;

import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Patient;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;

public class CdaBodyGenerator {

	private static final Logger logger = LoggerFactory.getLogger(CdaBodyGenerator.class);
	
	public static String generateCdaBody(Dstu2FhirData data, LaunchDetails details) {
		
		StringBuilder eICRBody = new StringBuilder(200);
		
		eICRBody.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
		eICRBody.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.STRUC_BODY_EL_NAME));
	
		if(data != null) {
			
			eICRBody.append(CdaProblemGenerator.generateProblemSection(data,details));
			
		}
		
		eICRBody.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.STRUC_BODY_EL_NAME));
		eICRBody.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
		
		return eICRBody.toString();
		
	}
	
	
}
