package com.drajer.eca.model;

import java.util.List;
import java.util.Set;

import org.apache.commons.collections4.SetUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;

public class EcaUtils {

	private static Logger logger = LoggerFactory.getLogger(EcaUtils.class);
	
	public static boolean matchTriggerCodesForDSTU2(List<ActionData> codePaths, Dstu2FhirData data, PatientExecutionState state, LaunchDetails details) {
		
		logger.info(" Start Matching Trigger Codes ");
		boolean matchfound = false;
		state.getMatchTriggerStatus().setTriggerMatchStatus(false);
		
		for(ActionData ad: codePaths) {
			
			logger.info(" Need to match Trigger Codes for : " + ad.getPath());
			
			List<CodeableConceptDt> ptCodes = data.getCodesForExpression(ad.getPath());
			
			if(ptCodes != null && ptCodes.size() > 0) {
				
				logger.info(" Found a Total # of " + ptCodes.size() + " codes found for Patient." + ad.getPath());
				
				Set<String> codesToMatch = ApplicationUtils.convertCodeableConceptsToString(ptCodes);
				matchTriggerCodes(details,ad,codesToMatch,state,matchfound);
			}
		}
	
		logger.info(" End Matching Trigger Codes ");
		return matchfound;
	}
	
	public static boolean matchTriggerCodesForR4(List<ActionData> codePaths, R4FhirData data, PatientExecutionState state, LaunchDetails details) {
		
		logger.info(" Start Matching Trigger Codes ");
		boolean matchfound = false;
		state.getMatchTriggerStatus().setTriggerMatchStatus(false);
		
		for(ActionData ad: codePaths) {
			
			logger.info(" Need to match Trigger Codes for : " + ad.getPath());
			
			List<CodeableConcept> ptCodes = data.getR4CodesForExpression(ad.getPath());
			
			if(ptCodes != null && ptCodes.size() > 0) {
				
				logger.info(" Found a Total # of " + ptCodes.size() + " codes found for Patient." + ad.getPath());
				
				Set<String> codesToMatch = ApplicationUtils.convertR4CodeableConceptsToString(ptCodes);
				matchTriggerCodes(details,ad,codesToMatch,state,matchfound);
			}
		}
	
		logger.info(" End Matching Trigger Codes ");
		return matchfound;
	}
	
	public static void matchTriggerCodes(LaunchDetails details,ActionData ad,Set<String> codesToMatch,PatientExecutionState state,boolean matchfound) {
		Set<String> codesToMatchAgainst = null;
		
		if(details.getIsCovid()) {
								
			codesToMatchAgainst = ValueSetSingleton.getInstance().getCovidValueSetsAsStringForGrouper(ad.getPath());
			logger.info(" Total # of "+ codesToMatchAgainst.size() + " Codes in Trigger Code Value Set for matching for COVID-19");
			
		}
		else {
			
			codesToMatchAgainst = ValueSetSingleton.getInstance().getValueSetsAsStringForGrouper(ad.getPath());
			logger.info(" Total # of "+ codesToMatchAgainst.size() + " Codes in Trigger Code Value Set for matching for Full EICR ");
		}
						
		Set<String> intersection = SetUtils.intersection(codesToMatch, codesToMatchAgainst);
		
		if(intersection != null && intersection.size() > 0) {
			
			logger.info(" Number of Matched Codes = " + intersection.size());
			
			state.getMatchTriggerStatus().setTriggerMatchStatus(true);
			matchfound = true;
			
			// Hardcoded value set and value set version for CONNECTATHON
			String valueSet = "2.16.840.1.113762.1.4.1146.1123";
			String valuesetVersion = "1";
			
			state.getMatchTriggerStatus().addMatchedCodes(intersection, valueSet, ad.getPath(), valuesetVersion);
			
		}
		else {
			
			logger.info(" No Matched codes found for : " + ad.getPath());
		}
	}
	
}
