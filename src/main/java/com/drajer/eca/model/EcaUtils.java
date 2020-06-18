package com.drajer.eca.model;

import java.util.List;
import java.util.Set;

import org.apache.commons.collections4.SetUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;

public class EcaUtils {

	private static Logger logger = LoggerFactory.getLogger(EcaUtils.class);
	
	public static boolean matchTriggerCodesForDSTU2(List<ActionData> codePaths, Dstu2FhirData data, PatientExecutionState state, LaunchDetails details) {
		
		logger.info(" Start Matching Trigger Codes ");
		boolean matchfound = false;
		
		for(ActionData ad: codePaths) {
			
			logger.info(" Need to match Trigger Codes for : " + ad.getPath());
			
			List<CodeableConceptDt> ptCodes = data.getCodesForExpression(ad.getPath());
			
			if(ptCodes != null && ptCodes.size() > 0) {
				
				logger.info(" Found a Total # of " + ptCodes.size() + " codes found for Patient." + ad.getPath());
				
				Set<String> codesToMatch = ApplicationUtils.convertCodeableConceptsToString(ptCodes);
				Set<String> codesToMatchAgainst = null;
				
				if(details.getIsCovid()) {
										
					//codesToMatchAgainst = ValueSetSingleton.getInstance().getCovidValueSetsAsString();
					codesToMatchAgainst = ValueSetSingleton.getInstance().getCovidValueSetsAsStringForGrouper(ad.getPath());
					logger.info(" Total # of "+ codesToMatchAgainst.size() + " Codes in Trigger Code Value Set for matching for COVID-19");
					
				}
				else {
					
					// codesToMatchAgainst = ValueSetSingleton.getInstance().getValueSets();
					codesToMatchAgainst = ValueSetSingleton.getInstance().getValueSetsAsStringForGrouper(ad.getPath());
					logger.info(" Total # of "+ codesToMatchAgainst.size() + " Codes in Trigger Code Value Set for matching for Full EICR ");
				}
								
				Set<String> intersection = SetUtils.intersection(codesToMatch, codesToMatchAgainst);
				
				if(intersection != null && intersection.size() > 0) {
					
					logger.info(" Number of Matched Codes = " + intersection.size());
					
					// For Testing purposes until we get test data assume the data has matched and continue processing.
					state.getMatchTriggerStatus().setTriggerMatchStatus(true);
					matchfound = true;
					
					// Hardcoded value set and value set version for CONNECTATHON
					String valueSet = "2.16.840.1.113762.1.4.1146.1123";
					String valuesetVersion = "1";
					
					state.getMatchTriggerStatus().addMatchedCodes(intersection, valueSet, ad.getPath(), valuesetVersion);
					
				}
				else {
					
					logger.info(" No Matched codes found for : " + ad.getPath());
					state.getMatchTriggerStatus().setTriggerMatchStatus(false);
					matchfound = false;
				}
				
			}
		}
	
		logger.info(" End Matching Trigger Codes ");
		return matchfound;
	}
	
}
