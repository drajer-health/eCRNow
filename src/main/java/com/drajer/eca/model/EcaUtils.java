package com.drajer.eca.model;

import java.util.List;
import java.util.Set;

import org.apache.commons.collections4.SetUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.cdafromR4.CdaEicrGeneratorFromR4;
import com.drajer.cdafromdstu2.CdaEicrGenerator;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;

public class EcaUtils {

	private static Logger logger = LoggerFactory.getLogger(EcaUtils.class);
	
	public static boolean matchTriggerCodesForDSTU2(List<ActionData> codePaths, Dstu2FhirData data, PatientExecutionState state, LaunchDetails details) {
		
		logger.info(" Start Matching Trigger Codes ");
		boolean matchfound = false;
		state.getMatchTriggerStatus().setTriggerMatchStatus(false);
		
		for(ActionData ad: codePaths) {
			
			logger.info(" Need to match Trigger Codes for : {}" , ad.getPath());
			
			List<CodeableConceptDt> ptCodes = data.getCodesForExpression(ad.getPath());
			
			if(ptCodes != null && ptCodes.size() > 0) {
				
				logger.info(" Found a Total # of {} codes found for Patient {}", ptCodes.size(), ad.getPath());
				
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
			
			logger.info(" Need to match Trigger Codes for :{} " , ad.getPath());
			
			List<CodeableConcept> ptCodes = data.getR4CodesForExpression(ad.getPath());
			
			if(ptCodes != null && ptCodes.size() > 0) {
				
				logger.info(" Found a Total # of {} codes found for Patient." , ad.getPath());
				
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
			logger.info(" Total # of {} Codes in Trigger Code Value Set for matching for COVID-19",codesToMatchAgainst.size());
			
		}
		else {
			
			codesToMatchAgainst = ValueSetSingleton.getInstance().getValueSetsAsStringForGrouper(ad.getPath());
			logger.info(" Total # of {} Codes in Trigger Code Value Set for matching for Full EICR ",codesToMatchAgainst.size());
		}
						
		Set<String> intersection = SetUtils.intersection(codesToMatch, codesToMatchAgainst);
		
		if(intersection != null && intersection.size() > 0) {
			
			logger.info(" Number of Matched Codes = {}" , intersection.size());
			
			state.getMatchTriggerStatus().setTriggerMatchStatus(true);
			matchfound = true;
			
			// Hardcoded value set and value set version for CONNECTATHON
			String valueSet = "2.16.840.1.113762.1.4.1146.1123";
			String valuesetVersion = "1";
			
			state.getMatchTriggerStatus().addMatchedCodes(intersection, valueSet, ad.getPath(), valuesetVersion);
			
		}
		else {
			
			logger.info(" No Matched codes found for : {}" , ad.getPath());
		}
	}

	public static Eicr createEicr(LaunchDetails details){
		
		Eicr ecr = null;		
		
		if (ActionRepo.getInstance().getLoadingQueryService() != null) {

			logger.info(" Getting necessary data from Loading Queries ");
			FhirData data = ActionRepo.getInstance().getLoadingQueryService().getData(details, details.getStartDate(), details.getEndDate());

			String eICR = null;
			
			if (data instanceof Dstu2FhirData) {

				logger.info("Creating eICR based on FHIR DSTU2 ");
				Dstu2FhirData dstu2Data = (Dstu2FhirData) data;
				eICR = CdaEicrGenerator.convertDstu2FhirBundletoCdaEicr(dstu2Data, details);
				
			}
			else if(data instanceof R4FhirData) {
				
				logger.info("Creating eICR based on FHIR R4 ");
				R4FhirData r4Data = (R4FhirData) data;
				eICR = CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(r4Data, details);

			}
			else {

				String msg = "No Fhir Data retrieved to CREATE EICR.";
				logger.error(msg);

				throw new RuntimeException(msg);
			}

			if(eICR != null) {
				// Create the object for persistence.
				ecr = new Eicr();
				ecr.setData(eICR);
				ActionRepo.getInstance().getEicrRRService().saveOrUpdate(ecr);
				
			}
		}
		else {

			String msg = "System Startup Issue, Spring Injection not functioning properly, loading service is null.";
			logger.error(msg);

			throw new RuntimeException(msg);
		}
		
		return ecr;
	}

	public static void updateDetailStatus(LaunchDetails details, PatientExecutionState state)
	{
		ObjectMapper mapper = new ObjectMapper();
		
		try {
			
			details.setStatus(mapper.writeValueAsString(state));
			
		} catch (JsonProcessingException e) {

			String msg = "Unable to update execution state";
			logger.error(msg);
			e.printStackTrace();

			throw new RuntimeException(msg);
		}
	}

	public static PatientExecutionState getDetailStatus(LaunchDetails details){
		
		ObjectMapper mapper = new ObjectMapper();
		PatientExecutionState state = null;
		
		try {
			
			state = mapper.readValue(details.getStatus(), PatientExecutionState.class);
		
		} catch (JsonMappingException e1) {
			
			String msg = "Unable to read/write execution state";
			logger.error(msg);
			e1.printStackTrace();
			throw new RuntimeException(msg);
			
		} catch (JsonProcessingException e1) {
			String msg = "Unable to read/write execution state";
			logger.error(msg);
			e1.printStackTrace();
			
			throw new RuntimeException(msg);
		}
		
		return state;
	
	}
}
