package com.drajer.ecrapp.util;

import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.time.Instant;
import java.time.LocalDate;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.ValueSet;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionContainsComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.CreateEicrAction;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;

public class ApplicationUtils {
	
	private final static Logger logger = LoggerFactory.getLogger(ApplicationUtils.class);

	public static Boolean isCodePresent(Set<ValueSet> valuesets, String code) {
		Boolean isCodePresent = false;
		ValueSetExpansionComponent valueSetExpansionComponent;
		List<ValueSetExpansionContainsComponent> valueSetExpansionContainsComponentList;
		for(ValueSet valueset : valuesets) {
			valueSetExpansionComponent = valueset.getExpansion();
			valueSetExpansionContainsComponentList = valueSetExpansionComponent.getContains();
			for(ValueSetExpansionContainsComponent valueSetExpansionContainsComponent : valueSetExpansionContainsComponentList) {
				if(valueSetExpansionContainsComponent.getCode().equals(code)){
					isCodePresent = true;
					return isCodePresent;
				}
			}
		}
		return isCodePresent;
	}
	
	public static Set<String> convertValueSetsToString(Set<ValueSet> valuesets) { 
		
		Set<String> retVal = new HashSet<String>();
		ValueSetExpansionComponent valueSetExpansionComponent;
		List<ValueSetExpansionContainsComponent> valueSetExpansionContainsComponentList;
		
		if(valuesets != null && valuesets.size() > 0) {
			
			for(ValueSet vs : valuesets) {
				
				valueSetExpansionComponent = vs.getExpansion();
				valueSetExpansionContainsComponentList = valueSetExpansionComponent.getContains();
				
				for(ValueSetExpansionContainsComponent vscomp : valueSetExpansionContainsComponentList) {
					
					if(vscomp.getSystem() != null && 
					   vscomp.getCode() != null ) {
						
						retVal.add(vscomp.getSystem() + "|" + vscomp.getCode());
					}
				}
				
			}
		}
		
		return retVal;
	}
	
	public static Set<String> convertCodeableConceptsToString(List<CodeableConceptDt> codes) { 
		
		Set<String> retVal = new HashSet<String>();
		
		if(codes != null && codes.size() > 0) {
			
			for(CodeableConceptDt cd : codes) {
				
				List<CodingDt> cds = cd.getCoding();
							
				for(CodingDt code : cds) {
					
					if(code.getSystem() != null && 
					   code.getCode() != null ) {
						
						retVal.add(code.getSystem() + "|" + code.getCode());
					}
				}
				
			}
		}
		
		return retVal;
	}
	
	public static Instant convertDurationToInstant(Duration d) {
		
		Instant t = null;
		
		if(d != null) {
			
			if(d.getUnit().contentEquals("a")) {
				
				t = Instant.from(LocalDate.now().plusYears(d.getValue().longValue()));

			}
			else if(d.getUnit().contentEquals("mo")) {
				
				t = Instant.from(LocalDate.now().plusMonths(d.getValue().longValue()));
			}
			else if(d.getUnit().contentEquals("wk")) {
				t = Instant.from(LocalDate.now().plusWeeks(d.getValue().longValue()));
			}
			else if(d.getUnit().contentEquals("d")) {
				t = Instant.from(LocalDate.now().plusDays(d.getValue().longValue()));
			}
			else if(d.getUnit().contentEquals("h")) {
				
				t = new Date().toInstant().plusSeconds(d.getValue().longValue() * 60 * 60);
			}
			else if(d.getUnit().contentEquals("min")) {
			
				t = new Date().toInstant().plusSeconds(d.getValue().longValue() * 60);
			}
			else if(d.getUnit().contentEquals("s")) {
				
				t = new Date().toInstant().plusSeconds(d.getValue().longValue());
			}
			else {
				
				t = new Date().toInstant();
				
			}
		}
		
		
		return t; 
	}
	
	public static void saveDataToFile(String data, String fileName) {
		
		FileOutputStream fos;
		try {
			
			logger.error(" Writing eICR data to file: " + fileName);
			fos = new FileOutputStream(fileName);
			 DataOutputStream outStream = new DataOutputStream(new BufferedOutputStream(fos));
			 outStream.writeUTF(data);
			 outStream.close();
		} catch (IOException e) {
			
			logger.error(" Unable to write EICR to file: " + fileName);
			e.printStackTrace();
			
		}
	   
		
	}

}
