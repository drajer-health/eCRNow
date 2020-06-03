package com.drajer.ecrapp.util;

import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.time.Instant;
import java.time.LocalDate;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.UsageContext;
import org.hl7.fhir.r4.model.ValueSet;
import org.hl7.fhir.r4.model.ValueSet.ConceptSetComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionContainsComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.ecrapp.config.ValueSetSingleton;

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
	
	public static List<CanonicalType> getValueSetListFromGrouper(String grouperId){
		List<CanonicalType> valueSetIdList = null;
		for(ValueSet valueset : ValueSetSingleton.getInstance().getGrouperValueSets()) {
			if(valueset.getId().equals(grouperId)) {
				if(valueset.getCompose()!=null && valueset.getCompose().getInclude()!=null) {
					List<ConceptSetComponent> csc = valueset.getCompose().getInclude();
					for (ConceptSetComponent conceptSetComponent : csc) {
						valueSetIdList = conceptSetComponent.getValueSet();
					}
				}
			}
		}
		return valueSetIdList;
	}
	
	public static ValueSet getValueSetGrouperFromId(String grouperId){
		ValueSet valueSetGrouper= null;
		for(ValueSet valueset : ValueSetSingleton.getInstance().getGrouperValueSets()) {
			if(valueset.getId().equals(grouperId)) {
				valueSetGrouper = valueset;
			}
		}
		return valueSetGrouper;
	}
	
	public static Set<ValueSet> getValueSetByIds(List<CanonicalType> valueSetIdList) {
		Set<ValueSet> valueSets = new HashSet<>();
		if (Optional.ofNullable(valueSetIdList).isPresent()) {
			for(CanonicalType canonicalType : valueSetIdList) {
				for(ValueSet valueSet : ValueSetSingleton.getInstance().getValueSets()) {
					if(valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())){
						valueSets.add(valueSet);
					}
				}
				for(ValueSet valueSet : ValueSetSingleton.getInstance().getCovidValueSets()) {
					if(valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())){
						valueSets.add(valueSet);
					}
				}
			}
		}
		return valueSets;
	}
	
	public static Set<ValueSet> getCovidValueSetByIds(List<CanonicalType> valueSetIdList) {
		
		Set<ValueSet> valueSets = new HashSet<>();
		
		if (Optional.ofNullable(valueSetIdList).isPresent()) {
			for(CanonicalType canonicalType : valueSetIdList) {
				
				for(ValueSet valueSet : ValueSetSingleton.getInstance().getValueSets()) {
					
					if(valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())){
						
					
					List<UsageContext> usageContextList = valueSet.getUseContext();
					
					if (!usageContextList.isEmpty()) {
						
							for (UsageContext usageContext : usageContextList) {
								
								if (Optional.ofNullable(usageContext).isPresent()) {
									
									if (usageContext.getValueCodeableConcept() != null && usageContext
											.getValueCodeableConcept().getText().equalsIgnoreCase("COVID-19")) {
										
										logger.info("Found Grouper to COVID Value Set Mapping, ValueSet Id = : " 
											        + valueSet.getId());
										
										valueSets.add(valueSet);
									}
					
								}
							}
					}
					}
				}
			}
		}
		
		return valueSets;
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
			outStream.writeBytes(data);
			outStream.close();
		} catch (IOException e) {
			
			logger.error(" Unable to write EICR to file: " + fileName);
			e.printStackTrace();
			
		}
	   
		
	}

}
