package com.drajer.ecrapp.util;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.hl7.fhir.r4.model.ValueSet;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionContainsComponent;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;

public class ApplicationUtils {
	
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

}
