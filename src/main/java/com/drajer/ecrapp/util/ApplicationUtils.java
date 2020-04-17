package com.drajer.ecrapp.util;

import java.util.List;
import java.util.Set;

import org.hl7.fhir.r4.model.ValueSet;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionContainsComponent;

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

}
