package com.drajer.ecrapp.config;

import java.util.Set;

import org.hl7.fhir.r4.model.ValueSet;

import com.drajer.ecrapp.util.ApplicationUtils;

public class ValueSetSingleton {

	private static ValueSetSingleton instance;
	
	private Set<ValueSet> covidValueSets;
	private Set<String>   covidValueSetsAsString;
	
	private Set<ValueSet> valueSets;

	public Set<String> getCovidValueSetsAsString() {
		return covidValueSetsAsString;
	}

	public void setCovidValueSetsAsString(Set<String> covidValueSetsAsString) {
		this.covidValueSetsAsString = covidValueSetsAsString;
	}

	public static void setInstance(ValueSetSingleton instance) {
		ValueSetSingleton.instance = instance;
	}

	public static ValueSetSingleton getInstance() {
		if (instance == null) {
			instance = new ValueSetSingleton();
		}
		return instance;
	}

	public Set<ValueSet> getCovidValueSets() {
		return covidValueSets;
	}

	public void setCovidValueSets(Set<ValueSet> covidValueSets) {
		this.covidValueSets = covidValueSets;
		
		this.covidValueSetsAsString = ApplicationUtils.convertValueSetsToString(covidValueSets);
	}

	public Set<ValueSet> getValueSets() {
		return valueSets;
	}

	public void setValueSets(Set<ValueSet> valueSets) {
		this.valueSets = valueSets;
	}
	
	private ValueSetSingleton() {
	}
	
	

}
