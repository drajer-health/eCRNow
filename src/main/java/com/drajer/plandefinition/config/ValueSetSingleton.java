package com.drajer.plandefinition.config;

import java.util.Set;

import org.hl7.fhir.r4.model.ValueSet;

public class ValueSetSingleton {

	private static ValueSetSingleton instance;
	private Set<ValueSet> covidValueSets;
	private Set<ValueSet> valueSets;

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
	}

	public Set<ValueSet> getValueSets() {
		return valueSets;
	}

	public void setValueSets(Set<ValueSet> valueSets) {
		this.valueSets = valueSets;
	}

}
