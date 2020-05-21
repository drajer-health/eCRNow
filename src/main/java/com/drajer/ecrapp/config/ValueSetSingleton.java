package com.drajer.ecrapp.config;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.hl7.fhir.r4.model.ValueSet;

import com.drajer.ecrapp.util.ApplicationUtils;

public class ValueSetSingleton {

	private static ValueSetSingleton instance;
	
	private Set<ValueSet> covidValueSets;
	private Set<String>   covidValueSetsAsString;
	private Set<ValueSet> valueSets;
	private Set<ValueSet> grouperValueSets;
	
	private Map<String, Set<ValueSet>> triggerPathToValueSetsMap;
	
	private Map<String, ValueSet> triggerPathToGrouperMap;

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
		if(covidValueSets== null) {
			covidValueSets = new HashSet<>();
		}
		return covidValueSets;
	}

	public void setCovidValueSets(Set<ValueSet> covidValueSets) {
		this.covidValueSets = covidValueSets;
		this.covidValueSetsAsString = ApplicationUtils.convertValueSetsToString(covidValueSets);
	}

	public Set<ValueSet> getValueSets() {
		if(valueSets== null) {
			valueSets = new HashSet<>();
		}
		return valueSets;
	}

	public void setValueSets(Set<ValueSet> valueSets) {
		this.valueSets = valueSets;
	}
	

	public Set<ValueSet> getGrouperValueSets() {
		return grouperValueSets;
	}

	public void setGrouperValueSets(Set<ValueSet> grouperValueSets) {
		this.grouperValueSets = grouperValueSets;
	}

	public Map<String, Set<ValueSet>> getTriggerPathToValueSetsMap() {
		if(triggerPathToValueSetsMap== null) {
			triggerPathToValueSetsMap = new HashMap<>();
		}
		return triggerPathToValueSetsMap;
	}

	public void setTriggerPathToValueSetsMap(Map<String, Set<ValueSet>> triggerPathToValueSetsMap) {
		this.triggerPathToValueSetsMap = triggerPathToValueSetsMap;
	}

	public Map<String, ValueSet> getTriggerPathToGrouperMap() {
		if(triggerPathToGrouperMap== null) {
			triggerPathToGrouperMap = new HashMap<>();
		}
		return triggerPathToGrouperMap;
	}

	public void setTriggerPathToGrouperMap(Map<String, ValueSet> triggerPathToGrouperMap) {
		this.triggerPathToGrouperMap = triggerPathToGrouperMap;
	}

	private ValueSetSingleton() {
	}
}
