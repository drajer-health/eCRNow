package com.drajer.ecrapp.config;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.hl7.fhir.r4.model.ValueSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.ecrapp.util.ApplicationUtils;

public class ValueSetSingleton {

	private static ValueSetSingleton instance;
	
	private final Logger logger = LoggerFactory.getLogger(ValueSetSingleton.class);
	
	private Set<ValueSet> covidValueSets;
	private Set<String>   covidValueSetsAsString;
	private Set<ValueSet> valueSets;
	private Set<ValueSet> grouperValueSets;
	
	private Map<String, Set<ValueSet>> triggerPathToValueSetsMap;	
	private Map<String, ValueSet> 	   triggerPathToGrouperMap;
	private Map<String, Set<ValueSet>> grouperToValueSetMap;
	private Map<String, Set<ValueSet>> grouperToCovidValueSetMap;
	
	public void print() {
		
		logger.info(" *** Start Printing Value Set Singleton *** ");
		
		logger.info(" *** Start Printing Grouper To Value Sets **** " + grouperToValueSetMap.size());
		
		for (Map.Entry<String,Set<ValueSet>> entry : grouperToValueSetMap.entrySet()) {
			
			logger.info(" Key = " + entry.getKey());
			
			Set<ValueSet> vs = entry.getValue();
			
			if(vs != null) {
			for( ValueSet v : vs) { 
				
				logger.info(" Value Set = " + v.getId() + v.getUrl());
				
			}
			}
		}
		
		logger.info(" *** End Printing Grouper To Value Sets **** ");
		
		
		
		if(grouperToCovidValueSetMap != null) {
			
			logger.info(" *** Start Printing Grouper To Covid Value Sets **** " + grouperToCovidValueSetMap.size() );
		
		for (Map.Entry<String,Set<ValueSet>> coventry : grouperToCovidValueSetMap.entrySet()) {
			
			logger.info(" Key = " + coventry.getKey());
			
			Set<ValueSet> vs = coventry.getValue();
			
			if(vs != null && !vs.isEmpty()) {
				
			for( ValueSet v : vs) { 
				
				logger.info(" Value Set = " + v.getId() + v.getUrl());
				
			}
			
			}
		}
		
		}
		
		logger.info(" *** Start Printing Grouper To Covid Value Sets **** ");
		
		logger.info(" *** End Printing Value Set Singleton *** ");
		
	}

	public Map<String, Set<ValueSet>> getGrouperToValueSetMap() {
		return grouperToValueSetMap;
	}

	public void setGrouperToValueSetMap(Map<String, Set<ValueSet>> grouperToValueSetMap) {
		this.grouperToValueSetMap = grouperToValueSetMap;
	}

	public Map<String, Set<ValueSet>> getGrouperToCovidValueSetMap() {
		return grouperToCovidValueSetMap;
	}

	public void setGrouperToCovidValueSetMap(Map<String, Set<ValueSet>> grouperToCovidValueSetMap) {
		this.grouperToCovidValueSetMap = grouperToCovidValueSetMap;
	}

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
	
	public void addGrouperToValueSetMap(String grouper, Set<ValueSet> vs) {
		
		if(grouperToValueSetMap != null) {
			
			if(grouperToValueSetMap.containsKey(grouper)) {
			
				logger.info(" Adding Value sets for Grouper  " + grouper + " Size = " + vs.size());
				grouperToValueSetMap.get(grouper).addAll(vs);
				
			}
			else {
				
				logger.info(" Creating new entry and adding value sets for Grouper  " + grouper + " Size = " + vs.size());
				
				grouperToValueSetMap.put(grouper, vs);
				
			}
			
		}
		else {
			
			logger.info(" Creating new entry and adding value sets for Grouper  " + grouper + " Size = " + vs.size());
			
			grouperToValueSetMap = new HashMap<String, Set<ValueSet>>();
			
			grouperToValueSetMap.put(grouper, vs);
			
		}
	}
	
	public void addGrouperToCovidValueSetMap(String grouper, Set<ValueSet> vs) {
		
		if(grouperToCovidValueSetMap != null) {
			
			if(grouperToCovidValueSetMap.containsKey(grouper)) {
			
				logger.info(" Adding Covid Value sets for Grouper  " + grouper + " Size = " + vs.size());
				
				grouperToCovidValueSetMap.get(grouper).addAll(vs);
				
			}
			else {
				
				logger.info(" Creating new entry and adding Covid value sets for Grouper  " + grouper + " Size = " + vs.size());
				
				grouperToCovidValueSetMap.put(grouper, vs);
				
			}
			
		}
		else {
			
			logger.info(" Creating new entry and adding Covid Value sets for Grouper  " + grouper + " Size = " + vs.size());
			
			grouperToCovidValueSetMap = new HashMap<String, Set<ValueSet>>();
			
			grouperToCovidValueSetMap.put(grouper, vs);
			
		}
	}
	
	public Set<String> getValueSetsAsStringForGrouper(String path) {
		String grouperId = null;
		Set<String> retVal = new HashSet<String>();
		ValueSet grouperValueSet = getTriggerPathToGrouperMap().get(path);
		if (grouperValueSet != null) {
			grouperId = grouperValueSet.getId();
		}
		if (grouperId != null && getGrouperToValueSetMap() != null) {
			Set<ValueSet> valuesets = getGrouperToValueSetMap().get(grouperId);
			retVal = ApplicationUtils.convertValueSetsToString(valuesets);
		}
		return retVal;
	}
	
	public Set<String> getCovidValueSetsAsStringForGrouper(String path) {
		String grouperId = null;
		Set<String> retVal = new HashSet<String>();
		ValueSet grouperValueSet = getTriggerPathToGrouperMap().get(path);
		if (grouperValueSet != null) {
			grouperId = grouperValueSet.getId();
		}
		if (grouperId != null && getGrouperToCovidValueSetMap() != null) {
			Set<ValueSet> valuesets = getGrouperToCovidValueSetMap().get(grouperId);
			retVal = ApplicationUtils.convertValueSetsToString(valuesets);
		}
		return retVal;
	}

	private ValueSetSingleton() {
	}
}
