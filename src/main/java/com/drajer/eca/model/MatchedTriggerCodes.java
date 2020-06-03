package com.drajer.eca.model;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.collections4.SetUtils;

public class MatchedTriggerCodes {
	
	private Set<String>				matchedCodes;
	private String 					valueSet;
	private String 					valueSetVersion;
	private String 					matchedPath;
	
	public MatchedTriggerCodes() {
		matchedCodes = new HashSet<String>();
	}
	
	public Set<String> getMatchedCodes() {
		return matchedCodes;
	}
	public void setMatchedCodes(Set<String> matchedCodes) {
		this.matchedCodes = matchedCodes;
	}
	public String getValueSet() {
		return valueSet;
	}
	public void setValueSet(String valueSet) {
		this.valueSet = valueSet;
	}
	public String getValueSetVersion() {
		return valueSetVersion;
	}
	public void setValueSetVersion(String valueSetVersion) {
		this.valueSetVersion = valueSetVersion;
	}
	public String getMatchedPath() {
		return matchedPath;
	}
	public void setMatchedPath(String matchedPath) {
		this.matchedPath = matchedPath;
	}
	
	public Boolean hasMatchedTriggerCodes(String type) {
		
		if(matchedCodes != null && matchedCodes.size() > 0 &&
		   matchedPath != null && matchedPath.contains(type))
			return true;
		else
			return false;
		
	}
	
	public void addCodes(Set<String> codes) {
		
		if(matchedCodes == null)
			matchedCodes = new HashSet<String>();
		
		if(codes != null) {
			Set<String> union = SetUtils.union(matchedCodes, codes);
			
			matchedCodes = union;
		}
			
	}

}
