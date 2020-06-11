package com.drajer.sof.service;

import java.util.List;

import com.drajer.sof.model.LaunchDetails;

public interface LaunchService {

	LaunchDetails saveOrUpdate(LaunchDetails authDetails);
	
	LaunchDetails getAuthDetailsById(Integer id);
	
	LaunchDetails getLaunchDetailsByPatientAndEncounter(String patient, String encounter,String fhirServerUrl);
	
	List<LaunchDetails> getAllLaunchDetails();
}
