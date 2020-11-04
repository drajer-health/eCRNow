package com.drajer.sof.service;

import com.drajer.sof.model.LaunchDetails;

public interface LaunchService {

  LaunchDetails saveOrUpdate(LaunchDetails authDetails);

  LaunchDetails getAuthDetailsById(Integer id);

  LaunchDetails getLaunchDetailsByPatientAndEncounter(
      String patient, String encounter, String fhirServerUrl);

  LaunchDetails getLaunchDetailsByState(int state);

  void delete(LaunchDetails launchDetails);
}
