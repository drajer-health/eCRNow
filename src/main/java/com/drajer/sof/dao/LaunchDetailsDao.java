package com.drajer.sof.dao;

import com.drajer.sof.model.LaunchDetails;

public interface LaunchDetailsDao {

  LaunchDetails saveOrUpdate(LaunchDetails authDetails);

  LaunchDetails getAuthDetailsById(Integer id);

  LaunchDetails getLaunchDetailsByPatientAndEncounter(
      String patient, String encounter, String fhirServerUrl);

  LaunchDetails getLaunchDetailsByState(int state);

  void delete(LaunchDetails launchDetails);
}
