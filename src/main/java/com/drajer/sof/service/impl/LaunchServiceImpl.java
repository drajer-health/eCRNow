package com.drajer.sof.service.impl;

import com.drajer.sof.dao.LaunchDetailsDao;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.LaunchService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class LaunchServiceImpl implements LaunchService {

  @Autowired LaunchDetailsDao authDetailsDao;

  public LaunchDetails saveOrUpdate(LaunchDetails authDetails) {
    authDetailsDao.saveOrUpdate(authDetails);
    return authDetails;
  }

  public LaunchDetails getAuthDetailsById(Integer id) {
    return authDetailsDao.getAuthDetailsById(id);
  }

  public LaunchDetails getLaunchDetailsByPatientAndEncounter(
      String patient, String encounter, String fhirServerUrl) {
    return authDetailsDao.getLaunchDetailsByPatientAndEncounter(patient, encounter, fhirServerUrl);
  }

  public LaunchDetails getLaunchDetailsByState(int state) {
    return authDetailsDao.getLaunchDetailsByState(state);
  }

  @Override
  public void delete(LaunchDetails launchDetails) {
    authDetailsDao.delete(launchDetails);
  }
}
