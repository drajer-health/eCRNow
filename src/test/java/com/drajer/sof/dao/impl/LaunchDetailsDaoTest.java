package com.drajer.sof.dao.impl;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestUtils;
import java.util.Date;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

@RunWith(SpringRunner.class)
@ContextConfiguration(classes = SpringConfiguration.class)
@AutoConfigureTestDatabase
@SpringBootTest
@ActiveProfiles("test")
@Transactional
public class LaunchDetailsDaoTest {

  @Autowired private LaunchDetailsDaoImpl launchDetailsDaoImpl;

  private static LaunchDetails expectedLaunchDetails;

  @Before
  public void setUp() {
    expectedLaunchDetails =
        (LaunchDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/LaunchDetails/LaunchDetails.json", LaunchDetails.class);
    expectedLaunchDetails.setLastUpdated(new Date());
  }

  @Test
  public void saveLaunchDetails() {
    LaunchDetails actualLaunchDetails = launchDetailsDaoImpl.saveOrUpdate(expectedLaunchDetails);

    assertNotNull(actualLaunchDetails);
    assertLaunchDetails(expectedLaunchDetails, actualLaunchDetails);
  }

  @Test
  public void getAuthDetailsById() {
    LaunchDetails savedLaunchDetails = launchDetailsDaoImpl.saveOrUpdate(expectedLaunchDetails);
    LaunchDetails actualLaunchDetails =
        launchDetailsDaoImpl.getAuthDetailsById(savedLaunchDetails.getId());

    assertNotNull(actualLaunchDetails);
    assertLaunchDetails(expectedLaunchDetails, actualLaunchDetails);
  }

  @Test
  public void getLaunchDetailsByPatientAndEncounter() {
    launchDetailsDaoImpl.saveOrUpdate(expectedLaunchDetails);
    String patientID = expectedLaunchDetails.getLaunchPatientId();
    String encounterID = expectedLaunchDetails.getEncounterId();
    String fhirServerUrl = expectedLaunchDetails.getEhrServerURL();
    LaunchDetails actualLaunchDetails =
        launchDetailsDaoImpl.getLaunchDetailsByPatientAndEncounter(
            patientID, encounterID, fhirServerUrl);

    assertNotNull(actualLaunchDetails);
    assertLaunchDetails(expectedLaunchDetails, actualLaunchDetails);
  }

  @Test
  public void deleteLaunchDetails() {
    LaunchDetails savedLaunchDetails = launchDetailsDaoImpl.saveOrUpdate(expectedLaunchDetails);

    String patientID = savedLaunchDetails.getLaunchPatientId();
    String encounterID = savedLaunchDetails.getEncounterId();
    String fhirServerUrl = savedLaunchDetails.getEhrServerURL();

    launchDetailsDaoImpl.delete(savedLaunchDetails);
    LaunchDetails retrievedLaunchDetails =
        launchDetailsDaoImpl.getLaunchDetailsByPatientAndEncounter(
            patientID, encounterID, fhirServerUrl);
    assertNull(retrievedLaunchDetails);
  }

  @Test
  public void getLaunchDetailsByState() {
    LaunchDetails savedLaunchDetails = launchDetailsDaoImpl.saveOrUpdate(expectedLaunchDetails);
    LaunchDetails actualLaunchDetails =
        launchDetailsDaoImpl.getLaunchDetailsByState(savedLaunchDetails.getLaunchState());

    assertNotNull(actualLaunchDetails);
    assertLaunchDetails(expectedLaunchDetails, actualLaunchDetails);
  }

  public void assertLaunchDetails(LaunchDetails expected, LaunchDetails actual) {
    assertEquals(expected.getClientId(), actual.getClientId());
    assertEquals(expected.getEhrServerURL(), actual.getEhrServerURL());
    assertEquals(expected.getLaunchPatientId(), actual.getLaunchPatientId());
    assertEquals(expected.getEncounterId(), actual.getEncounterId());
    assertEquals(expected.getAccessToken(), actual.getAccessToken());
    assertEquals(expected.getAssigningAuthorityId(), actual.getAssigningAuthorityId());
    assertEquals(expected.getAuthorizationCode(), actual.getAuthorizationCode());
    assertEquals(expected.getAuthUrl(), actual.getAuthUrl());
    assertEquals(expected.getDirectHost(), actual.getDirectHost());
    assertEquals(expected.getDirectRecipient(), actual.getDirectRecipient());
    assertEquals(expected.getDirectUser(), actual.getDirectUser());
    assertEquals(expected.getFhirVersion(), actual.getFhirVersion());
    assertEquals(expected.getImapPort(), actual.getImapPort());
    assertEquals(expected.getScope(), actual.getScope());
    assertEquals(expected.getRedirectURI(), actual.getRedirectURI());
    assertEquals(expected.getRefreshToken(), actual.getRefreshToken());
    assertEquals(expected.getValidationMode(), actual.getValidationMode());
    assertEquals(expected.getRestAPIURL(), actual.getRestAPIURL());
    assertEquals(expected.getSmtpPort(), actual.getSmtpPort());
    assertEquals(expected.getStatus(), actual.getStatus());
    assertEquals(expected.getTokenUrl(), actual.getTokenUrl());
    assertEquals(expected.getUserId(), actual.getUserId());
    assertEquals(expected.getVersionNumber(), actual.getVersionNumber());
    assertEquals(expected.getxRequestId(), actual.getxRequestId());
    assertEquals(expected.getxRequestId(), actual.getxRequestId());
    assertEquals(expected.getDebugFhirQueryAndEicr(), actual.getDebugFhirQueryAndEicr());
    assertEquals(expected.getExpiry(), actual.getExpiry());
    assertEquals(expected.getIsCovid(), actual.getIsCovid());
    assertEquals(expected.getIsSystem(), actual.getIsSystem());
    assertEquals(expected.getLaunchState(), actual.getLaunchState());
    assertEquals(expected.getSetId(), actual.getSetId());
  }
}
