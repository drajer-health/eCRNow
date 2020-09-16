package com.drajer.sof.dao.impl;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
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

  ObjectMapper mapper = new ObjectMapper();

  @Test
  public void saveLaunchDetails() throws JsonParseException, JsonMappingException, IOException {
    LaunchDetails launchDetails =
        mapper.readValue(
            this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"),
            LaunchDetails.class);

    LaunchDetails savedLaunchDetails = launchDetailsDaoImpl.saveOrUpdate(launchDetails);

    assertEquals(launchDetails.getClientId(), savedLaunchDetails.getClientId());
    assertEquals(launchDetails.getUserId(), savedLaunchDetails.getUserId());
    assertEquals(launchDetails.getEhrServerURL(), savedLaunchDetails.getEhrServerURL());
    assertEquals(launchDetails.getScope(), savedLaunchDetails.getScope());
  }

  @Test
  public void getAuthDetailsById() throws JsonParseException, JsonMappingException, IOException {
    LaunchDetails launchDetails =
        mapper.readValue(
            this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"),
            LaunchDetails.class);

    LaunchDetails savedLaunchDetails = launchDetailsDaoImpl.saveOrUpdate(launchDetails);
    LaunchDetails retrievedLaunchDetails =
        launchDetailsDaoImpl.getAuthDetailsById(savedLaunchDetails.getId());

    assertNotNull(retrievedLaunchDetails);
  }

  @Test
  public void getLaunchDetailsByPatientAndEncounter()
      throws JsonParseException, JsonMappingException, IOException {
    LaunchDetails launchDetails =
        mapper.readValue(
            this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"),
            LaunchDetails.class);

    LaunchDetails savedLaunchDetails = launchDetailsDaoImpl.saveOrUpdate(launchDetails);
    String patientID = savedLaunchDetails.getLaunchPatientId();
    String encounterID = savedLaunchDetails.getEncounterId();
    String fhirServerUrl = savedLaunchDetails.getEhrServerURL();
    LaunchDetails retrievedLaunchDetails =
        launchDetailsDaoImpl.getLaunchDetailsByPatientAndEncounter(
            patientID, encounterID, fhirServerUrl);

    assertNotNull(retrievedLaunchDetails);
  }

  @Test
  public void getLaunchDetailsByState()
      throws JsonParseException, JsonMappingException, IOException {
    LaunchDetails launchDetails =
        mapper.readValue(
            this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"),
            LaunchDetails.class);

    LaunchDetails savedLaunchDetails = launchDetailsDaoImpl.saveOrUpdate(launchDetails);

    LaunchDetails retrievedLaunchDetails =
        launchDetailsDaoImpl.getLaunchDetailsByState(savedLaunchDetails.getLaunchState());

    assertNotNull(retrievedLaunchDetails);
  }
}
