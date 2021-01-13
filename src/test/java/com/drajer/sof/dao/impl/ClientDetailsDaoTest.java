package com.drajer.sof.dao.impl;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.sof.model.ClientDetails;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import java.io.IOException;
import java.util.List;
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
public class ClientDetailsDaoTest {

  @Autowired private ClientDetailsDaoImpl clientDetailsDao;

  private static ClientDetails clientDetails = null;
  private static ClientDetails clientDetails2 = null;

  @Before
  public void setUp() {
    clientDetails =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDataEntry1.json", ClientDetails.class);
    clientDetails2 =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDataEntry2.json", ClientDetails.class);
  }

  @Test
  public void saveClientDetails() throws JsonParseException, JsonMappingException, IOException {

    ClientDetails savedClientDetails = clientDetailsDao.saveOrUpdate(clientDetails);

    assertEquals(clientDetails.getClientId(), savedClientDetails.getClientId());
    assertEquals(
        clientDetails.getDirectRecipientAddress(), savedClientDetails.getDirectRecipientAddress());
    assertEquals(clientDetails.getFhirServerBaseURL(), savedClientDetails.getFhirServerBaseURL());
    assertEquals(clientDetails.getScopes(), savedClientDetails.getScopes());
  }

  @Test
  public void getClientDetailsById() throws JsonParseException, JsonMappingException, IOException {

    ClientDetails savedClientDetails = clientDetailsDao.saveOrUpdate(clientDetails);

    ClientDetails retrievedClientDetails =
        clientDetailsDao.getClientDetailsById(savedClientDetails.getId());

    assertNotNull(retrievedClientDetails);
  }

  @Test
  public void getClientDetailsByUrl() throws JsonParseException, JsonMappingException, IOException {

    String fhirServerBaseURL = clientDetails.getFhirServerBaseURL();

    clientDetailsDao.saveOrUpdate(clientDetails);

    ClientDetails savedClientDetails = clientDetailsDao.getClientDetailsByUrl(fhirServerBaseURL);

    assertNotNull(savedClientDetails);
  }

  @Test
  public void getAllClientDetails() throws JsonParseException, JsonMappingException, IOException {

    clientDetailsDao.saveOrUpdate(clientDetails);
    clientDetailsDao.saveOrUpdate(clientDetails2);

    List<ClientDetails> savedClientDetailsList = clientDetailsDao.getAllClientDetails();

    assertEquals(savedClientDetailsList.size(), 2);
  }
}
