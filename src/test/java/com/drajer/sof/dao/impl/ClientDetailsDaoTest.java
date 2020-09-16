package com.drajer.sof.dao.impl;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.sof.model.ClientDetails;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.List;
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

  ObjectMapper mapper = new ObjectMapper();

  @Test
  public void saveClientDetails() throws JsonParseException, JsonMappingException, IOException {
    ClientDetails clientDetails =
        mapper.readValue(
            this.getClass().getClassLoader().getResourceAsStream("clientDetails.json"),
            ClientDetails.class);

    ClientDetails savedClientDetails = clientDetailsDao.saveOrUpdate(clientDetails);

    assertEquals(clientDetails.getClientId(), savedClientDetails.getClientId());
    assertEquals(
        clientDetails.getDirectRecipientAddress(), savedClientDetails.getDirectRecipientAddress());
    assertEquals(clientDetails.getFhirServerBaseURL(), savedClientDetails.getFhirServerBaseURL());
    assertEquals(clientDetails.getScopes(), savedClientDetails.getScopes());
  }

  @Test
  public void getClientDetailsById() throws JsonParseException, JsonMappingException, IOException {
    ClientDetails clientDetails =
        mapper.readValue(
            this.getClass().getClassLoader().getResourceAsStream("clientDetails.json"),
            ClientDetails.class);

    ClientDetails savedClientDetails = clientDetailsDao.saveOrUpdate(clientDetails);

    ClientDetails retrievedClientDetails =
        clientDetailsDao.getClientDetailsById(savedClientDetails.getId());

    assertNotNull(retrievedClientDetails);
  }

  @Test
  public void getClientDetailsByUrl() throws JsonParseException, JsonMappingException, IOException {
    ClientDetails clientDetails =
        mapper.readValue(
            this.getClass().getClassLoader().getResourceAsStream("clientDetails.json"),
            ClientDetails.class);

    String fhirServerBaseURL = clientDetails.getFhirServerBaseURL();

    clientDetailsDao.saveOrUpdate(clientDetails);

    ClientDetails savedClientDetails = clientDetailsDao.getClientDetailsByUrl(fhirServerBaseURL);

    assertNotNull(savedClientDetails);
  }

  @Test
  public void getAllClientDetails() throws JsonParseException, JsonMappingException, IOException {
    ClientDetails clientDetails =
        mapper.readValue(
            this.getClass().getClassLoader().getResourceAsStream("clientDetails.json"),
            ClientDetails.class);
    ClientDetails clientDetails2 =
        mapper.readValue(
            this.getClass().getClassLoader().getResourceAsStream("clientDetails2.json"),
            ClientDetails.class);

    clientDetailsDao.saveOrUpdate(clientDetails);
    clientDetailsDao.saveOrUpdate(clientDetails2);

    List<ClientDetails> savedClientDetailsList = clientDetailsDao.getAllClientDetails();

    assertEquals(savedClientDetailsList.size(), 2);
  }
}
