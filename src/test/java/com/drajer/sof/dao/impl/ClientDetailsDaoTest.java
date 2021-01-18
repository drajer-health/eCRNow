package com.drajer.sof.dao.impl;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.sof.model.ClientDetails;
import com.drajer.test.util.TestUtils;
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

  private ClientDetails expectedClientDetails;
  private static ClientDetails secondClientDetails = null;

  @Before
  public void setUp() {
    expectedClientDetails =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDataEntry1.json", ClientDetails.class);
    secondClientDetails =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDataEntry2.json", ClientDetails.class);
  }

  @Test
  public void saveClientDetails() {
    ClientDetails actualClientDetails = clientDetailsDao.saveOrUpdate(expectedClientDetails);

    assertNotNull(actualClientDetails);
    assertClientDetails(expectedClientDetails, actualClientDetails);
  }

  @Test
  public void getClientDetailsById() {
    ClientDetails savedClientDetails = clientDetailsDao.saveOrUpdate(expectedClientDetails);
    ClientDetails actualClientDetails =
        clientDetailsDao.getClientDetailsById(savedClientDetails.getId());

    assertNotNull(actualClientDetails);
    assertClientDetails(expectedClientDetails, actualClientDetails);
  }

  @Test
  public void getClientDetailsByUrl() {
    clientDetailsDao.saveOrUpdate(expectedClientDetails);
    ClientDetails actualClientDetails =
        clientDetailsDao.getClientDetailsByUrl(expectedClientDetails.getFhirServerBaseURL());

    assertNotNull(actualClientDetails);
    assertClientDetails(expectedClientDetails, actualClientDetails);
  }

  @Test
  public void getAllClientDetails() {
    clientDetailsDao.saveOrUpdate(expectedClientDetails);
    clientDetailsDao.saveOrUpdate(secondClientDetails);
    List<ClientDetails> savedClientDetailsList = clientDetailsDao.getAllClientDetails();

    assertEquals(2, savedClientDetailsList.size());
  }

  private void assertClientDetails(
      ClientDetails expectedClientDetails, ClientDetails actualClientDetails) {

    assertEquals(expectedClientDetails.getClientId(), actualClientDetails.getClientId());
    assertEquals(
        expectedClientDetails.getFhirServerBaseURL(), actualClientDetails.getFhirServerBaseURL());
    assertEquals(expectedClientDetails.getTokenURL(), actualClientDetails.getTokenURL());
    assertEquals(expectedClientDetails.getScopes(), actualClientDetails.getScopes());
    assertEquals(
        expectedClientDetails.getDirectRecipientAddress(),
        actualClientDetails.getDirectRecipientAddress());
    assertEquals(expectedClientDetails.getDirectUser(), actualClientDetails.getDirectUser());
    assertEquals(expectedClientDetails.getRestAPIURL(), actualClientDetails.getRestAPIURL());
    assertEquals(expectedClientDetails.getDirectHost(), actualClientDetails.getDirectHost());
    assertEquals(
        expectedClientDetails.getAssigningAuthorityId(),
        actualClientDetails.getAssigningAuthorityId());
    assertEquals(
        expectedClientDetails.getEncounterEndThreshold(),
        actualClientDetails.getEncounterEndThreshold());
    assertEquals(
        expectedClientDetails.getEncounterStartThreshold(),
        actualClientDetails.getEncounterStartThreshold());
    assertEquals(expectedClientDetails.getImapPort(), actualClientDetails.getImapPort());
    assertEquals(expectedClientDetails.getSmtpPort(), actualClientDetails.getSmtpPort());
    assertEquals(
        expectedClientDetails.getXdrRecipientAddress(),
        actualClientDetails.getXdrRecipientAddress());
    assertEquals(
        expectedClientDetails.getDebugFhirQueryAndEicr(),
        actualClientDetails.getDebugFhirQueryAndEicr());
    assertEquals(expectedClientDetails.getIsCovid(), actualClientDetails.getIsCovid());
    assertEquals(expectedClientDetails.getIsSystem(), actualClientDetails.getIsSystem());
    assertEquals(expectedClientDetails.getIsDirect(), actualClientDetails.getIsDirect());
    assertEquals(expectedClientDetails.getIsFullEcr(), actualClientDetails.getIsFullEcr());
    assertEquals(expectedClientDetails.getIsProvider(), actualClientDetails.getIsProvider());
    assertEquals(expectedClientDetails.getIsRestAPI(), actualClientDetails.getIsRestAPI());
    assertEquals(expectedClientDetails.getIsXdr(), actualClientDetails.getIsXdr());
  }
}
