package com.drajer.ecrapp.dao.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

import com.drajer.bsa.dao.impl.PhMessageDaoImpl;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.sof.model.PublicHealthMessageData;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.ProjectionList;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class PhMessageDaoTest {

  private static final String PH_MESSAGE_FILE = "ecrTestData/PhMessageOutput/PhMessage.json";
  private static final String SEARCH_PARAM_FILE = "ecrTestData/phMessageInput/searchParam.json";

  @Mock private SessionFactory sessionFactory;

  @Mock private Session session;

  @Mock private Criteria criteria;

  @InjectMocks private PhMessageDaoImpl phMessageDaoImpl;

  private List<PublicHealthMessage> expectedPublicHealthMessages;
  private Map<String, String> searchParams;

  @Before
  public void setUp() {
    expectedPublicHealthMessages =
        TestUtils.readFileContents(
            PH_MESSAGE_FILE, new TypeReference<List<PublicHealthMessage>>() {});
    searchParams =
        TestUtils.readFileContents(SEARCH_PARAM_FILE, new TypeReference<Map<String, String>>() {});

    Mockito.lenient().when(sessionFactory.getCurrentSession()).thenReturn(session);
    Mockito.lenient().when(session.createCriteria(PublicHealthMessage.class)).thenReturn(criteria);
    mockProjectionAndCriteria();

    Mockito.lenient().when(criteria.list()).thenReturn(expectedPublicHealthMessages);
  }

  @Test
  public void testGetPhMessageData() {

    List<PublicHealthMessage> actualPublicHealthMessages =
        phMessageDaoImpl.getPhMessageData(searchParams, false);

    assertThat(actualPublicHealthMessages).hasSize(expectedPublicHealthMessages.size());
    assertEquals(
        TestUtils.toJsonString(expectedPublicHealthMessages),
        TestUtils.toJsonString(actualPublicHealthMessages));
  }

  @Test
  public void testGetPhMessageByXRequestIds() {
    List<String> xRequestIds = Arrays.asList("xRequestId1", "xRequestId2");

    List<PublicHealthMessage> actualPublicHealthMessages =
        phMessageDaoImpl.getPhMessageByXRequestIds(xRequestIds, false);

    assertThat(actualPublicHealthMessages).hasSize(expectedPublicHealthMessages.size());
    assertEquals(
        TestUtils.toJsonString(expectedPublicHealthMessages),
        TestUtils.toJsonString(actualPublicHealthMessages));
  }

  @Test
  public void testGetPhMessageByParameters() {
    PublicHealthMessageData publicHealthMessageData = new PublicHealthMessageData();
    publicHealthMessageData.setId(UUID.randomUUID());

    List<PublicHealthMessage> actualPublicHealthMessages =
        phMessageDaoImpl.getPhMessageByParameters(publicHealthMessageData);

    assertThat(actualPublicHealthMessages).isNotEmpty();

    PublicHealthMessageData publicHealthMessageData1 = new PublicHealthMessageData();
    publicHealthMessageData1.setFhirServerBaseUrl("http://localhost:8080/test/r4/fhir/");
    publicHealthMessageData1.setSubmittedVersionNumber(1);
    publicHealthMessageData1.setPatientId("c76d4027-7dff-43d4-adec-e3a9");
    publicHealthMessageData1.setNotifiedResourceId("66");

    List<PublicHealthMessage> actualPublicHealthMessages1 =
        phMessageDaoImpl.getPhMessageByParameters(publicHealthMessageData);

    assertThat(actualPublicHealthMessages1).isNotEmpty();
  }

  @Test
  public void testDelete() {
    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();

    Mockito.lenient().doNothing().when(session).delete(publicHealthMessage);

    phMessageDaoImpl.delete(publicHealthMessage);

    verify(session).delete(publicHealthMessage);
  }

  private void mockProjectionAndCriteria() {
    Mockito.lenient().when(criteria.setProjection(any(ProjectionList.class))).thenReturn(criteria);
    Mockito.lenient().when(criteria.setResultTransformer(any())).thenReturn(criteria);
    Mockito.lenient().when(criteria.add(any())).thenReturn(criteria);
    Mockito.lenient().when(criteria.addOrder(any())).thenReturn(criteria);
  }
}
