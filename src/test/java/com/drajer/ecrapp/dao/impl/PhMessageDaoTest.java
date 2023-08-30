package com.drajer.ecrapp.dao.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
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
        phMessageDaoImpl.getPhMessageData(searchParams);

    assertThat(actualPublicHealthMessages).hasSize(expectedPublicHealthMessages.size());
    assertEquals(
        TestUtils.toJsonString(expectedPublicHealthMessages),
        TestUtils.toJsonString(actualPublicHealthMessages));
  }

  @Test
  public void testGetPhMessageByXRequestIds() {
    List<String> xRequestIds = Arrays.asList("xRequestId1", "xRequestId2");

    List<PublicHealthMessage> actualPublicHealthMessages =
        phMessageDaoImpl.getPhMessageByXRequestIds(xRequestIds);

    assertThat(actualPublicHealthMessages).hasSize(expectedPublicHealthMessages.size());
    assertEquals(
        TestUtils.toJsonString(expectedPublicHealthMessages),
        TestUtils.toJsonString(actualPublicHealthMessages));
  }

  private void mockProjectionAndCriteria() {
    Mockito.lenient().when(criteria.setProjection(any(ProjectionList.class))).thenReturn(criteria);
    Mockito.lenient().when(criteria.setResultTransformer(any())).thenReturn(criteria);
    Mockito.lenient().when(criteria.add(any())).thenReturn(criteria);
    Mockito.lenient().when(criteria.addOrder(any())).thenReturn(criteria);
  }
}
