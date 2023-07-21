package com.drajer.ecrapp.dao.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import jakarta.persistence.EntityManager;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.List;
import java.util.Map;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.criteria.HibernateCriteriaBuilder;
import org.hibernate.query.criteria.JpaCriteriaQuery;
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

  //  @Mock private Criteria criteria;

  @Mock private HibernateCriteriaBuilder hibernateCriteriaBuilder;

  @InjectMocks private PhMessageDaoImpl phMessageDaoImpl;

  @Mock private EntityManager entityManager;

  @Mock JpaCriteriaQuery<PublicHealthMessage> jpaCriteriaQuery;

  @Mock private TypedQuery<PublicHealthMessage> typedQuery;

  private List<PublicHealthMessage> expectedPublicHealthMessages;
  private Map<String, String> searchParams;

  @Mock private CriteriaQuery<PublicHealthMessage> criteriaQuery;

  @Mock private Root<PublicHealthMessage> root;

  @Before
  public void setUp() {
    expectedPublicHealthMessages =
        TestUtils.readFileContents(
            PH_MESSAGE_FILE, new TypeReference<List<PublicHealthMessage>>() {});
    searchParams =
        TestUtils.readFileContents(SEARCH_PARAM_FILE, new TypeReference<Map<String, String>>() {});

    Mockito.lenient().when(sessionFactory.getCurrentSession()).thenReturn(session);
    CriteriaBuilder criteriaBuilder = mock(CriteriaBuilder.class);
    Mockito.lenient().when(session.getCriteriaBuilder()).thenReturn(hibernateCriteriaBuilder);

    CriteriaQuery<PublicHealthMessage> queryMock = mock(CriteriaQuery.class);
    when(criteriaBuilder.createQuery(PublicHealthMessage.class)).thenReturn(jpaCriteriaQuery);
    when(criteriaQuery.from(PublicHealthMessage.class)).thenReturn(root);
    //
    // Mockito.lenient().when(session.createCriteria(PublicHealthMessage.class)).thenReturn(criteria);
    //    Mockito.lenient().when(criteria.list()).thenReturn(expectedPublicHealthMessages);
    when(typedQuery.getResultList()).thenReturn(expectedPublicHealthMessages);
  }

  @Test
  public void testGetPhMessageData() {
    CriteriaBuilder criteriaBuilder = mock(CriteriaBuilder.class);
    when(criteriaBuilder.createQuery(PublicHealthMessage.class)).thenReturn(jpaCriteriaQuery);
    List<PublicHealthMessage> actualPublicHealthMessages =
        phMessageDaoImpl.getPhMessageData(searchParams);

    assertThat(actualPublicHealthMessages).hasSize(expectedPublicHealthMessages.size());
    assertEquals(
        TestUtils.toJsonString(expectedPublicHealthMessages),
        TestUtils.toJsonString(actualPublicHealthMessages));
  }
}
