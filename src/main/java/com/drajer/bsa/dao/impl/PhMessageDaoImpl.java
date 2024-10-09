package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.PhMessageDao;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.model.PublicHealthMessageData;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.*;
import org.hibernate.transform.Transformers;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;
import java.util.*;
import org.hibernate.query.Query;

import static com.drajer.ecrapp.dao.impl.EicrDaoImpl.RESPONSE_DOC_ID;

@Repository
@Transactional
public class PhMessageDaoImpl extends AbstractDao implements PhMessageDao {

  private static final Logger logger = LoggerFactory.getLogger(PhMessageDaoImpl.class);

  public static final String ID = "id";
  public static final String FHIR_SERVER_BASE_URL = "fhirServerBaseUrl";
  public static final String PATIENT_ID = "patientId";
  public static final String ENCOUNTER_ID = "encounterId";
  public static final String X_REQUEST_ID = "xRequestId";
  public static final String SUBMITTED_DATA_ID = "submittedDataId";
  public static final String VERSION = "version";
  public static final String RESPONSE_DATA_ID = "responseDataId";
  public static final String RESPONSE_PROCESSING_INSTRUCTION = "responseProcessingInstruction";
  public static final String NOTIFIED_RESOURCE_ID = "notifiedResourceId";
  public static final String NOTIFIED_RESOURCE_TYPE = "notifiedResourceType";
  public static final String KAR_UNIQUE_ID = "karUniqueId";
  public static final String NOTIFICATION_ID = "notificationId";
  public static final String X_CORRELATION_ID = "xCorrelationId";
  public static final String SUBMISSION_TIME = "submissionTime";
  public static final String RESPONSE_RECEIVED_TIME = "responseReceivedTime";
  public static final String SUBMITTED_VERSION_NUMBER = "submittedVersionNumber";
  public static final String SUMMARY_FLAG = "summaryFlag";

  public List<PublicHealthMessage> getPhMessageData(
      Map<String, String> searchParams, boolean summaryFlag) {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    List<Predicate> predicates = preparePredicate(cb, root, searchParams);
    predicates.add(
            cb.equal(root.get(RESPONSE_DOC_ID), Integer.parseInt(searchParams.get(RESPONSE_DOC_ID))));
    Predicate[] predArr = new Predicate[predicates.size()];
    predArr = predicates.toArray(predArr);
    Predicate criteria = cb.and(predArr);
    cq.where(criteria);
    cq.orderBy(cb.desc(root.get("id")));
    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  public List<PublicHealthMessage> getPhMessageDataSummary(Map<String, String> searchParams) {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    List<Predicate> predicates = preparePredicate(cb, root, searchParams);
    Predicate[] predArr = new Predicate[predicates.size()];
    predArr = predicates.toArray(predArr);
    Predicate criteria = cb.and(predArr);
    cq.where(criteria);
    cq.orderBy(cb.desc(root.get("id")));
    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  public static List<Predicate> preparePredicate(
          CriteriaBuilder cb, Root<PublicHealthMessage> root, Map<String, String> searchParams) {
    List<Predicate> predicates = new ArrayList<Predicate>();

    if (searchParams.get(FHIR_SERVER_BASE_URL) != null) {
      predicates.add(
              cb.equal(root.get(FHIR_SERVER_BASE_URL), searchParams.get(FHIR_SERVER_BASE_URL)));
    }
    if (searchParams.get(PATIENT_ID) != null) {
      predicates.add(cb.equal(root.get(PATIENT_ID), searchParams.get(PATIENT_ID)));
    }
    if (searchParams.get(X_REQUEST_ID) != null) {
      predicates.add(cb.equal(root.get(X_REQUEST_ID), searchParams.get(X_REQUEST_ID)));
    }
    if (searchParams.get(ENCOUNTER_ID) != null) {
      predicates.add(cb.equal(root.get(ENCOUNTER_ID), searchParams.get(ENCOUNTER_ID)));
    }
    if (searchParams.get(SUBMITTED_DATA_ID) != null) {
      predicates.add(cb.equal(root.get(SUBMITTED_DATA_ID), searchParams.get(SUBMITTED_DATA_ID)));
    }
    if (searchParams.get(VERSION) != null) {
      predicates.add(cb.equal(root.get(VERSION), searchParams.get(VERSION)));
    }
    if (searchParams.get(RESPONSE_DATA_ID) != null) {
      predicates.add(cb.equal(root.get(RESPONSE_DATA_ID), searchParams.get(RESPONSE_DATA_ID)));
    }
    if (searchParams.get(RESPONSE_PROCESSING_INSTRUCTION) != null) {
      predicates.add(
              cb.equal(
                      root.get(RESPONSE_PROCESSING_INSTRUCTION),
                      searchParams.get(RESPONSE_PROCESSING_INSTRUCTION)));
    }
    if (searchParams.get(NOTIFIED_RESOURCE_ID) != null) {
      predicates.add(
              cb.equal(root.get(NOTIFIED_RESOURCE_ID), searchParams.get(NOTIFIED_RESOURCE_ID)));
    }
    if (searchParams.get(NOTIFIED_RESOURCE_TYPE) != null) {
      predicates.add(
              cb.equal(root.get(NOTIFIED_RESOURCE_TYPE), searchParams.get(NOTIFIED_RESOURCE_TYPE)));
    }
    if (searchParams.get(KAR_UNIQUE_ID) != null) {
      predicates.add(cb.equal(root.get(KAR_UNIQUE_ID), searchParams.get(KAR_UNIQUE_ID)));
    }
    if (searchParams.get(NOTIFICATION_ID) != null) {
      predicates.add(cb.equal(root.get(NOTIFICATION_ID), searchParams.get(NOTIFICATION_ID)));
    }
    if (searchParams.get(X_CORRELATION_ID) != null) {
      predicates.add(cb.equal(root.get(X_CORRELATION_ID), searchParams.get(X_CORRELATION_ID)));
    }
    if (searchParams.get(SUBMISSION_TIME) != null
        && searchParams.get(RESPONSE_RECEIVED_TIME) != null) {

      String submissionTimeString = searchParams.get(SUBMISSION_TIME);
      String responseReceivedTimeString = searchParams.get(RESPONSE_RECEIVED_TIME);
      Date submissionTimeDate = null;
      Date responseReceivedTimeDate = null;
      try {
        submissionTimeDate = new SimpleDateFormat("yyyy-MM-dd").parse(submissionTimeString);
        responseReceivedTimeDate =
            new SimpleDateFormat("yyyy-MM-dd").parse(responseReceivedTimeString);
      } catch (ParseException e) {
        logger.error("Exception while converting into date format", e);
      }
      Predicate disjPred =
              cb.or(
                      cb.and(
                              cb.greaterThanOrEqualTo(root.get(SUBMISSION_TIME), submissionTimeDate),
                              cb.lessThanOrEqualTo(root.get(SUBMISSION_TIME), responseReceivedTimeDate)),
                      cb.and(
                              cb.greaterThanOrEqualTo(root.get(RESPONSE_RECEIVED_TIME), submissionTimeDate),
                              cb.lessThanOrEqualTo(
                                      root.get(RESPONSE_RECEIVED_TIME), responseReceivedTimeDate)));

      predicates.add(disjPred);
    }

    return predicates;
  }

  @Override
  public List<PublicHealthMessage> getPhMessageByXRequestIds(
      List<String> xRequestIds, boolean summaryFlag) {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    Expression<String> xRequestIdExpression = root.get(X_REQUEST_ID);
    Predicate xRequestIdPredicate = xRequestIdExpression.in(xRequestIds);
    cq.where(xRequestIdPredicate);
    cq.orderBy(cb.desc(root.get("id")));

    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  // @Override
  public List<PublicHealthMessage> getPhMessagesContainingXRequestIds(
      List<String> xRequestIds, boolean summaryFlag) {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    List<Predicate> predicates = new ArrayList<Predicate>();
    for (String xRequestId : xRequestIds) {
      predicates.add(cb.like(cb.lower(root.get(X_REQUEST_ID)), xRequestId));
    }
    Predicate[] predArr = new Predicate[predicates.size()];
    predArr = predicates.toArray(predArr);

    Predicate criteria = cb.and(predArr);
    cq.where(criteria);
    cq.orderBy(cb.desc(root.get("id")));

    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  /*private ProjectionList buildProjectionList(List<String> selectedProperties, Criteria criteria) {
    ProjectionList projectionList = Projections.projectionList();
    for (String propertyName : selectedProperties) {
      projectionList.add(Projections.property(propertyName), propertyName);
    }
    return projectionList;
  }*/

  @Override
  public List<PublicHealthMessage> getPhMessageByParameters(
      PublicHealthMessageData publicHealthMessageData) {

    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    UUID id = publicHealthMessageData.getId();
    List<Predicate> predicates = new ArrayList<Predicate>();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    if (id != null) {
      predicates.add(cb.equal(root.get(ID), id));
    } else {

      String fhirServerBaseUrl = publicHealthMessageData.getFhirServerBaseUrl();
      if (fhirServerBaseUrl != null) {
        predicates.add(cb.equal(root.get(FHIR_SERVER_BASE_URL), fhirServerBaseUrl));
      }

      String notifiedResourceId = publicHealthMessageData.getNotifiedResourceId();
      if (notifiedResourceId != null) {
        predicates.add(cb.equal(root.get(NOTIFIED_RESOURCE_ID), notifiedResourceId));
      }

      String patientId = publicHealthMessageData.getPatientId();
      if (patientId != null) {
        predicates.add(cb.equal(root.get(PATIENT_ID), patientId));
      }

      Integer submittedVersionNumber = publicHealthMessageData.getSubmittedVersionNumber();
      if (submittedVersionNumber != null) {
        predicates.add(cb.equal(root.get(SUBMITTED_VERSION_NUMBER), submittedVersionNumber));
      }
    }

    Predicate[] predArr = new Predicate[predicates.size()];
    predArr = predicates.toArray(predArr);

    Predicate criteria = cb.and(predArr);
    cq.where(criteria);

    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  @Override
  public void delete(PublicHealthMessage message) {
    getSession().delete(message);
  }

  private CompoundSelection<Object[]> getSelectedProperties(
          CriteriaBuilder cb, Root<PublicHealthMessage> root) {
    return cb.array(
            root.get("id"),
            root.get("fhirServerBaseUrl"),
            root.get("patientId"),
            root.get("encounterId"),
            root.get("notifiedResourceId"),
            root.get("notifiedResourceType"),
            root.get("karUniqueId"),
            root.get("notificationId"),
            root.get("xCorrelationId"),
            root.get("xRequestId"),
            root.get("submittedMessageType"),
            root.get("submittedDataId"),
            root.get("submittedVersionNumber"),
            root.get("submittedMessageId"),
            root.get("submissionMessageStatus"),
            root.get("submissionTime"),
            root.get("responseMessageType"),
            root.get("responseDataId"),
            root.get("responseMessageId"),
            root.get("responseProcessingInstruction"),
            root.get("responseProcessingStatus"),
            root.get("responseReceivedTime"),
            root.get("responseEhrDocRefId"),
            root.get("initiatingAction"),
            root.get("patientLinkerId"),
            root.get("lastUpdated"));
  }

  /*private void applySummaryFlagProjection(Criteria criteria, boolean summaryFlag) {
    if (summaryFlag) {
      List<String> selectedProperties = getSelectedProperties();
      ProjectionList projectionList = buildProjectionList(selectedProperties, criteria);

      criteria.setProjection(projectionList);

      criteria.setResultTransformer(Transformers.aliasToBean(PublicHealthMessage.class));
    }
  }*/
}
