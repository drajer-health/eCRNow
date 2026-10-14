package com.drajer.bsa.dao.impl;

import static com.drajer.ecrapp.dao.impl.EicrDaoImpl.RESPONSE_DOC_ID;

import com.drajer.bsa.dao.PhMessageDao;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.model.PublicHealthMessageData;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.BiFunction;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.query.Query;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

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

  /**
   * Properties selected when summaryFlag is true. The order must match the arguments of the
   * PublicHealthMessage summary projection constructor; the large payload columns are left out.
   */
  static final List<String> SUMMARY_PROPERTIES =
      List.of(
          ID,
          FHIR_SERVER_BASE_URL,
          PATIENT_ID,
          ENCOUNTER_ID,
          NOTIFIED_RESOURCE_ID,
          NOTIFIED_RESOURCE_TYPE,
          KAR_UNIQUE_ID,
          NOTIFICATION_ID,
          X_CORRELATION_ID,
          X_REQUEST_ID,
          "submittedMessageType",
          SUBMITTED_DATA_ID,
          SUBMITTED_VERSION_NUMBER,
          "submittedMessageId",
          "submissionMessageStatus",
          SUBMISSION_TIME,
          "responseMessageType",
          RESPONSE_DATA_ID,
          "responseMessageId",
          RESPONSE_PROCESSING_INSTRUCTION,
          "responseProcessingStatus",
          RESPONSE_RECEIVED_TIME,
          "responseEhrDocRefId",
          "initiatingAction",
          "patientLinkerId",
          "lastUpdated");

  public List<PublicHealthMessage> getPhMessageData(
      Map<String, String> searchParams, boolean summaryFlag) {
    return executeQuery(
        summaryFlag,
        (cb, root) -> {
          List<Predicate> predicates = preparePredicate(cb, root, searchParams);
          if (StringUtils.isNotBlank(searchParams.getOrDefault(RESPONSE_DOC_ID, ""))) {
            predicates.add(
                cb.equal(
                    root.get(RESPONSE_DOC_ID),
                    Integer.parseInt(searchParams.get(RESPONSE_DOC_ID))));
          }
          return predicates;
        });
  }

  public List<PublicHealthMessage> getPhMessageDataSummary(Map<String, String> searchParams) {
    // This API is always a summary, as it was with the Hibernate 5 projection.
    return executeQuery(true, (cb, root) -> preparePredicate(cb, root, searchParams));
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
        submissionTimeDate =
            new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(submissionTimeString);
        responseReceivedTimeDate =
            new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(responseReceivedTimeString);
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
    return executeQuery(
        summaryFlag,
        (cb, root) -> {
          Expression<String> xRequestIdExpression = root.get(X_REQUEST_ID);
          List<Predicate> predicates = new ArrayList<>();
          predicates.add(xRequestIdExpression.in(xRequestIds));
          return predicates;
        });
  }

  // @Override
  public List<PublicHealthMessage> getPhMessagesContainingXRequestIds(
      List<String> xRequestIds, boolean summaryFlag) {
    return executeQuery(
        summaryFlag,
        (cb, root) -> {
          List<Predicate> predicates = new ArrayList<>();
          for (String xRequestId : xRequestIds) {
            predicates.add(cb.like(cb.lower(root.get(X_REQUEST_ID)), xRequestId));
          }
          return predicates;
        });
  }

  /**
   * Runs a PublicHealthMessage query ordered by id descending, filtered by the predicates from
   * predicateBuilder.
   *
   * <p>summaryFlag = false returns managed, fully loaded entities. summaryFlag = true selects only
   * SUMMARY_PROPERTIES and lets Hibernate build each result through the PublicHealthMessage summary
   * constructor, so the large payload columns are never read. Those instances are not managed by
   * the persistence context and must not be merged or saved back.
   */
  private List<PublicHealthMessage> executeQuery(
      boolean summaryFlag,
      BiFunction<CriteriaBuilder, Root<PublicHealthMessage>, List<Predicate>> predicateBuilder) {

    try (EntityManager em = getSession().getEntityManagerFactory().createEntityManager()) {
      CriteriaBuilder cb = em.getCriteriaBuilder();
      CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
      Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);

      if (summaryFlag) {
        cq.select(getSummarySelection(cb, root));
      }
      cq.where(cb.and(predicateBuilder.apply(cb, root).toArray(new Predicate[0])));
      cq.orderBy(cb.desc(root.get(ID)));
      Query<PublicHealthMessage> q = getSession().createQuery(cq);

      return q.getResultList();
    }
  }

  @Override
  public List<PublicHealthMessage> getPhMessageByParameters(
      PublicHealthMessageData publicHealthMessageData) {

    try (EntityManager em = getSession().getEntityManagerFactory().createEntityManager()) {
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
  }

  @Override
  public void delete(PublicHealthMessage message) {
    remove(message);
  }

  private CompoundSelection<PublicHealthMessage> getSummarySelection(
      CriteriaBuilder cb, Root<PublicHealthMessage> root) {
    Selection<?>[] selections = new Selection<?>[SUMMARY_PROPERTIES.size()];
    for (int i = 0; i < selections.length; i++) {
      selections[i] = root.get(SUMMARY_PROPERTIES.get(i));
    }
    return cb.construct(PublicHealthMessage.class, selections);
  }
}
