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
import org.hibernate.Criteria;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.transform.Transformers;
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

  public List<PublicHealthMessage> getPhMessageData(
      Map<String, String> searchParams, boolean summaryFlag) {
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);

    prepareCriteria(criteria, searchParams);
    applySummaryFlagProjection(criteria, summaryFlag);

    List<PublicHealthMessage> phMessage = criteria.list();

    if (phMessage != null) {
      return phMessage;
    }
    return null;
  }

  public List<PublicHealthMessage> getPhMessageDataSummary(Map<String, String> searchParams) {
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);

    List<String> selectedProperties = getSelectedProperties();
    ProjectionList projectionList = buildProjectionList(selectedProperties, criteria);

    criteria.setProjection(projectionList);
    prepareCriteria(criteria, searchParams);

    criteria.setResultTransformer(Transformers.aliasToBean(PublicHealthMessage.class));

    List<PublicHealthMessage> phMessage = criteria.list();

    if (phMessage != null) {
      return phMessage;
    }
    return null;
  }

  public static void prepareCriteria(Criteria criteria, Map<String, String> searchParams) {

    if (searchParams.get(FHIR_SERVER_BASE_URL) != null) {
      criteria.add(Restrictions.eq(FHIR_SERVER_BASE_URL, searchParams.get(FHIR_SERVER_BASE_URL)));
    }
    if (searchParams.get(PATIENT_ID) != null) {
      criteria.add(Restrictions.eq(PATIENT_ID, searchParams.get(PATIENT_ID)));
    }
    if (searchParams.get(X_REQUEST_ID) != null) {
      criteria.add(Restrictions.eq(X_REQUEST_ID, searchParams.get(X_REQUEST_ID)));
    }
    if (searchParams.get(ENCOUNTER_ID) != null) {
      criteria.add(Restrictions.eq(ENCOUNTER_ID, searchParams.get(ENCOUNTER_ID)));
    }
    if (searchParams.get(SUBMITTED_DATA_ID) != null) {
      criteria.add(Restrictions.eq(SUBMITTED_DATA_ID, searchParams.get(SUBMITTED_DATA_ID)));
    }
    if (searchParams.get(VERSION) != null) {
      criteria.add(Restrictions.eq(VERSION, searchParams.get(VERSION)));
    }
    if (searchParams.get(RESPONSE_DATA_ID) != null) {
      criteria.add(Restrictions.eq(RESPONSE_DATA_ID, searchParams.get(RESPONSE_DATA_ID)));
    }
    if (searchParams.get(RESPONSE_PROCESSING_INSTRUCTION) != null) {
      criteria.add(
          Restrictions.eq(
              RESPONSE_PROCESSING_INSTRUCTION, searchParams.get(RESPONSE_PROCESSING_INSTRUCTION)));
    }
    if (searchParams.get(NOTIFIED_RESOURCE_ID) != null) {
      criteria.add(Restrictions.eq(NOTIFIED_RESOURCE_ID, searchParams.get(NOTIFIED_RESOURCE_ID)));
    }
    if (searchParams.get(NOTIFIED_RESOURCE_TYPE) != null) {
      criteria.add(
          Restrictions.eq(NOTIFIED_RESOURCE_TYPE, searchParams.get(NOTIFIED_RESOURCE_TYPE)));
    }
    if (searchParams.get(KAR_UNIQUE_ID) != null) {
      criteria.add(Restrictions.eq(KAR_UNIQUE_ID, searchParams.get(KAR_UNIQUE_ID)));
    }
    if (searchParams.get(NOTIFICATION_ID) != null) {
      criteria.add(Restrictions.eq(NOTIFICATION_ID, searchParams.get(NOTIFICATION_ID)));
    }
    if (searchParams.get(X_CORRELATION_ID) != null) {
      criteria.add(Restrictions.eq(X_CORRELATION_ID, searchParams.get(X_CORRELATION_ID)));
    }
    if (searchParams.get(SUBMISSION_TIME) != null
        && searchParams.get(RESPONSE_RECEIVED_TIME) != null) {

      Conjunction conjunction1 = Restrictions.conjunction();
      Conjunction conjunction2 = Restrictions.conjunction();
      Disjunction disjunction = Restrictions.disjunction();

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
      conjunction1.add(Restrictions.ge(SUBMISSION_TIME, submissionTimeDate));
      conjunction1.add(Restrictions.le(SUBMISSION_TIME, responseReceivedTimeDate));

      conjunction2.add(Restrictions.ge(RESPONSE_RECEIVED_TIME, submissionTimeDate));
      conjunction2.add(Restrictions.le(RESPONSE_RECEIVED_TIME, responseReceivedTimeDate));

      disjunction.add(conjunction1);
      disjunction.add(conjunction2);

      criteria.add(disjunction);
    }
  }

  @Override
  public List<PublicHealthMessage> getPhMessageByXRequestIds(
      List<String> xRequestIds, boolean summaryFlag) {
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);
    applySummaryFlagProjection(criteria, summaryFlag);
    criteria.add(Restrictions.in(X_REQUEST_ID, xRequestIds));
    return criteria.addOrder(Order.desc("id")).list();
  }

  // @Override
  public List<PublicHealthMessage> getPhMessagesContainingXRequestIds(
      List<String> xRequestIds, boolean summaryFlag) {
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);
    applySummaryFlagProjection(criteria, summaryFlag);

    Disjunction disjunction = Restrictions.disjunction();
    for (String xRequestId : xRequestIds) {
      disjunction.add(Restrictions.ilike(X_REQUEST_ID, xRequestId, MatchMode.ANYWHERE));
    }
    criteria.add(disjunction);
    return criteria.addOrder(Order.desc("id")).list();
  }

  private ProjectionList buildProjectionList(List<String> selectedProperties, Criteria criteria) {
    ProjectionList projectionList = Projections.projectionList();
    for (String propertyName : selectedProperties) {
      projectionList.add(Projections.property(propertyName), propertyName);
    }
    return projectionList;
  }

  @Override
  public List<PublicHealthMessage> getPhMessageByParameters(
      PublicHealthMessageData publicHealthMessageData) {

    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);

    UUID id = publicHealthMessageData.getId();
    if (id != null) {
      criteria.add(Restrictions.eq(ID, id));
    } else {

      String fhirServerBaseUrl = publicHealthMessageData.getFhirServerBaseUrl();
      if (fhirServerBaseUrl != null) {
        criteria.add(Restrictions.eq(FHIR_SERVER_BASE_URL, fhirServerBaseUrl));
      }

      String notifiedResourceId = publicHealthMessageData.getNotifiedResourceId();
      if (notifiedResourceId != null) {
        criteria.add(Restrictions.eq(NOTIFIED_RESOURCE_ID, notifiedResourceId));
      }

      String patientId = publicHealthMessageData.getPatientId();
      if (patientId != null) {
        criteria.add(Restrictions.eq(PATIENT_ID, patientId));
      }

      Integer submittedVersionNumber = publicHealthMessageData.getSubmittedVersionNumber();
      if (submittedVersionNumber != null) {
        criteria.add(Restrictions.eq(SUBMITTED_VERSION_NUMBER, submittedVersionNumber));
      }
    }

    return criteria.list();
  }

  @Override
  public void delete(PublicHealthMessage message) {
    getSession().delete(message);
  }

  private List<String> getSelectedProperties() {
    return Arrays.asList(
        "id",
        "fhirServerBaseUrl",
        "patientId",
        "encounterId",
        "notifiedResourceId",
        "notifiedResourceType",
        "karUniqueId",
        "notificationId",
        "xCorrelationId",
        "xRequestId",
        "submittedMessageType",
        "submittedDataId",
        "submittedVersionNumber",
        "submittedMessageId",
        "submissionMessageStatus",
        "submissionTime",
        "responseMessageType",
        "responseDataId",
        "responseMessageId",
        "responseProcessingInstruction",
        "responseProcessingStatus",
        "responseReceivedTime",
        "responseEhrDocRefId",
        "initiatingAction",
        "patientLinkerId",
        "lastUpdated");
  }

  private void applySummaryFlagProjection(Criteria criteria, boolean summaryFlag) {
    if (summaryFlag) {
      List<String> selectedProperties = getSelectedProperties();
      ProjectionList projectionList = buildProjectionList(selectedProperties, criteria);

      criteria.setProjection(projectionList);

      criteria.setResultTransformer(Transformers.aliasToBean(PublicHealthMessage.class));
    }
  }
}
