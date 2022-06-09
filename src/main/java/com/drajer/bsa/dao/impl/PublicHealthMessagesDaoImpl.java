package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.dao.AbstractDao;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class PublicHealthMessagesDaoImpl extends AbstractDao implements PublicHealthMessagesDao {

  public static final String SUBMITTED_DATA_ID = "submittedDataId";
  public static final String SUBMITTED_VERSION_NUMBER = "submittedVersionNumber";
  public static final String RESPONSE_DATA_ID = "responseDataId";
  public static final String FHIR_SERVER_URL = "fhirServerBaseUrl";
  public static final String ENCOUNTER_ID = "encounterId";
  public static final String PATIENT_ID = "patientId";
  public static final String NOTIFIED_RESOURCE_ID = "notifiedResourceId";
  public static final String NOTIFIED_RESOURCE_TYPE = "notifiedResourceType";
  public static final String KAR_UNIQUE_ID = "karUniqueId";
  public static final String X_REQUEST_ID = "xRequestId";
  public static final String X_CORRELATION_ID = "xCorrelationId";
  public static final String SUBMITTED_MESSAGE_ID = "submittedMessageId";
  public static final String RESPONSE_MESSAGE_ID = "responseMessageId";
  public static final String RESPONSE_PROCESSING_INS = "responseProcessingInstruction";
  public static final String RESPONSE_PROCESSING_STATUS = "responseProcessingStatus";

  @Override
  public PublicHealthMessage saveOrUpdate(PublicHealthMessage message) {
    getSession().saveOrUpdate(message);
    return message;
  }

  @Override
  public PublicHealthMessage getById(UUID id) {
    return getSession().get(PublicHealthMessage.class, id);
  }

  @Override
  public Integer getMaxVersionId(PublicHealthMessage message) {
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);
    criteria.add(Restrictions.eq(FHIR_SERVER_URL, message.getFhirServerBaseUrl()));
    criteria.add(Restrictions.eq(NOTIFIED_RESOURCE_ID, message.getNotifiedResourceId()));
    criteria.add(Restrictions.eq(NOTIFIED_RESOURCE_TYPE, message.getNotifiedResourceType()));
    criteria.add(Restrictions.eq(PATIENT_ID, message.getPatientId()));
    criteria.add(Restrictions.eq(KAR_UNIQUE_ID, message.getKarUniqueId()));

    criteria.addOrder(Order.desc(SUBMITTED_VERSION_NUMBER));
    criteria.setMaxResults(1);

    PublicHealthMessage result = (PublicHealthMessage) criteria.uniqueResult();

    if (result != null) {
      return result.getSubmittedVersionNumber();
    }
    return 0;
  }

  @Override
  public PublicHealthMessage getByCorrelationId(String coorelId) {
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);
    criteria.add(Restrictions.eq(X_CORRELATION_ID, coorelId));
    return (PublicHealthMessage) criteria.uniqueResult();
  }

  @Override
  public List<PublicHealthMessage> getPublicHealthMessage(Map<String, String> searchParams) {
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);
    prepareCriteria(criteria, searchParams);
    return criteria.addOrder(Order.desc(SUBMITTED_VERSION_NUMBER)).list();
  }

  @Override
  public List<PublicHealthMessage> getByXRequestId(String xRequestId) {
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);
    criteria.add(Restrictions.eq(X_REQUEST_ID, xRequestId));
    return criteria.addOrder(Order.desc("id")).list();
  }

  @Override
  public PublicHealthMessage getBySubmittedMessageId(String messageId) {
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);
    criteria.add(Restrictions.eq(SUBMITTED_MESSAGE_ID, messageId));
    return (PublicHealthMessage) criteria.uniqueResult();
  }

  @Override
  public PublicHealthMessage getByResponseMessageId(String id) {
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);
    criteria.add(Restrictions.eq(RESPONSE_MESSAGE_ID, id));
    return (PublicHealthMessage) criteria.uniqueResult();
  }

  @Override
  public void delete(PublicHealthMessage message) {
    getSession().delete(message);
  }

  @Override
  public PublicHealthMessage getBySubmittedDataId(String subId) {

    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);
    criteria.add(Restrictions.eq(SUBMITTED_DATA_ID, subId));

    return (PublicHealthMessage) criteria.uniqueResult();
  }

  public static void prepareCriteria(Criteria criteria, Map<String, String> searchParams) {

    if (searchParams.get(SUBMITTED_DATA_ID) != null) {
      criteria.add(Restrictions.eq(SUBMITTED_DATA_ID, searchParams.get(SUBMITTED_DATA_ID)));
    }
    if (searchParams.get(SUBMITTED_VERSION_NUMBER) != null) {
      criteria.add(
          Restrictions.eq(SUBMITTED_VERSION_NUMBER, searchParams.get(SUBMITTED_VERSION_NUMBER)));
    }
    if (searchParams.get(RESPONSE_DATA_ID) != null) {
      criteria.add(Restrictions.eq(RESPONSE_DATA_ID, searchParams.get(RESPONSE_DATA_ID)));
    }
    if (searchParams.get(FHIR_SERVER_URL) != null) {
      criteria.add(Restrictions.eq(FHIR_SERVER_URL, searchParams.get(FHIR_SERVER_URL)));
    }
    if (searchParams.get(PATIENT_ID) != null) {
      criteria.add(Restrictions.eq(PATIENT_ID, searchParams.get(PATIENT_ID)));
    }
    if (searchParams.get(ENCOUNTER_ID) != null) {
      criteria.add(Restrictions.eq(ENCOUNTER_ID, searchParams.get(ENCOUNTER_ID)));
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
    if (searchParams.get(SUBMITTED_MESSAGE_ID) != null) {
      criteria.add(Restrictions.eq(SUBMITTED_MESSAGE_ID, searchParams.get(SUBMITTED_MESSAGE_ID)));
    }
    if (searchParams.get(X_REQUEST_ID) != null) {
      criteria.add(Restrictions.eq(X_REQUEST_ID, searchParams.get(X_REQUEST_ID)));
    }
    if (searchParams.get(X_CORRELATION_ID) != null) {
      criteria.add(Restrictions.eq(X_CORRELATION_ID, searchParams.get(X_CORRELATION_ID)));
    }
    if (searchParams.get(RESPONSE_MESSAGE_ID) != null) {
      criteria.add(Restrictions.eq(RESPONSE_MESSAGE_ID, searchParams.get(RESPONSE_MESSAGE_ID)));
    }
    if (searchParams.get(RESPONSE_PROCESSING_INS) != null) {
      criteria.add(
          Restrictions.eq(RESPONSE_PROCESSING_INS, searchParams.get(RESPONSE_PROCESSING_INS)));
    }
    if (searchParams.get(RESPONSE_PROCESSING_STATUS) != null) {
      criteria.add(
          Restrictions.eq(
              RESPONSE_PROCESSING_STATUS, searchParams.get(RESPONSE_PROCESSING_STATUS)));
    }
  }
}
