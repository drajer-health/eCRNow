package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.List;
import java.util.Map;
import java.util.UUID;
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

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> query =
        criteriaBuilder.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> publicHealthMessage = query.from(PublicHealthMessage.class);

    query.where(
        criteriaBuilder.equal(
            publicHealthMessage.get(FHIR_SERVER_URL), message.getFhirServerBaseUrl()));
    query.where(
        criteriaBuilder.equal(
            publicHealthMessage.get(NOTIFIED_RESOURCE_ID), message.getNotifiedResourceId()));
    query.where(
        criteriaBuilder.equal(
            publicHealthMessage.get(NOTIFIED_RESOURCE_TYPE), message.getNotifiedResourceType()));
    query.where(criteriaBuilder.equal(publicHealthMessage.get(PATIENT_ID), message.getPatientId()));
    query.where(
        criteriaBuilder.equal(publicHealthMessage.get(KAR_UNIQUE_ID), message.getKarUniqueId()));

    query.orderBy(criteriaBuilder.desc(publicHealthMessage.get(SUBMITTED_VERSION_NUMBER)));

    TypedQuery<PublicHealthMessage> typedQuery = getSession().createQuery(query);
    typedQuery.setMaxResults(1);

    PublicHealthMessage resultHealthMessage = typedQuery.getSingleResult();

    if (resultHealthMessage != null) {
      return resultHealthMessage.getSubmittedVersionNumber();
    }
    return 0;
  }

  @Override
  public PublicHealthMessage getByCorrelationId(String coorelId) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> query =
        criteriaBuilder.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> publicHealthMessage = query.from(PublicHealthMessage.class);

    query.where(criteriaBuilder.equal(publicHealthMessage.get(X_CORRELATION_ID), coorelId));

    return getSession().createQuery(query).getSingleResult();
  }

  @Override
  public List<PublicHealthMessage> getPublicHealthMessage(Map<String, String> searchParams) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> query =
        criteriaBuilder.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> publicHealthMessage = query.from(PublicHealthMessage.class);

    prepareCriteria(criteriaBuilder, query, publicHealthMessage, searchParams);
    query.orderBy(criteriaBuilder.desc(publicHealthMessage.get(SUBMITTED_VERSION_NUMBER)));

    return getSession().createQuery(query).getResultList();
  }

  @Override
  public List<PublicHealthMessage> getByXRequestId(String xRequestId) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> query =
        criteriaBuilder.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> publicHealthMessage = query.from(PublicHealthMessage.class);

    query.where(criteriaBuilder.equal(publicHealthMessage.get(X_REQUEST_ID), xRequestId));
    query.orderBy(criteriaBuilder.desc(publicHealthMessage.get("id")));
    return getSession().createQuery(query).getResultList();
  }

  @Override
  public PublicHealthMessage getBySubmittedMessageId(String messageId) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> query =
        criteriaBuilder.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> publicHealthMessage = query.from(PublicHealthMessage.class);

    query.where(criteriaBuilder.equal(publicHealthMessage.get(SUBMITTED_MESSAGE_ID), messageId));
    return getSession().createQuery(query).getSingleResult();
  }

  @Override
  public PublicHealthMessage getByResponseMessageId(String id) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> query =
        criteriaBuilder.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> publicHealthMessage = query.from(PublicHealthMessage.class);

    query.where(criteriaBuilder.equal(publicHealthMessage.get(RESPONSE_MESSAGE_ID), id));
    return getSession().createQuery(query).getSingleResult();
  }

  @Override
  public void delete(PublicHealthMessage message) {
    getSession().delete(message);
  }

  @Override
  public PublicHealthMessage getBySubmittedDataId(String subId) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> query =
        criteriaBuilder.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> publicHealthMessage = query.from(PublicHealthMessage.class);

    query.where(criteriaBuilder.equal(publicHealthMessage.get(SUBMITTED_DATA_ID), subId));
    return getSession().createQuery(query).getSingleResult();
  }

  public static void prepareCriteria(
      CriteriaBuilder criteriaBuilder,
      CriteriaQuery<PublicHealthMessage> query,
      Root<PublicHealthMessage> publicHealthMessage,
      Map<String, String> searchParams) {

    if (searchParams.get(SUBMITTED_DATA_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(SUBMITTED_DATA_ID), searchParams.get(SUBMITTED_DATA_ID)));
    }
    if (searchParams.get(SUBMITTED_VERSION_NUMBER) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(SUBMITTED_VERSION_NUMBER),
              searchParams.get(SUBMITTED_VERSION_NUMBER)));
    }
    if (searchParams.get(RESPONSE_DATA_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(RESPONSE_DATA_ID), searchParams.get(RESPONSE_DATA_ID)));
    }
    if (searchParams.get(FHIR_SERVER_URL) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(FHIR_SERVER_URL), searchParams.get(FHIR_SERVER_URL)));
    }
    if (searchParams.get(PATIENT_ID) != null) {
      query.where(
          criteriaBuilder.equal(publicHealthMessage.get(PATIENT_ID), searchParams.get(PATIENT_ID)));
    }
    if (searchParams.get(ENCOUNTER_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(ENCOUNTER_ID), searchParams.get(ENCOUNTER_ID)));
    }
    if (searchParams.get(NOTIFIED_RESOURCE_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(NOTIFIED_RESOURCE_ID),
              searchParams.get(NOTIFIED_RESOURCE_ID)));
    }
    if (searchParams.get(NOTIFIED_RESOURCE_TYPE) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(NOTIFIED_RESOURCE_TYPE),
              searchParams.get(NOTIFIED_RESOURCE_TYPE)));
    }
    if (searchParams.get(KAR_UNIQUE_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(KAR_UNIQUE_ID), searchParams.get(KAR_UNIQUE_ID)));
    }
    if (searchParams.get(SUBMITTED_MESSAGE_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(SUBMITTED_MESSAGE_ID),
              searchParams.get(SUBMITTED_MESSAGE_ID)));
    }
    if (searchParams.get(X_REQUEST_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(X_REQUEST_ID), searchParams.get(X_REQUEST_ID)));
    }
    if (searchParams.get(X_CORRELATION_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(X_CORRELATION_ID), searchParams.get(X_CORRELATION_ID)));
    }
    if (searchParams.get(RESPONSE_MESSAGE_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(RESPONSE_MESSAGE_ID), searchParams.get(RESPONSE_MESSAGE_ID)));
    }
    if (searchParams.get(RESPONSE_PROCESSING_INS) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(RESPONSE_PROCESSING_INS),
              searchParams.get(RESPONSE_PROCESSING_INS)));
    }
    if (searchParams.get(RESPONSE_PROCESSING_STATUS) != null) {
      query.where(
          criteriaBuilder.equal(
              publicHealthMessage.get(RESPONSE_PROCESSING_STATUS),
              searchParams.get(RESPONSE_PROCESSING_STATUS)));
    }
  }
}
