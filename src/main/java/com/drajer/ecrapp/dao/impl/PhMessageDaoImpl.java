package com.drajer.ecrapp.dao.impl;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.PhMessageDao;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class PhMessageDaoImpl extends AbstractDao implements PhMessageDao {

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

  public List<PublicHealthMessage> getPhMessageData(Map<String, String> searchParams) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> query =
        criteriaBuilder.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> phMessageEntity = query.from(PublicHealthMessage.class);

    prepareCriteria(criteriaBuilder, query, phMessageEntity, searchParams);

    List<PublicHealthMessage> phMessage = getSession().createQuery(query).getResultList();

    if (phMessage != null) {
      return phMessage;
    }
    return null;
  }

  public static void prepareCriteria(
      CriteriaBuilder criteriaBuilder,
      CriteriaQuery<PublicHealthMessage> query,
      Root<PublicHealthMessage> phMessageEntity,
      Map<String, String> searchParams) {

    if (searchParams.get(FHIR_SERVER_BASE_URL) != null) {
      query.where(
          criteriaBuilder.equal(
              phMessageEntity.get(FHIR_SERVER_BASE_URL), searchParams.get(FHIR_SERVER_BASE_URL)));
    }
    if (searchParams.get(PATIENT_ID) != null) {
      query.where(
          criteriaBuilder.equal(phMessageEntity.get(PATIENT_ID), searchParams.get(PATIENT_ID)));
    }
    if (searchParams.get(X_REQUEST_ID) != null) {
      query.where(
          criteriaBuilder.equal(phMessageEntity.get(X_REQUEST_ID), searchParams.get(X_REQUEST_ID)));
    }
    if (searchParams.get(ENCOUNTER_ID) != null) {
      query.where(
          criteriaBuilder.equal(phMessageEntity.get(ENCOUNTER_ID), searchParams.get(ENCOUNTER_ID)));
    }
    if (searchParams.get(SUBMITTED_DATA_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              phMessageEntity.get(SUBMITTED_DATA_ID), searchParams.get(SUBMITTED_DATA_ID)));
    }
    if (searchParams.get(VERSION) != null) {
      query.where(criteriaBuilder.equal(phMessageEntity.get(VERSION), searchParams.get(VERSION)));
    }
    if (searchParams.get(RESPONSE_DATA_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              phMessageEntity.get(RESPONSE_DATA_ID), searchParams.get(RESPONSE_DATA_ID)));
    }
    if (searchParams.get(RESPONSE_PROCESSING_INSTRUCTION) != null) {
      // criteria.add(
      //  Restrictions.eq(
      //  RESPONSE_PROCESSING_INSTRUCTION, searchParams.get(RESPONSE_PROCESSING_INSTRUCTION)));
      query.where(
          criteriaBuilder.equal(
              phMessageEntity.get(RESPONSE_PROCESSING_INSTRUCTION),
              searchParams.get(RESPONSE_PROCESSING_INSTRUCTION)));
    }
    if (searchParams.get(NOTIFIED_RESOURCE_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              phMessageEntity.get(NOTIFIED_RESOURCE_ID), searchParams.get(NOTIFIED_RESOURCE_ID)));
    }
    if (searchParams.get(NOTIFIED_RESOURCE_TYPE) != null) {
      query.where(
          criteriaBuilder.equal(
              phMessageEntity.get(NOTIFIED_RESOURCE_TYPE),
              searchParams.get(NOTIFIED_RESOURCE_TYPE)));
    }
    if (searchParams.get(KAR_UNIQUE_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              phMessageEntity.get(KAR_UNIQUE_ID), searchParams.get(KAR_UNIQUE_ID)));
    }
    if (searchParams.get(NOTIFICATION_ID) != null) {
      query.where(
          criteriaBuilder.equal(
              phMessageEntity.get(NOTIFICATION_ID), searchParams.get(NOTIFICATION_ID)));
    }
  }
}
