package com.drajer.ecrapp.dao.impl;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.PhMessageDao;
import java.util.List;
import java.util.Map;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
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
    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);

    prepareCriteria(criteria, searchParams);

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
  }
}
