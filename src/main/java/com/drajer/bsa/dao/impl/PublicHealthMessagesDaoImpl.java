package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import org.hibernate.query.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

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

  private final EntityManager em = getSession().getEntityManagerFactory().createEntityManager();

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
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);

    Predicate criteria = cb.and(
            cb.equal(root.get(FHIR_SERVER_URL), message.getFhirServerBaseUrl()),
            cb.equal(root.get(NOTIFIED_RESOURCE_ID), message.getNotifiedResourceId()),
            cb.equal(root.get(NOTIFIED_RESOURCE_TYPE), message.getNotifiedResourceType()),
            cb.equal(root.get(PATIENT_ID), message.getPatientId()),
            cb.equal(root.get(KAR_UNIQUE_ID), message.getKarUniqueId())
    );
    cq.where(criteria);
    cq.orderBy(cb.desc(root.get(SUBMITTED_VERSION_NUMBER)));

    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.uniqueResultOptional().map(PublicHealthMessage::getSubmittedVersionNumber).orElse(0);
  }

  @Override
  public PublicHealthMessage getByCorrelationId(String coorelId) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    cq.where(cb.equal(root.get(X_CORRELATION_ID), coorelId));

    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  @Override
  public List<PublicHealthMessage> getPublicHealthMessage(Map<String, String> searchParams) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    List<Predicate> predicates = preparePredicate(cb, root, searchParams);

    Predicate[] predArr = new Predicate[predicates.size()];
    predArr = predicates.toArray(predArr);

    Predicate criteria = cb.and(predArr);
    cq.where(criteria);
    cq.orderBy(cb.desc(root.get(SUBMITTED_VERSION_NUMBER)));

    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  @Override
  public List<PublicHealthMessage> getByXRequestId(String xRequestId) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    cq.where(cb.equal(root.get(X_REQUEST_ID), xRequestId));
    cq.orderBy(cb.desc(root.get("id")));

    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  @Override
  public PublicHealthMessage getBySubmittedMessageId(String messageId) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    cq.where(cb.equal(root.get(SUBMITTED_MESSAGE_ID), messageId));

    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  @Override
  public PublicHealthMessage getByResponseMessageId(String id) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    cq.where(cb.equal(root.get(RESPONSE_MESSAGE_ID), id));

    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  @Override
  public void delete(PublicHealthMessage message) {
    getSession().delete(message);
  }

  @Override
  public PublicHealthMessage getBySubmittedDataId(String subId) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthMessage> cq = cb.createQuery(PublicHealthMessage.class);
    Root<PublicHealthMessage> root = cq.from(PublicHealthMessage.class);
    cq.where(cb.equal(root.get(SUBMITTED_DATA_ID), subId));

    Query<PublicHealthMessage> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  public static List<Predicate> preparePredicate(CriteriaBuilder cb, Root<PublicHealthMessage> root, Map<String, String> searchParams) {
    List<Predicate> predicates = new ArrayList<Predicate>();

    if (searchParams.get(SUBMITTED_DATA_ID) != null) {
      predicates.add(cb.equal(root.get(SUBMITTED_DATA_ID), searchParams.get(SUBMITTED_DATA_ID)));
    }
    if (searchParams.get(SUBMITTED_VERSION_NUMBER) != null) {
      predicates.add(cb.equal(root.get(SUBMITTED_VERSION_NUMBER), searchParams.get(SUBMITTED_VERSION_NUMBER)));
    }
    if (searchParams.get(RESPONSE_DATA_ID) != null) {
      predicates.add(cb.equal(root.get(RESPONSE_DATA_ID), searchParams.get(RESPONSE_DATA_ID)));
    }
    if (searchParams.get(FHIR_SERVER_URL) != null) {
      predicates.add(cb.equal(root.get(FHIR_SERVER_URL), searchParams.get(FHIR_SERVER_URL)));
    }
    if (searchParams.get(PATIENT_ID) != null) {
      predicates.add(cb.equal(root.get(PATIENT_ID), searchParams.get(PATIENT_ID)));
    }
    if (searchParams.get(ENCOUNTER_ID) != null) {
      predicates.add(cb.equal(root.get(ENCOUNTER_ID), searchParams.get(ENCOUNTER_ID)));
    }
    if (searchParams.get(NOTIFIED_RESOURCE_ID) != null) {
      predicates.add(cb.equal(root.get(NOTIFIED_RESOURCE_ID), searchParams.get(NOTIFIED_RESOURCE_ID)));
    }
    if (searchParams.get(NOTIFIED_RESOURCE_TYPE) != null) {
      predicates.add(cb.equal(root.get(NOTIFIED_RESOURCE_TYPE), searchParams.get(NOTIFIED_RESOURCE_TYPE)));
    }
    if (searchParams.get(KAR_UNIQUE_ID) != null) {
      predicates.add(cb.equal(root.get(KAR_UNIQUE_ID), searchParams.get(KAR_UNIQUE_ID)));
    }
    if (searchParams.get(SUBMITTED_MESSAGE_ID) != null) {
      predicates.add(cb.equal(root.get(SUBMITTED_MESSAGE_ID), searchParams.get(SUBMITTED_MESSAGE_ID)));
    }
    if (searchParams.get(X_REQUEST_ID) != null) {
      predicates.add(cb.equal(root.get(X_REQUEST_ID), searchParams.get(X_REQUEST_ID)));
    }
    if (searchParams.get(X_CORRELATION_ID) != null) {
      predicates.add(cb.equal(root.get(X_CORRELATION_ID), searchParams.get(X_CORRELATION_ID)));
    }
    if (searchParams.get(RESPONSE_MESSAGE_ID) != null) {
      predicates.add(cb.equal(root.get(RESPONSE_MESSAGE_ID), searchParams.get(RESPONSE_MESSAGE_ID)));
    }
    if (searchParams.get(RESPONSE_PROCESSING_INS) != null) {
      predicates.add(cb.equal(root.get(RESPONSE_PROCESSING_INS), searchParams.get(RESPONSE_PROCESSING_INS)));
    }
    if (searchParams.get(RESPONSE_PROCESSING_STATUS) != null) {
      predicates.add(cb.equal(root.get(RESPONSE_PROCESSING_STATUS), searchParams.get(RESPONSE_PROCESSING_STATUS)));
    }

    return predicates;
  }
}
