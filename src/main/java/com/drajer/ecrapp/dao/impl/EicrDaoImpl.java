package com.drajer.ecrapp.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
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

@Repository
@Transactional
public class EicrDaoImpl extends AbstractDao implements EicrDao {

  public static final String FHIR_SERVER_URL = "fhirServerUrl";
  public static final String ENCOUNTER_ID = "encounterId";
  public static final String EICR_DOC_ID = "eicrDocId";
  public static final String RESPONSE_DOC_ID = "responseDocId";
  public static final String SET_ID = "setId";
  public static final String X_REQUEST_ID = "xRequestId";

  private final EntityManager em = getSession().getEntityManagerFactory().createEntityManager();

  public Eicr saveOrUpdate(Eicr eicr) {
    getSession().saveOrUpdate(eicr);
    return eicr;
  }

  public Eicr getEicrById(Integer id) {
    return getSession().get(Eicr.class, id);
  }

  public ReportabilityResponse saveOrUpdate(ReportabilityResponse rr) {
    getSession().saveOrUpdate(rr);
    return rr;
  }

  public ReportabilityResponse getRRById(Integer id) {
    return getSession().get(ReportabilityResponse.class, id);
  }

  public Integer getMaxVersionId(Eicr eicr) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<Eicr> cq = cb.createQuery(Eicr.class);
    Root<Eicr> root = cq.from(Eicr.class);

    Predicate criteria = cb.and(
            cb.equal(root.get(FHIR_SERVER_URL), eicr.getFhirServerUrl()),
            cb.equal(root.get("launchPatientId"), eicr.getLaunchPatientId()),
            cb.equal(root.get(ENCOUNTER_ID), eicr.getEncounterId())
    );
    cq.where(criteria);
    cq.orderBy(cb.desc(root.get("docVersion")));

    Query<Eicr> q = getSession().createQuery(cq);
    q.setFirstResult(0);
    q.setMaxResults(1);

    return q.uniqueResultOptional().map(Eicr::getDocVersion).orElse(0);
  }

  public Eicr getEicrByCorrelationId(String xcoorrId) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<Eicr> cq = cb.createQuery(Eicr.class);
    Root<Eicr> root = cq.from(Eicr.class);
    cq.where(cb.equal(root.get("xCorrelationId"), xcoorrId));

    Query<Eicr> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  public List<Eicr> getEicrData(Map<String, String> searchParams) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<Eicr> cq = cb.createQuery(Eicr.class);
    Root<Eicr> root = cq.from(Eicr.class);
    List<Predicate> predicates = preparePredicate(cb, root, searchParams);
    predicates.add(cb.equal(root.get("id"), Integer.parseInt(searchParams.get("eicrId"))));

    Predicate[] predArr = new Predicate[predicates.size()];
    predArr = predicates.toArray(predArr);

    Predicate criteria = cb.and(predArr);
    cq.where(criteria);
    cq.orderBy(cb.desc(root.get("id")));

    Query<Eicr> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  public List<Eicr> getRRData(Map<String, String> searchParams) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<Eicr> cq = cb.createQuery(Eicr.class);
    Root<Eicr> root = cq.from(Eicr.class);
    List<Predicate> predicates = preparePredicate(cb, root, searchParams);
    predicates.add(cb.equal(root.get(RESPONSE_DOC_ID), Integer.parseInt(searchParams.get(RESPONSE_DOC_ID))));

    Predicate[] predArr = new Predicate[predicates.size()];
    predArr = predicates.toArray(predArr);

    Predicate criteria = cb.and(predArr);
    cq.where(criteria);
    cq.orderBy(cb.desc(root.get("id")));

    Query<Eicr> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  public List<Eicr> getEicrAndRRByXRequestId(String xRequestId) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<Eicr> cq = cb.createQuery(Eicr.class);
    Root<Eicr> root = cq.from(Eicr.class);
    cq.where(cb.equal(root.get(X_REQUEST_ID), xRequestId));

    Query<Eicr> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  @Override
  public Eicr getEicrByDocId(String docId) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<Eicr> cq = cb.createQuery(Eicr.class);
    Root<Eicr> root = cq.from(Eicr.class);
    cq.where(cb.equal(root.get(EICR_DOC_ID), docId));

    Query<Eicr> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  public static List<Predicate> preparePredicate(CriteriaBuilder cb, Root<Eicr> root, Map<String, String> searchParams) {
    List<Predicate> predicates = new ArrayList<Predicate>();

    if (searchParams.get(EICR_DOC_ID) != null) {
      predicates.add(cb.equal(root.get(EICR_DOC_ID), searchParams.get(EICR_DOC_ID)));
    }
    if (searchParams.get(FHIR_SERVER_URL) != null) {
      predicates.add(cb.equal(root.get(FHIR_SERVER_URL), searchParams.get(FHIR_SERVER_URL)));
    }
    if (searchParams.get(SET_ID) != null) {
      predicates.add(cb.equal(root.get(SET_ID), searchParams.get(SET_ID)));
    }
    if (searchParams.get("patientId") != null) {
      predicates.add(cb.equal(root.get("launchPatientId"), searchParams.get("patientId")));
    }
    if (searchParams.get(ENCOUNTER_ID) != null) {
      predicates.add(cb.equal(root.get(ENCOUNTER_ID), searchParams.get(ENCOUNTER_ID)));
    }
    if (searchParams.get("version") != null) {
      predicates.add(cb.equal(root.get("docVersion"), searchParams.get("version")));
    }
    if (searchParams.get(X_REQUEST_ID) != null) {
      predicates.add(cb.equal(root.get(X_REQUEST_ID), searchParams.get(X_REQUEST_ID)));
    }

    return predicates;
  }

  public void deleteEicr(Eicr eicr) {
    getSession().delete(eicr);
  }
}
