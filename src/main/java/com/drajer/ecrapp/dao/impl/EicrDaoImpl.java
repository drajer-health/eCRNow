package com.drajer.ecrapp.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class EicrDaoImpl extends AbstractDao implements EicrDao {

  public static final String FHIR_SERVER_URL = "fhirServerUrl";
  public static final String ENCOUNTER_ID = "encounterId";
  public static final String EICR_DOC_ID = "eicrDocId";
  public static final String RESPONSE_DOC_ID = "responseDocId";
  public static final String SET_ID = "setId";
  public static final String X_REQUEST_ID = "xRequestId";

  public Eicr saveOrUpdate(Eicr eicr) {
    getSession().saveOrUpdate(eicr);
    ;
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

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<Eicr> query = criteriaBuilder.createQuery(Eicr.class);
    Root<Eicr> rootEicr = query.from(Eicr.class);

    query.where(criteriaBuilder.equal(rootEicr.get(FHIR_SERVER_URL), eicr.getFhirServerUrl()));
    query.where(criteriaBuilder.equal(rootEicr.get("launchPatientId"), eicr.getLaunchPatientId()));
    query.where(criteriaBuilder.equal(rootEicr.get(ENCOUNTER_ID), eicr.getEncounterId()));
    query.orderBy(criteriaBuilder.desc(rootEicr.get("docVersion")));

    TypedQuery<Eicr> typedQuery = getSession().createQuery(query);
    typedQuery.setMaxResults(1);

    Eicr resultEicr = typedQuery.getSingleResult();

    if (resultEicr != null) {
      return resultEicr.getDocVersion();
    }
    return 0;
  }

  public Eicr getEicrByCorrelationId(String xcoorrId) {
    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<Eicr> query = criteriaBuilder.createQuery(Eicr.class);
    Root<Eicr> eicr = query.from(Eicr.class);
    query.where(criteriaBuilder.equal(eicr.get("xCorrelationId"), "xcoorrId"));

    return getSession().createQuery(query).getSingleResult();
  }

  public List<Eicr> getEicrData(Map<String, String> searchParams) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<Eicr> query = criteriaBuilder.createQuery(Eicr.class);
    Root<Eicr> eicr = query.from(Eicr.class);
    if (searchParams.get("eicrId") != null) {
      query.where(
          criteriaBuilder.equal(eicr.get("id"), Integer.parseInt(searchParams.get("eicrId"))));
    }
    prepareCriteria(criteriaBuilder, query, eicr, searchParams);
    query.orderBy(criteriaBuilder.desc(eicr.get("id")));
    return getSession().createQuery(query).getResultList();
  }

  public List<Eicr> getRRData(Map<String, String> searchParams) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<Eicr> query = criteriaBuilder.createQuery(Eicr.class);
    Root<Eicr> eicr = query.from(Eicr.class);
    if (searchParams.get(RESPONSE_DOC_ID) != null) {
      query.where(
          criteriaBuilder.equal(eicr.get(RESPONSE_DOC_ID), searchParams.get(RESPONSE_DOC_ID)));
    }
    prepareCriteria(criteriaBuilder, query, eicr, searchParams);
    query.orderBy(criteriaBuilder.desc(eicr.get("id")));
    return getSession().createQuery(query).getResultList();
  }

  public List<Eicr> getEicrAndRRByXRequestId(String xRequestId) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<Eicr> query = criteriaBuilder.createQuery(Eicr.class);
    Root<Eicr> eicr = query.from(Eicr.class);
    query.where(criteriaBuilder.equal(eicr.get(X_REQUEST_ID), "xRequestId"));

    query.orderBy(criteriaBuilder.desc(eicr.get("id")));
    return getSession().createQuery(query).getResultList();
  }

  //
  @Override
  public Eicr getEicrByDocId(String docId) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<Eicr> query = criteriaBuilder.createQuery(Eicr.class);
    Root<Eicr> eicr = query.from(Eicr.class);
    query.where(criteriaBuilder.equal(eicr.get(EICR_DOC_ID), "docId"));

    return getSession().createQuery(query).getSingleResult();
  }

  public static void prepareCriteria(
      CriteriaBuilder criteriaBuilder,
      CriteriaQuery<Eicr> query,
      Root<Eicr> eicr,
      Map<String, String> searchParams) {

    if (searchParams.get(EICR_DOC_ID) != null) {
      query.where(criteriaBuilder.equal(eicr.get(EICR_DOC_ID), searchParams.get(EICR_DOC_ID)));
    }
    if (searchParams.get(FHIR_SERVER_URL) != null) {
      query.where(
          criteriaBuilder.equal(eicr.get(FHIR_SERVER_URL), searchParams.get(FHIR_SERVER_URL)));
    }
    if (searchParams.get(SET_ID) != null) {
      query.where(criteriaBuilder.equal(eicr.get(SET_ID), searchParams.get(SET_ID)));
    }
    if (searchParams.get("patientId") != null) {
      query.where(criteriaBuilder.equal(eicr.get("patientId"), searchParams.get("patientId")));
    }
    if (searchParams.get(ENCOUNTER_ID) != null) {
      query.where(criteriaBuilder.equal(eicr.get(ENCOUNTER_ID), searchParams.get(ENCOUNTER_ID)));
    }
    if (searchParams.get("version") != null) {
      query.where(criteriaBuilder.equal(eicr.get("version"), searchParams.get("version")));
    }
    if (searchParams.get(X_REQUEST_ID) != null) {
      query.where(criteriaBuilder.equal(eicr.get(X_REQUEST_ID), searchParams.get(X_REQUEST_ID)));
    }
  }

  public void deleteEicr(Eicr eicr) {
    getSession().delete(eicr);
  }
}
