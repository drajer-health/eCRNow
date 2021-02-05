package com.drajer.ecrapp.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import java.util.List;
import java.util.Map;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class EicrDaoImpl extends AbstractDao implements EicrDao {

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
    Criteria criteria = getSession().createCriteria(Eicr.class);
    criteria.add(Restrictions.eq("fhirServerUrl", eicr.getFhirServerUrl()));
    criteria.add(Restrictions.eq("launchPatientId", eicr.getLaunchPatientId()));
    criteria.add(Restrictions.eq("encounterId", eicr.getEncounterId()));
    criteria.addOrder(Order.desc("docVersion"));
    criteria.setMaxResults(1);

    Eicr resultEicr = (Eicr) criteria.uniqueResult();

    if (resultEicr != null) {
      return resultEicr.getDocVersion();
    }
    return 0;
  }

  public Eicr getEicrByCoorrelationId(String xcoorrId) {
    Criteria criteria = getSession().createCriteria(Eicr.class);
    criteria.add(Restrictions.eq("xCorrelationId", xcoorrId));

    return (Eicr) criteria.uniqueResult();
  }

  public List<Eicr> getEicrData(Map<String, String> searchParams) {
    Criteria criteria = getSession().createCriteria(Eicr.class);
    if (searchParams.get("eicrId") != null) {
      criteria.add(Restrictions.eq("id", Integer.parseInt(searchParams.get("eicrId"))));
    }
    if (searchParams.get("eicrDocId") != null) {
      criteria.add(Restrictions.eq("eicrDocId", searchParams.get("eicrDocId")));
    }
    if (searchParams.get("setId") != null) {
      criteria.add(Restrictions.eq("setId", searchParams.get("setId")));
    }
    if (searchParams.get("patientId") != null) {
      criteria.add(Restrictions.eq("launchPatientId", searchParams.get("patientId")));
    }
    if (searchParams.get("encounterId") != null) {
      criteria.add(Restrictions.eq("encounterId", searchParams.get("encounterId")));
    }
    if (searchParams.get("version") != null) {
      criteria.add(Restrictions.eq("docVersion", Integer.parseInt(searchParams.get("version"))));
    }
    if (searchParams.get("fhirServerUrl") != null) {
      criteria.add(Restrictions.eq("fhirServerUrl", searchParams.get("fhirServerUrl")));
    }
    return criteria.addOrder(Order.desc("id")).list();
  }

  public List<Eicr> getRRData(Map<String, String> searchParams) {
    Criteria criteria = getSession().createCriteria(Eicr.class);
    if (searchParams.get("responseDocId") != null) {
      criteria.add(Restrictions.eq("responseDocId", searchParams.get("responseDocId")));
    }
    if (searchParams.get("eicrDocId") != null) {
      criteria.add(Restrictions.eq("eicrDocId", searchParams.get("eicrDocId")));
    }
    if (searchParams.get("fhirServerUrl") != null) {
      criteria.add(Restrictions.eq("fhirServerUrl", searchParams.get("fhirServerUrl")));
    }
    if (searchParams.get("setId") != null) {
      criteria.add(Restrictions.eq("setId", searchParams.get("setId")));
    }
    if (searchParams.get("patientId") != null) {
      criteria.add(Restrictions.eq("launchPatientId", searchParams.get("patientId")));
    }
    if (searchParams.get("encounterId") != null) {
      criteria.add(Restrictions.eq("encounterId", searchParams.get("encounterId")));
    }
    if (searchParams.get("version") != null) {
      criteria.add(Restrictions.eq("docVersion", Integer.parseInt(searchParams.get("version"))));
    }
    return criteria.addOrder(Order.desc("id")).list();
  }

  @Override
  public Eicr getEicrByDocId(String docId) {
    Criteria criteria = getSession().createCriteria(Eicr.class);
    criteria.add(Restrictions.eq("eicrDocId", docId));

    return (Eicr) criteria.uniqueResult();
  }
}
