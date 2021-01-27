package com.drajer.ecrapp.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
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
}
