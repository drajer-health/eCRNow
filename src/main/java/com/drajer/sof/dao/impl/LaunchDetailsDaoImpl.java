package com.drajer.sof.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.dao.LaunchDetailsDao;
import com.drajer.sof.model.LaunchDetails;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class LaunchDetailsDaoImpl extends AbstractDao implements LaunchDetailsDao {

  public LaunchDetails saveOrUpdate(LaunchDetails authDetails) {
    getSession().saveOrUpdate(authDetails);
    return authDetails;
  }

  public LaunchDetails getAuthDetailsById(Integer id) {
    return getSession().get(LaunchDetails.class, id);
  }

  public LaunchDetails getLaunchDetailsByPatientAndEncounter(
      String patient, String encounter, String fhirServerUrl) {
    Criteria criteria = getSession().createCriteria(LaunchDetails.class);
    criteria.add(Restrictions.eq("ehrServerURL", fhirServerUrl));
    criteria.add(Restrictions.eq("launchPatientId", patient));
    criteria.add(Restrictions.eq("encounterId", encounter));
    return (LaunchDetails) criteria.uniqueResult();
  }

  public LaunchDetails getLaunchDetailsByState(int state) {
    Criteria criteria = getSession().createCriteria(LaunchDetails.class);
    criteria.add(Restrictions.eq("launchState", state));
    return (LaunchDetails) criteria.uniqueResult();
  }

  public void delete(LaunchDetails launchDetails) {
    getSession().delete(launchDetails);
  }
}
