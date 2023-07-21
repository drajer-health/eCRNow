package com.drajer.sof.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.dao.LaunchDetailsDao;
import com.drajer.sof.model.LaunchDetails;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class LaunchDetailsDaoImpl extends AbstractDao implements LaunchDetailsDao {

  private final Logger logger = LoggerFactory.getLogger(LaunchDetailsDaoImpl.class);

  public LaunchDetails saveOrUpdate(LaunchDetails authDetails) {
    getSession().saveOrUpdate(authDetails);
    logger.info("Launch Details data successfully inserted in DB");
    return authDetails;
  }

  public LaunchDetails getAuthDetailsById(Integer id) {
    return getSession().get(LaunchDetails.class, id);
  }

  public LaunchDetails getLaunchDetailsByPatientAndEncounter(
      String patient, String encounter, String fhirServerUrl) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<LaunchDetails> query = criteriaBuilder.createQuery(LaunchDetails.class);
    Root<LaunchDetails> launchDetailsEntity = query.from(LaunchDetails.class);
    query.where(criteriaBuilder.equal(launchDetailsEntity.get("ehrServerURL"), fhirServerUrl));
    query.where(criteriaBuilder.equal(launchDetailsEntity.get("launchPatientId"), patient));
    query.where(criteriaBuilder.equal(launchDetailsEntity.get("encounterId"), encounter));
    return getSession().createQuery(query).getSingleResult();
  }

  public LaunchDetails getLaunchDetailsByState(int state) {
    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<LaunchDetails> query = criteriaBuilder.createQuery(LaunchDetails.class);
    Root<LaunchDetails> launchDetailsEntity = query.from(LaunchDetails.class);
    query.where(criteriaBuilder.equal(launchDetailsEntity.get("launchState"), state));
    return getSession().createQuery(query).getSingleResult();
  }

  public void delete(LaunchDetails launchDetails) {
    getSession().delete(launchDetails);
  }
}
