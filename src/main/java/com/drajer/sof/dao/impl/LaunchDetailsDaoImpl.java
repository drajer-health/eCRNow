package com.drajer.sof.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.dao.LaunchDetailsDao;
import com.drajer.sof.model.LaunchDetails;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import org.hibernate.query.Query;
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
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<LaunchDetails> cq = cb.createQuery(LaunchDetails.class);
    Root<LaunchDetails> root = cq.from(LaunchDetails.class);

    Predicate criteria =
        cb.and(
            cb.equal(root.get("ehrServerURL"), fhirServerUrl),
            cb.equal(root.get("launchPatientId"), patient),
            cb.equal(root.get("encounterId"), encounter));
    cq.where(criteria);

    Query<LaunchDetails> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  public LaunchDetails getLaunchDetailsByState(int state) {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<LaunchDetails> cq = cb.createQuery(LaunchDetails.class);
    Root<LaunchDetails> root = cq.from(LaunchDetails.class);
    cq.where(cb.equal(root.get("launchState"), state));

    Query<LaunchDetails> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  public void delete(LaunchDetails launchDetails) {
    getSession().delete(launchDetails);
  }
}
