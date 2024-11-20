package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.KarDao;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.List;
import org.hibernate.query.Query;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class KarDaoImpl extends AbstractDao implements KarDao {

  private final Logger logger = LoggerFactory.getLogger(KarDaoImpl.class);

  @Override
  public KnowledgeArtifactRepository saveOrUpdate(KnowledgeArtifactRepository kar) {
    getSession().saveOrUpdate(kar);
    return kar;
  }

  @Override
  public KnowledgeArtifactRepository getKARById(Integer id) {
    return getSession().get(KnowledgeArtifactRepository.class, id);
  }

  @Override
  public KnowledgeArtifactRepository getKARByUrl(String url) {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<KnowledgeArtifactRepository> cq =
        cb.createQuery(KnowledgeArtifactRepository.class);
    Root<KnowledgeArtifactRepository> root = cq.from(KnowledgeArtifactRepository.class);
    cq.where(cb.equal(root.get("fhirServerURL"), url));

    Query<KnowledgeArtifactRepository> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  @Override
  public List<KnowledgeArtifactRepository> getAllKARs() {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<KnowledgeArtifactRepository> cq =
        cb.createQuery(KnowledgeArtifactRepository.class);
    Root<KnowledgeArtifactRepository> root = cq.from(KnowledgeArtifactRepository.class);

    Query<KnowledgeArtifactRepository> q = getSession().createQuery(cq);
    cq.orderBy(cb.desc(root.get("id")));

    return q.getResultList();
  }

  private List<KnowledgeArtifactRepository> removeKarsNotAvailable(
      List<KnowledgeArtifactRepository> repos) {

    if (repos != null) {
      repos.forEach(repo -> repo.removeArtifactsNotAvailable());
    }

    return repos;
  }

  @Override
  public KnowledgeArtifactStatus saveOrUpdateKARStatus(KnowledgeArtifactStatus karStatus) {
    getSession().saveOrUpdate(karStatus);
    return karStatus;
  }

  @Override
  public List<KnowledgeArtifactStatus> getKARStatusByHsId(Integer hsId) {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<KnowledgeArtifactStatus> cq = cb.createQuery(KnowledgeArtifactStatus.class);
    Root<KnowledgeArtifactStatus> root = cq.from(KnowledgeArtifactStatus.class);
    cq.where(cb.equal(root.get("hsId"), hsId));

    Query<KnowledgeArtifactStatus> q = getSession().createQuery(cq);

    logger.info("Getting KAR Status by hsId. ");

    return q.getResultList();
  }

  @Override
  public KnowledgeArtifactStatus getKarStausByKarIdAndKarVersion(
      String karId, String karVersion, Integer hsId) {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<KnowledgeArtifactStatus> cq = cb.createQuery(KnowledgeArtifactStatus.class);
    Root<KnowledgeArtifactStatus> root = cq.from(KnowledgeArtifactStatus.class);

    Predicate criteria =
        cb.and(
            cb.equal(root.get("launchPatientId"), karId + "|" + karVersion),
            cb.equal(root.get("hsId"), hsId));
    cq.where(criteria);
    cq.orderBy(cb.desc(root.get("docVersion")));

    Query<KnowledgeArtifactStatus> q = getSession().createQuery(cq);
    logger.info("Getting KAR Status by using karId and karVersion. ");

    return q.uniqueResult();
  }
}
