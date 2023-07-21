package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.KarDao;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.List;
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
    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<KnowledgeArtifactRepository> query =
        criteriaBuilder.createQuery(KnowledgeArtifactRepository.class);
    Root<KnowledgeArtifactRepository> root = query.from(KnowledgeArtifactRepository.class);

    query.select(root).where(criteriaBuilder.equal(root.get("fhirServerURL"), url));

    return getSession().createQuery(query).uniqueResult();
  }

  @Override
  public List<KnowledgeArtifactRepository> getAllKARs() {
    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<KnowledgeArtifactRepository> query =
        criteriaBuilder.createQuery(KnowledgeArtifactRepository.class);
    Root<KnowledgeArtifactRepository> root = query.from(KnowledgeArtifactRepository.class);

    query.select(root).orderBy(criteriaBuilder.desc(root.get("id")));

    return getSession().createQuery(query).getResultList();
  }

  @Override
  public KnowledgeArtifactStatus saveOrUpdateKARStatus(KnowledgeArtifactStatus karStatus) {
    getSession().saveOrUpdate(karStatus);
    return karStatus;
  }

  @Override
  public List<KnowledgeArtifactStatus> getKARStatusByHsId(Integer hsId) {
    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<KnowledgeArtifactStatus> query =
        criteriaBuilder.createQuery(KnowledgeArtifactStatus.class);
    Root<KnowledgeArtifactStatus> root = query.from(KnowledgeArtifactStatus.class);

    query.select(root).where(criteriaBuilder.equal(root.get("hsId"), hsId));

    return getSession().createQuery(query).getResultList();
  }

  @Override
  public KnowledgeArtifactStatus getKarStausByKarIdAndKarVersion(
      String karId, String karVersion, Integer hsId) {
    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<KnowledgeArtifactStatus> query =
        criteriaBuilder.createQuery(KnowledgeArtifactStatus.class);
    Root<KnowledgeArtifactStatus> root = query.from(KnowledgeArtifactStatus.class);

    Predicate karIdVersionPredicate =
        criteriaBuilder.equal(root.get("versionUniqueKarId"), karId + "|" + karVersion);
    Predicate hsIdPredicate = criteriaBuilder.equal(root.get("hsId"), hsId);
    query.select(root).where(criteriaBuilder.and(karIdVersionPredicate, hsIdPredicate));

    return getSession().createQuery(query).uniqueResult();
  }
}
