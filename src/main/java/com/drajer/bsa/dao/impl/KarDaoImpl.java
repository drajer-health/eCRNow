package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.KarDao;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import com.drajer.ecrapp.dao.AbstractDao;
import java.util.List;
import org.hibernate.Criteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
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
    Criteria criteria = getSession().createCriteria(KnowledgeArtifactRepository.class);
    criteria.add(Restrictions.like("fhirServerURL", url, MatchMode.EXACT));
    return (KnowledgeArtifactRepository) criteria.uniqueResult();
  }

  @Override
  public List<KnowledgeArtifactRepository> getAllKARs() {
    Criteria criteria = getSession().createCriteria(KnowledgeArtifactRepository.class);
    return criteria.addOrder(Order.desc("id")).list();
  }

  @Override
  public KnowledgeArtifactStatus saveOrUpdateKARStatus(KnowledgeArtifactStatus karStatus) {
    getSession().saveOrUpdate(karStatus);
    return karStatus;
  }

  @Override
  public List<KnowledgeArtifactStatus> getKARStatusByHsId(Integer hsId) {
    Criteria criteria = getSession().createCriteria(KnowledgeArtifactStatus.class);
    criteria.add(Restrictions.eq("hsId", hsId));
    List<KnowledgeArtifactStatus> kars = criteria.list();
    logger.info("Getting KAR Status by hsId. ");
    return kars;
  }

  @Override
  public KnowledgeArtifactStatus getKarStausByKarIdAndKarVersion(
      String karId, String karVersion, Integer hsId) {
    Criteria criteria = getSession().createCriteria(KnowledgeArtifactStatus.class);
    criteria.add(Restrictions.like("versionUniqueKarId", karId + "|" + karVersion,MatchMode.EXACT));
    criteria.add(Restrictions.like("hsId", hsId));
    KnowledgeArtifactStatus kars = (KnowledgeArtifactStatus) criteria.uniqueResult();
    logger.info("Getting KAR Status by using karId and karVersion. ");

    return kars;
  }
}
