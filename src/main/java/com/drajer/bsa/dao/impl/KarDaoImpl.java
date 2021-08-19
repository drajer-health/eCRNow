package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.KarDao;
import com.drajer.bsa.model.KnowledgeArtifiactRepository;
import com.drajer.ecrapp.dao.AbstractDao;
import java.util.List;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class KarDaoImpl extends AbstractDao implements KarDao {

  private final Logger logger = LoggerFactory.getLogger(HealthcareSettingsDaoImpl.class);

  @Override
  public KnowledgeArtifiactRepository saveOrUpdate(KnowledgeArtifiactRepository kar) {
    getSession().saveOrUpdate(kar);
    return kar;
  }

  @Override
  public KnowledgeArtifiactRepository getKARById(Integer id) {
    KnowledgeArtifiactRepository kar = getSession().get(KnowledgeArtifiactRepository.class, id);
    return kar;
  }

  @Override
  public KnowledgeArtifiactRepository getKARByUrl(String url) {
    Criteria criteria = getSession().createCriteria(KnowledgeArtifiactRepository.class);
    criteria.add(Restrictions.eq("fhirServerURL", url));
    KnowledgeArtifiactRepository kar = (KnowledgeArtifiactRepository) criteria.uniqueResult();
    return kar;
  }

  @Override
  public List<KnowledgeArtifiactRepository> getAllKARs() {
    Criteria criteria = getSession().createCriteria(KnowledgeArtifiactRepository.class);
    return criteria.addOrder(Order.desc("id")).list();
  }
}
