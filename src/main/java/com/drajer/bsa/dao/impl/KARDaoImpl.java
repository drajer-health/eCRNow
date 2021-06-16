package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.KarDao;
import com.drajer.bsa.model.KAR;
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
public class KARDaoImpl extends AbstractDao implements KarDao {

  private final Logger logger = LoggerFactory.getLogger(HealthcareSettingsDaoImpl.class);

  @Override
  public KAR saveOrUpdate(KAR kar) {
    getSession().saveOrUpdate(kar);
    return kar;
  }

  @Override
  public KAR getKARById(Integer id) {
    KAR kar = getSession().get(KAR.class, id);
    return kar;
  }

  @Override
  public KAR getKARByUrl(String url) {
    Criteria criteria = getSession().createCriteria(KAR.class);
    criteria.add(Restrictions.eq("fhirServerURL", url));
    KAR kar = (KAR) criteria.uniqueResult();
    return kar;
  }

  @Override
  public List<KAR> getAllKARs() {
    Criteria criteria = getSession().createCriteria(KAR.class);
    return criteria.addOrder(Order.desc("id")).list();
  }
}
