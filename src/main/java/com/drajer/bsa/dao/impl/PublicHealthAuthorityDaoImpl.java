package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.PublicHealthAuthorityDao;
import com.drajer.bsa.model.PublicHealthAuthority;
import com.drajer.ecrapp.dao.AbstractDao;
import java.util.List;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import org.hibernate.Session;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class PublicHealthAuthorityDaoImpl extends AbstractDao implements PublicHealthAuthorityDao {
  @Override
  public PublicHealthAuthority saveOrUpdate(PublicHealthAuthority pha) {
    getSession().saveOrUpdate(pha);
    return pha;
  }

  @Override
  public PublicHealthAuthority getPublicHealthAuthorityById(Integer id) {
    return getSession().get(PublicHealthAuthority.class, id);
  }

  @Override
  public PublicHealthAuthority getPublicHealthAuthorityByUrl(String url) {
    Session session = getSession();
    CriteriaBuilder cb = session.getCriteriaBuilder();
    CriteriaQuery<PublicHealthAuthority> criteria = cb.createQuery(PublicHealthAuthority.class);
    Root<PublicHealthAuthority> root = criteria.from(PublicHealthAuthority.class);
    criteria.select(root).where(cb.equal(root.get("fhirServerBaseURL"), url)).distinct(true);
    return session.createQuery(criteria).uniqueResultOptional().orElse(null);
  }

  @Override
  public List<PublicHealthAuthority> getAllPublicHealthAuthority() {
    Session session = getSession();
    CriteriaBuilder cb = session.getCriteriaBuilder();
    CriteriaQuery<PublicHealthAuthority> criteria = cb.createQuery(PublicHealthAuthority.class);
    Root<PublicHealthAuthority> root = criteria.from(PublicHealthAuthority.class);
    criteria.orderBy(cb.desc(root.get("id")));
    return session.createQuery(criteria).getResultList();
  }
}
