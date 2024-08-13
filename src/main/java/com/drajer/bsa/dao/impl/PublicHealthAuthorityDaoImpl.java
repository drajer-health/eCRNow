package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.PublicHealthAuthorityDao;
import com.drajer.bsa.model.PublicHealthAuthority;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.List;
import org.hibernate.query.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class PublicHealthAuthorityDaoImpl extends AbstractDao implements PublicHealthAuthorityDao {

  private final EntityManager em = getSession().getEntityManagerFactory().createEntityManager();

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
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthAuthority> cq = cb.createQuery(PublicHealthAuthority.class);
    Root<PublicHealthAuthority> root = cq.from(PublicHealthAuthority.class);
    cq.where(cb.equal(root.get("fhirServerBaseURL"), url));

    Query<PublicHealthAuthority> q = getSession().createQuery(cq);

    return q.uniqueResultOptional().orElse(null);
  }

  @Override
  public List<PublicHealthAuthority> getAllPublicHealthAuthority() {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<PublicHealthAuthority> cq = cb.createQuery(PublicHealthAuthority.class);
    Root<PublicHealthAuthority> root = cq.from(PublicHealthAuthority.class);

    Query<PublicHealthAuthority> q = getSession().createQuery(cq);
    cq.orderBy(cb.desc(root.get("id")));

    return q.getResultList();
  }
}
