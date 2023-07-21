package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.PublicHealthAuthorityDao;
import com.drajer.bsa.model.PublicHealthAuthority;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.List;
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
    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<PublicHealthAuthority> query =
        criteriaBuilder.createQuery(PublicHealthAuthority.class);
    Root<PublicHealthAuthority> root = query.from(PublicHealthAuthority.class);

    Predicate condition = criteriaBuilder.equal(root.get("fhirServerBaseURL"), url);
    query.select(root).where(condition).distinct(true);

    return getSession().createQuery(query).uniqueResultOptional().orElse(null);
  }

  @Override
  public List<PublicHealthAuthority> getAllPublicHealthAuthority() {
    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<PublicHealthAuthority> query =
        criteriaBuilder.createQuery(PublicHealthAuthority.class);
    Root<PublicHealthAuthority> root = query.from(PublicHealthAuthority.class);

    query.select(root).orderBy(criteriaBuilder.desc(root.get("id")));

    return getSession().createQuery(query).getResultList();
  }
}
