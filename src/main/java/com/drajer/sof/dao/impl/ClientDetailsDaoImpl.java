package com.drajer.sof.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.dao.ClientDetailsDao;
import com.drajer.sof.model.ClientDetails;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.List;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class ClientDetailsDaoImpl extends AbstractDao implements ClientDetailsDao {

  public ClientDetails saveOrUpdate(ClientDetails clientDetails) {
    getSession().saveOrUpdate(clientDetails);
    return clientDetails;
  }

  public ClientDetails getClientDetailsById(Integer id) {
    return getSession().get(ClientDetails.class, id);
  }

  public ClientDetails getClientDetailsByUrl(String url) {
    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<ClientDetails> query = criteriaBuilder.createQuery(ClientDetails.class);
    Root<ClientDetails> root = query.from(ClientDetails.class);

    Predicate condition = criteriaBuilder.equal(root.get("fhirServerBaseURL"), url);
    query.select(root).where(condition);

    return getSession().createQuery(query).uniqueResult();
  }

  public List<ClientDetails> getAllClientDetails() {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<ClientDetails> query = criteriaBuilder.createQuery(ClientDetails.class);
    Root<ClientDetails> root = query.from(ClientDetails.class);

    query.select(root).orderBy(criteriaBuilder.desc(root.get("id")));

    return getSession().createQuery(query).getResultList();
  }

  public void delete(ClientDetails clientDetails) {
    getSession().delete(clientDetails);
  }
}
