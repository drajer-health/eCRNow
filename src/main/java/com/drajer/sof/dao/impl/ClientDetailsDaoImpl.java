package com.drajer.sof.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.dao.ClientDetailsDao;
import com.drajer.sof.model.ClientDetails;
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
public class ClientDetailsDaoImpl extends AbstractDao implements ClientDetailsDao {

  private final EntityManager em = getSession().getEntityManagerFactory().createEntityManager();

  public ClientDetails saveOrUpdate(ClientDetails clientDetails) {
    getSession().saveOrUpdate(clientDetails);
    return clientDetails;
  }

  public ClientDetails getClientDetailsById(Integer id) {
    return getSession().get(ClientDetails.class, id);
  }

  public ClientDetails getClientDetailsByUrl(String url) {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<ClientDetails> cq = cb.createQuery(ClientDetails.class);
    Root<ClientDetails> root = cq.from(ClientDetails.class);
    cq.where(cb.equal(root.get("fhirServerBaseURL"), url));

    Query<ClientDetails> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  public List<ClientDetails> getAllClientDetails() {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<ClientDetails> cq = cb.createQuery(ClientDetails.class);
    Root<ClientDetails> root = cq.from(ClientDetails.class);

    Query<ClientDetails> q = getSession().createQuery(cq);
    cq.orderBy(cb.desc(root.get("id")));

    return q.getResultList();
  }

  public void delete(ClientDetails clientDetails) {
    getSession().delete(clientDetails);
  }
}
