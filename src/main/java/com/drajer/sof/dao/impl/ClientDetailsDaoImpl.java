package com.drajer.sof.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.sof.dao.ClientDetailsDao;
import com.drajer.sof.model.ClientDetails;
import java.util.List;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
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
    Criteria criteria = getSession().createCriteria(ClientDetails.class);
    criteria.add(Restrictions.eq("fhirServerBaseURL", url));
    return (ClientDetails) criteria.uniqueResult();
  }

  public List<ClientDetails> getAllClientDetails() {
    Criteria criteria = getSession().createCriteria(ClientDetails.class);
    return criteria.addOrder(Order.desc("id")).list();
  }
}
