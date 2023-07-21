package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.transaction.Transactional;
import java.util.UUID;
import org.springframework.stereotype.Repository;

@Repository
@Transactional
public class NotificationContextDaoImpl extends AbstractDao implements NotificationContextDao {

  /**
   * Method to create or update a NotificationContext.
   *
   * @param hsd The HealthcareSettings details to be used for creation or updation.
   * @return Returns the NotificationContext created or updated.
   */
  @Override
  public NotificationContext saveOrUpdate(NotificationContext nc) {
    getSession().saveOrUpdate(nc);
    return nc;
  }

  /**
   * Method to retrieve a HealthcareSetting.
   *
   * @param id The NotificationContext details to be retrieved based on the id.
   * @return Returns the NotificationContext for the provided id.
   */
  @Override
  public NotificationContext getNotificationContextById(UUID id) {
    return getSession().get(NotificationContext.class, id);
  }

  /**
   * Method to retrieve a NotificationContext by Url.
   *
   * @param url The NotificationContext details to be retrieved based on the url.
   * @return Returns the NotificationContext for the provided url.
   */
  @Override
  public NotificationContext getNotificationContextByUrl(String url) {
    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<NotificationContext> query =
        criteriaBuilder.createQuery(NotificationContext.class);
    Root<NotificationContext> root = query.from(NotificationContext.class);

    Predicate condition = criteriaBuilder.equal(root.get("fhirServerBaseURL"), url);
    query.select(root).where(condition);

    return getSession().createQuery(query).uniqueResult();
  }
}
