package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.hibernate.query.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>NotificationContextDaoImpl</h1>
 *
 * The NotificationContextDaoImpl class implements the Create, Read, Update service methods for
 * NotificationContext.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
@Repository
@Transactional
public class NotificationContextDaoImpl extends AbstractDao implements NotificationContextDao {

  private final EntityManager em = getSession().getEntityManagerFactory().createEntityManager();

  /**
   * Method to create or update a NotificationContext.
   *
   * @param nc The NotificationContext to be used for creation or update.
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
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<NotificationContext> cq = cb.createQuery(NotificationContext.class);
    Root<NotificationContext> root = cq.from(NotificationContext.class);
    cq.where(cb.equal(root.get("fhirServerBaseURL"), url));

    Query<NotificationContext> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  /**
   * Method to retrieve a NotificationContext by Url, patient_id, notification_resource_id,
   * notification_resource_type
   *
   * @param url The url to retrieve the notification context.
   * @param patientId The patient Id to retrieve the notification context.
   * @param notificationResourceId The Notification Resource Id to retrieve the notification
   *     context.
   * @param notificationResourceType The Notification Resource Type to retieve the notification
   *     context.
   * @return Returns the NotificationContext for the provided unique constraints.
   */
  @Override
  public NotificationContext getNotificationContextByUniqueConstraints(
      String url,
      String patientId,
      String notificationResourceId,
      String notificationResourceType) {

    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<NotificationContext> cq = cb.createQuery(NotificationContext.class);
    Root<NotificationContext> root = cq.from(NotificationContext.class);

    Predicate criteria =
        cb.and(
            cb.equal(root.get("fhirServerBaseURL"), url),
            cb.equal(root.get("patientId"), patientId),
            cb.equal(root.get("notificationResourceId"), notificationResourceId),
            cb.equal(root.get("notificationResourceType"), notificationResourceType));
    cq.where(criteria);
    cq.orderBy(cb.desc(root.get("docVersion")));

    Query<NotificationContext> q = getSession().createQuery(cq);

    return q.uniqueResult();
  }

  @Override
  public List<NotificationContext> getNotificationContextData(
      UUID id, String fhirServerBaseUrl, String notificationResourceId, String patientId) {
    List<Predicate> predicates = new ArrayList<Predicate>();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<NotificationContext> cq = cb.createQuery(NotificationContext.class);
    Root<NotificationContext> root = cq.from(NotificationContext.class);

    if (id != null) {
      predicates.add(cb.equal(root.get("id"), id));
    } else {
      if (fhirServerBaseUrl != null)
        predicates.add(cb.equal(root.get("fhirServerBaseURL"), fhirServerBaseUrl));
      if (notificationResourceId != null)
        predicates.add(cb.equal(root.get("notificationResourceId"), notificationResourceId));
      if (patientId != null) predicates.add(cb.equal(root.get("patientId"), patientId));
    }
    Predicate[] predArr = new Predicate[predicates.size()];
    predArr = predicates.toArray(predArr);

    Predicate criteria = cb.and(predArr);
    cq.where(criteria);

    Query<NotificationContext> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  @Override
  public void delete(NotificationContext notificationContext) {
    getSession().delete(notificationContext);
  }
}
