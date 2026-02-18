package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.hibernate.Session;
import org.hibernate.query.Query;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

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
    /*
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<NotificationContext> cq = cb.createQuery(NotificationContext.class);
    Root<NotificationContext> root = cq.from(NotificationContext.class);
    cq.where(cb.equal(root.get("fhirServerBaseURL"), url));

    Query<NotificationContext> q = getSession().createQuery(cq);

    return q.uniqueResult();
    */
    try (EntityManager em = getSession().getEntityManagerFactory().createEntityManager()) {
      CriteriaBuilder cb = em.getCriteriaBuilder();
      CriteriaQuery<NotificationContext> cq = cb.createQuery(NotificationContext.class);
      Root<NotificationContext> root = cq.from(NotificationContext.class);
      cq.where(cb.equal(root.get("fhirServerBaseURL"), url));

      Query<NotificationContext> q = getSession().createQuery(cq);

      return q.uniqueResult();
    }
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
    /*
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
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

    Query<NotificationContext> q = getSession().createQuery(cq);

    return q.uniqueResult();
    */
    try (EntityManager em = getSession().getEntityManagerFactory().createEntityManager()) {
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

      Query<NotificationContext> q = getSession().createQuery(cq);

      return q.uniqueResult();
    }
  }

  @Override
  public List<NotificationContext> getNotificationContextData(
      UUID id, String fhirServerBaseUrl, String notificationResourceId, String patientId) {
    /*
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    List<Predicate> predicates = new ArrayList<>();
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
    */
    try (EntityManager em = getSession().getEntityManagerFactory().createEntityManager()) {
      List<Predicate> predicates = new ArrayList<>();
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
  }

  @Override
  public List<NotificationContext> getAllNotificationContext(
      UUID id, Map<String, String> searchParams) {
    Session session = getSession();
    CriteriaBuilder cb = session.getCriteriaBuilder();
    CriteriaQuery<NotificationContext> cq = cb.createQuery(NotificationContext.class);
    Root<NotificationContext> root = cq.from(NotificationContext.class);

    List<Predicate> predicates = new ArrayList<>();

    if (id != null) {
      predicates.add(cb.equal(root.get("id"), id));
    } else {
      if (searchParams.get("fhirServerBaseURL") != null) {
        predicates.add(
            cb.equal(root.get("fhirServerBaseUrl"), searchParams.get("fhirServerBaseURL")));
      }
      if (searchParams.get("notificationResourceId") != null) {
        predicates.add(
            cb.equal(
                root.get("notificationResourceId"), searchParams.get("notificationResourceId")));
      }
      if (searchParams.get("patientId") != null) {
        predicates.add(cb.equal(root.get("patientId"), searchParams.get("patientId")));
      }

      if (searchParams.get("notificationProcessingStatus") != null) {
        String notificationProcessingStatus = searchParams.get("notificationProcessingStatus");
        List<String> searchStatusList =
            notificationProcessingStatus.contains(",")
                ? Arrays.asList(notificationProcessingStatus.split(","))
                : Collections.singletonList(notificationProcessingStatus);
        predicates.add(root.get("notificationProcessingStatus").in(searchStatusList));
      }

      String startDate = searchParams.get("startDateTime");
      String endDate = searchParams.get("endDateTime");
      String lastUpdatedStartTime = searchParams.get("lastUpdatedStartTime");
      String lastUpdatedEndTime = searchParams.get("lastUpdatedEndTime");

      SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm");

      try {
        if (startDate != null) {
          Date eicrStartDate = sdf.parse(startDate);
          predicates.add(cb.greaterThanOrEqualTo(root.get("encounterStartTime"), eicrStartDate));
        }
        if (endDate != null) {
          Date eicrEndDate = sdf.parse(endDate);
          predicates.add(cb.lessThanOrEqualTo(root.get("encounterEndTime"), eicrEndDate));
        }
        if (lastUpdatedStartTime != null) {
          Date eicrLastUpdatedStartTime = sdf.parse(lastUpdatedStartTime);
          predicates.add(
              cb.greaterThanOrEqualTo(root.get("lastUpdated"), eicrLastUpdatedStartTime));
        }
        if (lastUpdatedEndTime != null) {
          Date eicrLastUpdatedEndTime = sdf.parse(lastUpdatedEndTime);
          predicates.add(cb.lessThanOrEqualTo(root.get("lastUpdated"), eicrLastUpdatedEndTime));
        }
      } catch (ParseException e) {
        throw new ResponseStatusException(
            HttpStatus.BAD_REQUEST, "Invalid date format: " + e.getMessage());
      }
    }

    cq.select(root)
        .where(cb.and(predicates.toArray(new Predicate[0])))
        .orderBy(cb.asc(root.get("lastUpdated")));

    Query<NotificationContext> query = session.createQuery(cq);

    if (searchParams.get("limit") != null) {
      query.setMaxResults(Integer.parseInt(searchParams.get("limit")));
    }

    return query.getResultList();
  }

  @Override
  public List<NotificationContext> getNotificationContextForReprocessing(
      UUID id, Map<String, String> searchParams) {

    Date lastUpdatedStart = null;
    Date lastUpdatedEnd = null;
    int limit =
        searchParams.get("limit") != null ? Integer.parseInt(searchParams.get("limit")) : 10;

    final String startParam = searchParams.get("lastUpdatedStartTime");
    final String endParam = searchParams.get("lastUpdatedEndTime");
    try {
      if (startParam != null) {
        lastUpdatedStart =
            new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").parse(startParam);
      }
      if (endParam != null) {
        lastUpdatedEnd = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").parse(endParam);
      }
    } catch (Exception e) {
      throw new org.springframework.web.server.ResponseStatusException(
          org.springframework.http.HttpStatus.BAD_REQUEST,
          "Invalid date format: " + e.getMessage());
    }

    var cb = getSession().getCriteriaBuilder();
    var cq = cb.createQuery(NotificationContext.class);
    var f = cq.from(NotificationContext.class); // alias "f" (FAILED row candidate)

    var notExists = cq.subquery(Long.class);
    var nc = notExists.from(NotificationContext.class);
    notExists
        .select(cb.literal(1L))
        .where(
            cb.equal(nc.get("notificationResourceId"), f.get("notificationResourceId")),
            cb.notEqual(nc.get("notificationProcessingStatus"), "FAILED"),
            cb.greaterThan(
                nc.<java.util.Date>get("lastUpdated"), f.<java.util.Date>get("lastUpdated")));

    java.util.List<jakarta.persistence.criteria.Predicate> preds = new java.util.ArrayList<>();
    preds.add(cb.equal(f.get("notificationProcessingStatus"), "FAILED"));

    if (lastUpdatedStart != null) {
      preds.add(cb.greaterThanOrEqualTo(f.<java.util.Date>get("lastUpdated"), lastUpdatedStart));
    }
    // (Your original code parsed end time but didn’t apply it; adding it here is usually intended.)
    if (lastUpdatedEnd != null) {
      preds.add(cb.lessThanOrEqualTo(f.<java.util.Date>get("lastUpdated"), lastUpdatedEnd));
    }

    preds.add(cb.not(cb.exists(notExists)));

    cq.select(f)
        .where(preds.toArray(new jakarta.persistence.criteria.Predicate[0]))
        .orderBy(cb.asc(f.get("lastUpdated")));

    var results = getSession().createQuery(cq).getResultList();

    var byId =
        results.stream()
            .sorted(java.util.Comparator.comparing(NotificationContext::getLastUpdated).reversed())
            .collect(
                java.util.stream.Collectors.groupingBy(
                    NotificationContext::getNotificationResourceId));

    return byId.entrySet().stream()
        .flatMap(
            e ->
                java.util.stream.IntStream.range(0, e.getValue().size())
                    .mapToObj(
                        i -> new java.util.AbstractMap.SimpleEntry<>(e.getValue().get(i), i + 1)))
        .filter(pair -> pair.getValue() == 1)
        .map(java.util.Map.Entry::getKey)
        .sorted(java.util.Comparator.comparing(NotificationContext::getLastUpdated))
        .limit(limit)
        .collect(java.util.stream.Collectors.toList());
  }

  @Override
  public void delete(NotificationContext notificationContext) {
    getSession().delete(notificationContext);
  }
}
