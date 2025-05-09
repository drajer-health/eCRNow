package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.ecrapp.dao.AbstractDao;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.hibernate.Criteria;
import org.hibernate.criterion.*;
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
    Criteria criteria = getSession().createCriteria(NotificationContext.class);
    criteria.add(Restrictions.eq("fhirServerBaseURL", url));
    return (NotificationContext) criteria.uniqueResult();
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
    Criteria criteria = getSession().createCriteria(NotificationContext.class);

    criteria.add(Restrictions.eq("fhirServerBaseURL", url));
    criteria.add(Restrictions.eq("patientId", patientId));
    criteria.add(Restrictions.eq("notificationResourceId", notificationResourceId));
    criteria.add(Restrictions.eq("notificationResourceType", notificationResourceType));
    return (NotificationContext) criteria.uniqueResult();
  }

  @Override
  public List<NotificationContext> getNotificationContextData(
      UUID id, String fhirServerBaseUrl, String notificationResourceId, String patientId) {
    Criteria criteria = getSession().createCriteria(NotificationContext.class);

    if (id != null) {
      criteria.add(Restrictions.eq("id", id));
    } else {
      if (fhirServerBaseUrl != null)
        criteria.add(Restrictions.eq("fhirServerBaseUrl", fhirServerBaseUrl));
      if (notificationResourceId != null)
        criteria.add(Restrictions.eq("notificationResourceId", notificationResourceId));
      if (patientId != null) criteria.add(Restrictions.eq("patientId", patientId));
    }
    return criteria.list();
  }

  @Override
  public List<NotificationContext> getAllNotificationContext(
      UUID id, Map<String, String> searchParams) {
    Criteria criteria = getSession().createCriteria(NotificationContext.class);

    if (id != null) {
      criteria.add(Restrictions.eq("id", id));
    } else {
      if (searchParams.get("fhirServerBaseURL") != null) {
        criteria.add(Restrictions.eq("fhirServerBaseUrl", searchParams.get("fhirServerBaseURL")));
      }
      if (searchParams.get("notificationResourceId") != null) {
        criteria.add(
            Restrictions.eq("notificationResourceId", searchParams.get("notificationResourceId")));
      }
      if (searchParams.get("patientId") != null) {
        criteria.add(Restrictions.eq("patientId", searchParams.get("patientId")));
      }

      if (searchParams.get("notificationProcessingStatus") != null) {
        String notificationProcessingStatus = searchParams.get("notificationProcessingStatus");
        List<String> searchStatusList =
            notificationProcessingStatus.contains(",")
                ? Arrays.asList(notificationProcessingStatus.split(","))
                : Arrays.asList(notificationProcessingStatus);

        criteria.add(Restrictions.in("notificationProcessingStatus", searchStatusList));
      }

      String startDate = searchParams.get("startDateTime");
      String endDate = searchParams.get("endDateTime");
      String lastUpdatedStartTime = searchParams.get("lastUpdatedStartTime");
      String lastUpdatedEndTime = searchParams.get("lastUpdatedEndTime");

      Date eicrStartDate = null;
      Date eicrEndDate = null;
      Date eicrLastUpdatedStartTime = null;
      Date eicrLastUpdatedEndTime = null;

      try {
        if (startDate != null) {
          eicrStartDate = new SimpleDateFormat("yyyy-MM-dd HH:mm").parse(startDate);
        }
        if (endDate != null) {
          eicrEndDate = new SimpleDateFormat("yyyy-MM-dd HH:mm").parse(endDate);
        }

        if (lastUpdatedStartTime != null) {
          eicrLastUpdatedStartTime =
              new SimpleDateFormat("yyyy-MM-dd HH:mm").parse(lastUpdatedStartTime);
        }
        if (lastUpdatedEndTime != null) {
          eicrLastUpdatedEndTime =
              new SimpleDateFormat("yyyy-MM-dd HH:mm").parse(lastUpdatedEndTime);
        }

      } catch (Exception e) {
        throw new ResponseStatusException(
            HttpStatus.BAD_REQUEST, "Invalid date format: " + e.getMessage());
      }

      if (eicrStartDate != null) {
        criteria.add(Restrictions.ge("encounterStartTime", eicrStartDate));
      }
      if (eicrEndDate != null) {
        criteria.add(Restrictions.le("encounterEndTime", eicrEndDate));
      }

      if (eicrLastUpdatedStartTime != null) {
        criteria.add(Restrictions.ge("lastUpdated", eicrLastUpdatedStartTime));
      }
      if (eicrLastUpdatedEndTime != null) {
        criteria.add(Restrictions.le("lastUpdated", eicrLastUpdatedEndTime));
      }
    }
    if (searchParams.get("limit") != null) {
      criteria.setMaxResults(Integer.parseInt(searchParams.get("limit")));
    }

    criteria.addOrder(Order.asc("lastUpdated"));

    return criteria.list();
  }

  @Override
  public List<NotificationContext> getNotificationContextForReprocessing(
      UUID id, Map<String, String> searchParams) {
    Date eicrLastUpdatedStartTime = null;
    Date eicrLastUpdatedEndTime = null;
    Integer limit;

    limit = searchParams.get("limit") != null ? Integer.parseInt(searchParams.get("limit")) : 10;

    Criteria criteria = getSession().createCriteria(NotificationContext.class, "f");

    DetachedCriteria subquery =
        DetachedCriteria.forClass(NotificationContext.class, "nc")
            .setProjection(Projections.property("nc.notificationResourceId"))
            .add(Restrictions.eqProperty("nc.notificationResourceId", "f.notificationResourceId"))
            .add(Restrictions.ne("nc.notificationProcessingStatus", "FAILED"))
            /*  .add(
                 Restrictions.in(
                     "nc.notificationProcessingStatus", Arrays.asList("FAILED")))

            */
            .add(Restrictions.gtProperty("nc.lastUpdated", "f.lastUpdated"));

    String lastUpdatedStartTime = searchParams.get("lastUpdatedStartTime");
    String lastUpdatedEndTime = searchParams.get("lastUpdatedEndTime");

    try {

      if (lastUpdatedStartTime != null) {
        eicrLastUpdatedStartTime =
            new SimpleDateFormat("yyyy-MM-dd HH:mm").parse(lastUpdatedStartTime);
      }
      if (lastUpdatedEndTime != null) {
        eicrLastUpdatedEndTime = new SimpleDateFormat("yyyy-MM-dd HH:mm").parse(lastUpdatedEndTime);
      }

    } catch (Exception e) {
      throw new ResponseStatusException(
          HttpStatus.BAD_REQUEST, "Invalid date format: " + e.getMessage());
    }

    criteria.add(Restrictions.eq("f.notificationProcessingStatus", "FAILED"));
    if (eicrLastUpdatedStartTime != null) {
      criteria.add(Restrictions.ge("f.lastUpdated", eicrLastUpdatedStartTime));
    }

    String[] properties = {"f.notificationResourceId"};
    criteria.add(Subqueries.propertiesNotIn(properties, subquery));

    criteria.addOrder(Order.asc("f.lastUpdated"));

    List<NotificationContext> notificationContexts = criteria.list();

    Map<String, List<NotificationContext>> notificationsById =
        notificationContexts
            .stream()
            .sorted(Comparator.comparing(NotificationContext::getLastUpdated).reversed())
            .collect(Collectors.groupingBy(NotificationContext::getNotificationResourceId));
    return notificationsById
        .entrySet()
        .stream()
        .flatMap(
            entry -> {
              return IntStream.range(0, entry.getValue().size())
                  .mapToObj(i -> new AbstractMap.SimpleEntry<>(entry.getValue().get(i), i + 1));
            })
        .filter(
            notificationContextIntegerSimpleEntry ->
                notificationContextIntegerSimpleEntry.getValue() == 1)
        .map(Map.Entry::getKey)
        .sorted(Comparator.comparing(NotificationContext::getLastUpdated))
        .limit(limit)
        .collect(Collectors.toList());
  }

  @Override
  public void delete(NotificationContext notificationContext) {
    getSession().delete(notificationContext);
  }
}
