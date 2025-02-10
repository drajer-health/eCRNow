package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.ecrapp.dao.AbstractDao;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
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
        criteria.add(
            Restrictions.ilike(
                "notificationProcessingStatus", searchParams.get("notificationProcessingStatus")));
      }

      String startDate = searchParams.get("startDate");
      String endDate = searchParams.get("endDate");
      Date eicrStartDate = null;
      Date eicrEndDate = null;

      try {
        if (startDate != null) {
          eicrStartDate = new SimpleDateFormat("yyyy-MM-dd").parse(startDate);
        }
        if (endDate != null) {
          eicrEndDate = new SimpleDateFormat("yyyy-MM-dd").parse(endDate);
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
    }

    return criteria.list();
  }

  @Override
  public void delete(NotificationContext notificationContext) {
    getSession().delete(notificationContext);
  }
}
