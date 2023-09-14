package com.drajer.bsa.service;

import com.drajer.bsa.model.NotificationContext;
import java.util.List;
import java.util.UUID;

/**
 *
 *
 * <h1>NotificationContextService Interface</h1>
 *
 * The NotificationContextService Interface class defines the typical Create, Read, Update service
 * methods for NotificationContext.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
public interface NotificationContextService {

  /**
   * Method to create or update a NotificationContext.
   *
   * @param nc The context that needs to be saved.
   * @return
   */
  public NotificationContext saveOrUpdate(NotificationContext nc);

  /**
   * Method to retrieve a NotificationContext by Id from DB.
   *
   * @param id The unique id for the NotificationContext in the DB.
   * @return
   */
  public NotificationContext getNotificationContext(UUID id);

  /**
   * Method to retrieve a NotificationContext by id, fhirServerBaseUrl, notificationResourceId and
   * patientId from DB
   *
   * @param id The unique id for the NotificationContext in the DB.
   * @param fhirServerBaseURL The fhirServerBaseURL for NotificationContext in the DB.
   * @param notificationResourceId The notificationResourceId for NotificationContext in the DB.
   * @param patientId The patientId for NotificationContext in the DB.
   * @return {@link NotificationContext}
   */
  public List<NotificationContext> getNotificationContextData(
      UUID id, String fhirServerBaseURL, String notificationResourceId, String patientId);

  /**
   * Method to delete NotificationContext by NotificationContext
   *
   * @param notificationContext The NotificationContext data from DB
   */
  public void delete(NotificationContext notificationContext);
}
