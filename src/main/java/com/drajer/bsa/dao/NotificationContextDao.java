package com.drajer.bsa.dao;

import com.drajer.bsa.model.NotificationContext;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 *
 *
 * <h1>NotificationContextDao Interface</h1>
 *
 * The NotificationContextDao Interface class defines the typical Create, Read, Update service
 * methods for NotificationContext.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
public interface NotificationContextDao {

  /**
   * Method to create or update a NotificationContext.
   *
   * @param hsd The HealthcareSettings details to be used for creation or updation.
   * @return Returns the NotificationContext created or updated.
   */
  public NotificationContext saveOrUpdate(NotificationContext nc);

  /**
   * Method to retrieve a NotificationContext.
   *
   * @param id The NotificationContext details to be retrieved based on the id.
   * @return Returns the NotificationContext for the provided id.
   */
  public NotificationContext getNotificationContextById(UUID id);

  /**
   * Method to retrieve a NotificationContext by Url.
   *
   * @param url The NotificationContext details to be retrieved based on the url.
   * @return Returns the NotificationContext for the provided url.
   */
  public NotificationContext getNotificationContextByUrl(String url);

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
  public NotificationContext getNotificationContextByUniqueConstraints(
      String url, String patientId, String notificationResourceId, String notificationResourceType);

  /**
   * Method to retrieve a NotificationContext by id, fhir_server_base_url, patient_id,
   * notification_resource_id,
   *
   * @param id The id to retrieve the notification context.
   * @param fhirServerBaseUrl The fhirServerBaseUrl to retieve the notification context.
   * @param patientId The patient Id to retrieve the notification context.
   * @param notificationResourceId The Notification Resource Id to retrieve the notification
   *     context.
   * @return Returns the NotificationContext for the provided unique constraints.
   */
  public List<NotificationContext> getNotificationContextData(
      UUID id, String fhirServerBaseUrl, String notificationResourceId, String patientId);

  /**
   * Method to retrieve a NotificationContext by id, fhir_server_base_url, patient_id,
   * notification_resource_id,
   *
   * @param id The id to retrieve the notification context.
   * @param searchParams The searchParams used filtering and retrieve context based on param
   *     provided .
   * @return Returns the NotificationContext for the provided unique constraints.
   */
  List<NotificationContext> getAllNotificationContext(UUID id, Map<String, String> searchParams);

  public List<NotificationContext> getNotificationContextForReprocessing(
      UUID id, Map<String, String> searchParams);

  /**
   * Method to delete the NotificationContext
   *
   * @param notificationContext The NotificationContext data from DB
   */
  public void delete(NotificationContext notificationContext);
}
