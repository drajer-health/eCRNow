package com.drajer.bsa.dao;

import com.drajer.bsa.model.NotificationContext;

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
  public NotificationContext getNotificationContextById(Integer id);

  /**
   * Method to retrieve a NotificationContext by Url.
   *
   * @param url The NotificationContext details to be retrieved based on the url.
   * @return Returns the NotificationContext for the provided url.
   */
  public NotificationContext getNotificationContextByUrl(String url);
}
