package com.drajer.bsa.service;

import com.drajer.bsa.model.NotificationContext;

/**
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
	 * @param nc	The context that needs to be saved.
	 * @return
	 */
	public NotificationContext saveOrUpdate(NotificationContext nc);

	/**
	 * Method to retrieve a NotificationContext by Id from DB.
	 * 
	 * @param id	The unique id for the NotificationContext in the DB.
	 * @return
	 */
	  public NotificationContext getNotificationContext(Integer id);

}
