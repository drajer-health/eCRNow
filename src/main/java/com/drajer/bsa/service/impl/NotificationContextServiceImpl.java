package com.drajer.bsa.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.service.NotificationContextService;

/**
*
* <h1>NotificationContextServiceImpl</h1>
*
* The NotificationContextServiceImpl class implements the Create, Read, Update service methods for
* NotificationContext.
*
* @author nbashyam
* @since 2021-04-15
*/
@Service
@Transactional
public class NotificationContextServiceImpl implements NotificationContextService {

	@Autowired NotificationContextDao ncDao;
	
	/**
	 * Method to create or update a NotificationContext.
	 * 
	 * @param nc	The context that needs to be saved.
	 * @return
	 */
	@Override
	public NotificationContext saveOrUpdate(NotificationContext nc) {
		ncDao.saveOrUpdate(nc);
	    return nc;
	}

	/**
	 * Method to retrieve a NotificationContext by Id from DB.
	 * 
	 * @param id	The unique id for the NotificationContext in the DB.
	 * @return
	 */
	@Override
	public NotificationContext getNotificationContext(Integer id) {
		return ncDao.getNotificationContextById(id);
	}

}
