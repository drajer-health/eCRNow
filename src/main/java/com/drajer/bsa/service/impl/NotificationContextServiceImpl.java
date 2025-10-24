package com.drajer.bsa.service.impl;

import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.service.NotificationContextService;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 *
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
   * @param nc The context that needs to be saved.
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
   * @param id The unique id for the NotificationContext in the DB.
   * @return
   */
  @Override
  public NotificationContext getNotificationContext(UUID id) {
    return ncDao.getNotificationContextById(id);
  }

  @Override
  public List<NotificationContext> getNotificationContextData(
      UUID id, String fhirServerBaseURL, String notificationResourceId, String patientId) {

    return ncDao.getNotificationContextData(
        id, fhirServerBaseURL, notificationResourceId, patientId);
  }

  @Override
  public List<NotificationContext> getAllNotificationContextData(
      UUID id, Map<String, String> searchParams) {

    return ncDao.getAllNotificationContext(id, searchParams);
  }

  @Override
  public List<NotificationContext> getNotificationContextForReprocessing(
      UUID id, Map<String, String> searchParams) {

    return ncDao.getNotificationContextForReprocessing(id, searchParams);
  }

  @Override
  public void delete(NotificationContext notificationContext) {
    ncDao.delete(notificationContext);
  }
}
