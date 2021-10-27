package com.drajer.ecrapp.service.impl;

import com.drajer.ecrapp.dao.SchedulerDao;
import com.drajer.ecrapp.model.ScheduledTasks;
import com.drajer.ecrapp.service.SchedulerService;
import java.util.List;
import javax.transaction.Transactional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class ScheduledServiceImpl implements SchedulerService {

  private final Logger logger = LoggerFactory.getLogger(ScheduledServiceImpl.class);

  @Autowired SchedulerDao schedulerDao;

  @Override
  public List<ScheduledTasks> getScheduledTasks(
      String actionType, String patientId, String encounterId, String launchId) {
    List<ScheduledTasks> tasksList =
        schedulerDao.getScheduledTasks(actionType, patientId, encounterId, launchId);
    return tasksList;
  }
}
