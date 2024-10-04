package com.drajer.ecrapp.dao;

import com.drajer.ecrapp.model.ScheduledTasks;
import java.util.List;

public interface SchedulerDao {

  List<ScheduledTasks> getScheduledTasks(String actionType, String launchId);

  ScheduledTasks saveOrUpdate(ScheduledTasks scheduledTasks);

  List<ScheduledTasks> getScheduledTasks();
}
