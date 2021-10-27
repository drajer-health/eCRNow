package com.drajer.ecrapp.service;

import com.drajer.ecrapp.model.ScheduledTasks;
import java.util.List;

public interface SchedulerService {

  List<ScheduledTasks> getScheduledTasks(
      String actionType, String patientId, String encounterId, String launchId);
}
