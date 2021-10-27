package com.drajer.ecrapp.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.SchedulerDao;
import com.drajer.ecrapp.model.ScheduledTasks;
import java.util.List;
import org.hibernate.Criteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class SchedulerDaoImpl extends AbstractDao implements SchedulerDao {

  private static final String TASK_INSTANCE = "task_instance";

  @Override
  public List<ScheduledTasks> getScheduledTasks(
      String actionType, String patientId, String encounterId, String launchId) {
    // String query = "select task_name,task_instance,execution_time from scheduled_tasks where ";
    Criteria criteria = getSession().createCriteria(ScheduledTasks.class);
    if (actionType != null) {
      criteria.add(Restrictions.ilike(TASK_INSTANCE, "%" + actionType + "%", MatchMode.ANYWHERE));
    }
    if (patientId != null) {
      criteria.add(Restrictions.ilike(TASK_INSTANCE, "%" + patientId + "%", MatchMode.ANYWHERE));
    }
    if (encounterId != null) {
      criteria.add(Restrictions.ilike(TASK_INSTANCE, "%" + encounterId + "%", MatchMode.ANYWHERE));
    }
    if (launchId != null) {
      criteria.add(Restrictions.ilike(TASK_INSTANCE, "%" + launchId + "%", MatchMode.ANYWHERE));
    }
    return criteria.list();
  }
}
