package com.drajer.ecrapp.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.SchedulerDao;
import com.drajer.ecrapp.model.ScheduledTasks;
import java.util.Collections;
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
  private static final String TASK_NAME = "task_name";

  @Override
  public List<ScheduledTasks> getScheduledTasks(String actionType, String launchId) {
    Criteria criteria = getSession().createCriteria(ScheduledTasks.class);

    String queryString = actionType + "_" + launchId + "_";

    criteria.add(Restrictions.eq(TASK_NAME, "EICRTask"));
    criteria.add(Restrictions.like(TASK_INSTANCE, queryString, MatchMode.START));

    return criteria.list();
  }

  @Override
  public List<ScheduledTasks> getScheduledTasks() {
    Criteria criteria = getSession().createCriteria(ScheduledTasks.class);

    List<ScheduledTasks> scheduledTasks = criteria.list();

    if (scheduledTasks != null && scheduledTasks.isEmpty()) {
      return scheduledTasks;
    }
    return Collections.EMPTY_LIST;
  }

  @Override
  public ScheduledTasks saveOrUpdate(ScheduledTasks scheduledTasks) {
    getSession().saveOrUpdate(scheduledTasks);
    return scheduledTasks;
  }
}
