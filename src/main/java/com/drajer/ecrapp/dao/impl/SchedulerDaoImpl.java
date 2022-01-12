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
  public List<ScheduledTasks> getScheduledTasks(String actionType, String launchId) {
    Criteria criteria = getSession().createCriteria(ScheduledTasks.class);

    String queryString = actionType + "_" + launchId + "_";

    criteria.add(Restrictions.ilike(TASK_INSTANCE, "%" + queryString + "%", MatchMode.START));

    return criteria.list();
  }
}
