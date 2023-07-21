package com.drajer.ecrapp.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.SchedulerDao;
import com.drajer.ecrapp.model.ScheduledTasks;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.List;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class SchedulerDaoImpl extends AbstractDao implements SchedulerDao {

  private static final String TASK_INSTANCE = "task_instance";
  private static final String TASK_NAME = "task_name";

  @Override
  public List<ScheduledTasks> getScheduledTasks(String actionType, String launchId) {

    CriteriaBuilder criteriaBuilder = getSession().getCriteriaBuilder();
    CriteriaQuery<ScheduledTasks> query = criteriaBuilder.createQuery(ScheduledTasks.class);
    Root<ScheduledTasks> scheduledTasks = query.from(ScheduledTasks.class);

    String queryString = actionType + "_" + launchId + "_" + "_%";
    query.where(criteriaBuilder.equal(scheduledTasks.get(TASK_NAME), "EICRTask"));
    query.where(criteriaBuilder.like(scheduledTasks.get(TASK_INSTANCE), queryString));

    return getSession().createQuery(query).getResultList();
  }
}
