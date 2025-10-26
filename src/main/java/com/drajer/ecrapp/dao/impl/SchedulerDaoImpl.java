package com.drajer.ecrapp.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.SchedulerDao;
import com.drajer.ecrapp.model.ScheduledTasks;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import java.util.Collections;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

import org.hibernate.query.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class SchedulerDaoImpl extends AbstractDao implements SchedulerDao {

  private static final String TASK_INSTANCE = "task_instance";
  private static final String TASK_NAME = "task_name";

  @Override
  public List<ScheduledTasks> getScheduledTasks(String actionType, String launchId) {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<ScheduledTasks> cq = cb.createQuery(ScheduledTasks.class);
    Root<ScheduledTasks> root = cq.from(ScheduledTasks.class);

    String queryString = actionType + "_" + launchId + "_";

    Predicate criteria =
        cb.and(
            cb.equal(root.get(TASK_NAME), "EICRTask"),
            cb.like(root.get("task_instance"), queryString));
    cq.where(criteria);

    Query<ScheduledTasks> q = getSession().createQuery(cq);

    return q.getResultList();
  }


    @Override
    public List<ScheduledTasks> getScheduledTasksBySearchQuery(String taskInstance) {
        CriteriaBuilder cb = getSession().getCriteriaBuilder();
        CriteriaQuery<ScheduledTasks> cq = cb.createQuery(ScheduledTasks.class);
        Root<ScheduledTasks> root = cq.from(ScheduledTasks.class);

        cq.select(root).distinct(true);

        if (StringUtils.isNotBlank(taskInstance)) {
            // Match anywhere (equivalent to MatchMode.ANYWHERE)
            String pattern = "%" + taskInstance.trim().toLowerCase() + "%";
            Predicate predicate = cb.like(cb.lower(root.get("taskInstance")), pattern);
            cq.where(predicate);
        }

        return getSession().createQuery(cq).getResultList();
    }


    @Override
  public List<ScheduledTasks> getScheduledTasks() {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<ScheduledTasks> cq = cb.createQuery(ScheduledTasks.class);

    Query<ScheduledTasks> q = getSession().createQuery(cq);

    List<ScheduledTasks> scheduledTasks =q.getResultList();
    if (scheduledTasks != null && !scheduledTasks.isEmpty()) {
      return scheduledTasks;
    }
    return Collections.EMPTY_LIST;
  }


  @Override
  public ScheduledTasks saveOrUpdate(ScheduledTasks scheduledTasks) {
    getSession().saveOrUpdate(scheduledTasks);
    return scheduledTasks;
  }

  @Override
  public ScheduledTasks delete(ScheduledTasks scheduledTasks) {
    getSession().delete(scheduledTasks);
    return scheduledTasks;
  }
}
