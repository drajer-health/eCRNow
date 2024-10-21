package com.drajer.ecrapp.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.SchedulerDao;
import com.drajer.ecrapp.model.ScheduledTasks;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.List;
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
            cb.like(root.get("launchPatientId"), queryString));
    cq.where(criteria);

    Query<ScheduledTasks> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  @Override
  public List<ScheduledTasks> getScheduledTasks() {
    EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<ScheduledTasks> cq = cb.createQuery(ScheduledTasks.class);

    Query<ScheduledTasks> q = getSession().createQuery(cq);

    return q.getResultList();
  }

  @Override
  public ScheduledTasks saveOrUpdate(ScheduledTasks scheduledTasks) {
    getSession().saveOrUpdate(scheduledTasks);
    return scheduledTasks;
  }
}
