package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.KarExecutionStateDao;
import com.drajer.bsa.model.KarExecutionState;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.List;
import java.util.UUID;
import org.hibernate.query.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>KarExecutionStateDao</h1>
 *
 * This Implementation class to perform CRUD operations on the KarExecutionState.
 *
 * @author nbashyam
 */
@Repository
@Transactional
public class KarExecutionStateDaoImpl extends AbstractDao implements KarExecutionStateDao {

  private final EntityManager em = getSession().getEntityManagerFactory().createEntityManager();

  /**
   * Method to create or update a KarExecutionState.
   *
   * @param kar
   * @return KarExecutionState that was updated
   */
  @Override
  public KarExecutionState saveOrUpdate(KarExecutionState kar) {
    getSession().saveOrUpdate(kar);
    return kar;
  }

  /**
   * Method to retrieve a KarExecutionState by Id from DB.
   *
   * @param id The unique id for the KarExecutionState from the DB.
   * @return KarExecutionState that was retrieved or null.
   */
  @Override
  public KarExecutionState getKarExecutionStateById(UUID id) {
    return getSession().get(KarExecutionState.class, id);
  }

  /**
   * Method to to retrieve all Kars
   *
   * @return List of all existing KarExecutionStates
   */
  @Override
  public List<KarExecutionState> getAllKarExecutionStates() {
    CriteriaBuilder cb = em.getCriteriaBuilder();
    CriteriaQuery<KarExecutionState> cq = cb.createQuery(KarExecutionState.class);
    Root<KarExecutionState> root = cq.from(KarExecutionState.class);

    Query<KarExecutionState> q = getSession().createQuery(cq);
    cq.orderBy(cb.desc(root.get("id")));

    return q.getResultList();
  }

  /** @param state - KarExecutionState that needs to be deleted. */
  @Override
  public void delete(KarExecutionState state) {
    getSession().delete(state);
  }
}
