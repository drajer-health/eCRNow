package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.KarExecutionStateDao;
import com.drajer.bsa.model.KarExecutionState;
import com.drajer.ecrapp.dao.AbstractDao;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
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
    Criteria criteria = getSession().createCriteria(KarExecutionState.class);
    return criteria.addOrder(Order.desc("id")).list();
  }

  /** @param state - KarExecutionState that needs to be deleted. */
  @Override
  public void delete(KarExecutionState state) {
    getSession().delete(state);
  }

  @Override
  public List<String> getExecutionIdsByNotificationContextDetails(
      String patientId, String fhirServerBaseUrl, String notificationResourceId) {

    String hql =
        "select ks from KarExecutionState ks "
            + "left join NotificationContext nc on ks.ncId = nc.id "
            + "where (:patientId is null or nc.patientId = :patientId) "
            + "and (:notificationResourceId is null or nc.notificationResourceId = :notificationResourceId) "
            + "and (:fhirServerBaseUrl is null or nc.fhirServerBaseUrl = :fhirServerBaseUrl)";

    Query<KarExecutionState> query = getSession().createQuery(hql, KarExecutionState.class);
    query.setParameter("patientId", patientId);
    query.setParameter("notificationResourceId", notificationResourceId);
    query.setParameter("fhirServerBaseUrl", fhirServerBaseUrl);

    List<KarExecutionState> result = query.getResultList();
    List<String> ids =
        result
            .stream()
            .map(karExecutionState -> karExecutionState.getId().toString())
            .collect(Collectors.toList());
    return ids;
  }
}
