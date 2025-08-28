package com.drajer.bsa.service.impl;

import com.drajer.bsa.dao.KarExecutionStateDao;
import com.drajer.bsa.model.KarExecutionState;
import com.drajer.bsa.service.KarExecutionStateService;
import java.util.List;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>KarExecutionStateServiceImpl</h1>
 *
 * The KarExecutionStateServiceImpl class implements the Create, Read, Update service methods for
 * KarExecutionState.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
@Service
@Transactional
public class KarExecutionStateServiceImpl implements KarExecutionStateService {

  @Autowired KarExecutionStateDao karExecutionStateDao;

  /**
   * Method to create or update a KarExecutionState.
   *
   * @param kar
   * @return KarExecutionState that was updated.
   */
  @Override
  public KarExecutionState saveOrUpdate(KarExecutionState kar) {
    karExecutionStateDao.saveOrUpdate(kar);
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
    return karExecutionStateDao.getKarExecutionStateById(id);
  }

  /**
   * Method to to retrieve all Kars
   *
   * @return List of all existing KarExecutionStates
   */
  @Override
  public List<KarExecutionState> getAllKarExecutionStates() {
    return karExecutionStateDao.getAllKarExecutionStates();
  }

  /** @param state - KarExecutionState that needs to be deleted. */
  @Override
  public void delete(KarExecutionState state) {
    karExecutionStateDao.delete(state);
  }

  @Override
  public List<String> getExecutionIdsByNotificationContextDetails(
      String patientId, String fhirServerBaseUrl, String notificationResourceId) {
    return karExecutionStateDao.getExecutionIdsByNotificationContextDetails(
        patientId, fhirServerBaseUrl, notificationResourceId);
  }
}
