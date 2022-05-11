package com.drajer.bsa.service;

import com.drajer.bsa.model.KarExecutionState;
import java.util.List;
import java.util.UUID;

/**
 *
 *
 * <h1>KarExecutionStateService Interface</h1>
 *
 * The KarExecutionStateService Interface class defines the typical Create, Read, Update service
 * methods for KarExecutionState.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
public interface KarExecutionStateService {

  /**
   * Method to create or update a KarExecutionState.
   *
   * @param kar
   * @return KarExecutionState that was updated
   */
  public KarExecutionState saveOrUpdate(KarExecutionState kar);

  /**
   * Method to retrieve a KarExecutionState by Id from DB.
   *
   * @param id The unique id for the KarExecutionState from the DB.
   * @return KarExecutionState that was retrieved or null.
   */
  public KarExecutionState getKarExecutionStateById(UUID id);

  /**
   * Method to to retrieve all KarExecutionStates
   *
   * @return List of all existing KarExecutionStates
   */
  public List<KarExecutionState> getAllKarExecutionStates();

  /**
   * Method to delete an Execution State
   *
   * @param state - KarExecutionState that needs to be deleted.
   */
  public void delete(KarExecutionState state);
}
