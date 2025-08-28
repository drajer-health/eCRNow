package com.drajer.bsa.dao;

import com.drajer.bsa.model.KarExecutionState;
import java.util.List;
import java.util.UUID;

/**
 *
 *
 * <h1>KarExecutionStateDao</h1>
 *
 * This interface declares methods to perform CRUD operations on the KarExecutionState.
 *
 * @author nbashyam
 */
public interface KarExecutionStateDao {

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
   * Method to to retrieve all Kars
   *
   * @return List of all existing KarExecutionStates
   */
  public List<KarExecutionState> getAllKarExecutionStates();

  /** @param state - KarExecutionState that needs to be deleted. */
  public void delete(KarExecutionState state);

  /**
   * Retrieves a list of execution IDs for {@link KarExecutionState} entities that match the given
   * patient ID, FHIR server base URL, and notification resource ID.
   *
   * <p>If any parameter is null, it will not be used as a filter.
   *
   * @param patientId The patient identifier to filter by (nullable).
   * @param fhirServerBaseUrl The FHIR server base URL to filter by (nullable).
   * @param notificationResourceId The notification resource identifier to filter by (nullable).
   * @return A list of String representing the matching KarExecutionState execution IDs.
   */
  List<String> getExecutionIdsByNotificationContextDetails(
      String patientId, String fhirServerBaseUrl, String notificationResourceId);
}
