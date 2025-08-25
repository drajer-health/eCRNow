package com.drajer.ecrapp.service;

import com.drajer.ecrapp.model.ScheduledTasks;
import java.io.IOException;
import java.util.List;

public interface SchedulerService {

  List<ScheduledTasks> getScheduledTasks(String actionType, String launchId);

  List<ScheduledTasks> getScheduledTasksBySearchQuery(String taskInstance);

  /**
   * Deletes scheduled tasks associated with the given FHIR server base URL, patient ID, and
   * encounter ID.
   *
   * <p>This method locates all scheduled tasks that match the provided context details, cancels
   * them in the scheduler, removes them from the database, and returns the list of deleted tasks.
   *
   * @param id The identifier for the delete operation (may be used for logging or tracking).
   * @param fhirServerBaseUrl The base URL of the FHIR server.
   * @param patientId The patient identifier.
   * @param encounterId The encounter identifier.
   * @return A list of {@link ScheduledTasks} objects that were deleted.
   * @throws IOException If an error occurs during task deletion or data processing.
   */
  List<ScheduledTasks> delete(
      String id, String fhirServerBaseUrl, String patientId, String encounterId) throws IOException;
}
