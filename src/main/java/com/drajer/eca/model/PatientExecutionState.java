package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.JobStatus;
import java.util.HashSet;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PatientExecutionState {

  private static final Logger logger = LoggerFactory.getLogger(PatientExecutionState.class);

  String patientId;
  String encounterId;
  MatchTriggerStatus matchTriggerStatus;
  CreateEicrStatus createEicrStatus;
  Set<PeriodicUpdateEicrStatus> periodicUpdateStatus;
  EventTypes.JobStatus periodicUpdateJobStatus;
  CloseOutEicrStatus closeOutEicrStatus;
  Set<ValidateEicrStatus> validateEicrStatus;
  Set<SubmitEicrStatus> submitEicrStatus;
  Set<RRStatus> rrStatus;

  public PatientExecutionState(String patId, String enId) {

    patientId = patId;
    encounterId = enId;

    matchTriggerStatus = new MatchTriggerStatus();

    createEicrStatus = new CreateEicrStatus();

    // Ignore Periodic Updates for now.
    periodicUpdateStatus = new HashSet<>();

    closeOutEicrStatus = new CloseOutEicrStatus();

    validateEicrStatus = new HashSet<>();

    submitEicrStatus = new HashSet<>();

    rrStatus = new HashSet<>();

    periodicUpdateJobStatus = JobStatus.NOT_STARTED;
  }

  public PatientExecutionState() {

    patientId = "";
    encounterId = "";

    matchTriggerStatus = new MatchTriggerStatus();

    createEicrStatus = new CreateEicrStatus();

    // Ignore Periodic Updates for now.
    periodicUpdateStatus = new HashSet<>();

    closeOutEicrStatus = new CloseOutEicrStatus();

    validateEicrStatus = new HashSet<>();

    submitEicrStatus = new HashSet<>();

    periodicUpdateJobStatus = JobStatus.NOT_STARTED;

    rrStatus = new HashSet<>();
  }

  public EventTypes.JobStatus getPeriodicUpdateJobStatus() {
    return periodicUpdateJobStatus;
  }

  public void setPeriodicUpdateJobStatus(EventTypes.JobStatus periodicUpdateJobStatus) {
    this.periodicUpdateJobStatus = periodicUpdateJobStatus;
  }

  public String getPatientId() {
    return patientId;
  }

  public void setPatientId(String patientId) {
    this.patientId = patientId;
  }

  public String getEncounterId() {
    return encounterId;
  }

  public void setEncounterId(String encounterId) {
    this.encounterId = encounterId;
  }

  public MatchTriggerStatus getMatchTriggerStatus() {
    return matchTriggerStatus;
  }

  public void setMatchTriggerStatus(MatchTriggerStatus matchTriggerStatus) {
    this.matchTriggerStatus = matchTriggerStatus;
  }

  public CreateEicrStatus getCreateEicrStatus() {
    return createEicrStatus;
  }

  public void setCreateEicrStatus(CreateEicrStatus createEicrStatus) {
    this.createEicrStatus = createEicrStatus;
  }

  public Set<PeriodicUpdateEicrStatus> getPeriodicUpdateStatus() {
    return periodicUpdateStatus;
  }

  public void setPeriodicUpdateStatus(Set<PeriodicUpdateEicrStatus> periodicUpdateStatus) {
    this.periodicUpdateStatus = periodicUpdateStatus;
  }

  public CloseOutEicrStatus getCloseOutEicrStatus() {
    return closeOutEicrStatus;
  }

  public void setCloseOutEicrStatus(CloseOutEicrStatus closeOutEicrStatus) {
    this.closeOutEicrStatus = closeOutEicrStatus;
  }

  public Set<ValidateEicrStatus> getValidateEicrStatus() {
    return validateEicrStatus;
  }

  public void setValidateEicrStatus(Set<ValidateEicrStatus> validateEicrStatus) {
    this.validateEicrStatus = validateEicrStatus;
  }

  public Set<SubmitEicrStatus> getSubmitEicrStatus() {
    return submitEicrStatus;
  }

  public void setSubmitEicrStatus(Set<SubmitEicrStatus> submitEicrStatus) {
    this.submitEicrStatus = submitEicrStatus;
  }

  public Set<RRStatus> getRrStatus() {
    return rrStatus;
  }

  public void setRrStatus(Set<RRStatus> rrStatus) {
    this.rrStatus = rrStatus;
  }

  public Boolean hasEicrBeenCreated() {

    Boolean retVal = false;
    if (createEicrStatus.getJobStatus() == JobStatus.COMPLETED) {
      logger.info(" Eicr has been creatd via Create Eicr Timer ");
      retVal = true;
    } else if (closeOutEicrStatus.getJobStatus() == JobStatus.COMPLETED) {
      logger.info(" Eicr has been creatd via Close Out Eicr Timer ");
      retVal = true;
    } else {

      for (PeriodicUpdateEicrStatus pd : periodicUpdateStatus) {

        if (pd.getJobStatus() == JobStatus.COMPLETED) {
          logger.info(" Found a Eicr created via Periodic timer");
          retVal = true;
        }
      }
    }

    return retVal;
  }

  public Boolean hasActionCompleted(String actionId) {

    JobStatus status = null;
    if (actionId.contentEquals(matchTriggerStatus.getActionId())) {
      // Add check to see if a trigger matched ...For testing because of lack of data the check is
      // omitted.
      status = matchTriggerStatus.getJobStatus();
    } else if (actionId.contentEquals(createEicrStatus.getActionId())) {

      status = createEicrStatus.getJobStatus();

      // When eicr has not been created, check to see if it was created through the periodic timer.
      if (status != JobStatus.COMPLETED) {
        for (PeriodicUpdateEicrStatus pd : periodicUpdateStatus) {

          if (pd.getJobStatus() == JobStatus.COMPLETED) {
            logger.info(
                " Create Eicr Job Status of Completed is also interpreted from Periodic Update");
            status = JobStatus.COMPLETED;
          }
        }
      }
    } else if (actionId.contentEquals(closeOutEicrStatus.getActionId())) {
      status = closeOutEicrStatus.getJobStatus();
    }

    if (status != null && status == JobStatus.COMPLETED) {
      return true;
    }

    for (PeriodicUpdateEicrStatus pd : periodicUpdateStatus) {

      if (actionId.contentEquals(pd.getActionId()) && pd.getJobStatus() == JobStatus.COMPLETED) {
        logger.info(" Found a Periodic Update that has been completed, hence returning true ");
        return true;
      }
    }

    for (ValidateEicrStatus vs : validateEicrStatus) {

      if (actionId.contentEquals(vs.getActionId()) && vs.getJobStatus() == JobStatus.COMPLETED) {
        return true;
      }
    }

    for (SubmitEicrStatus ss : submitEicrStatus) {

      if (actionId.contentEquals(ss.getActionId()) && ss.getJobStatus() == JobStatus.COMPLETED) {
        return true;
      }
    }

    return false;
  }

  public Set<Integer> getEicrIdForCompletedActions(String actionId) {

    Set<Integer> ids = new HashSet<>();

    if (actionId.contentEquals(createEicrStatus.getActionId())
        && createEicrStatus.getJobStatus() == JobStatus.COMPLETED) {

      ids.add(Integer.valueOf(createEicrStatus.geteICRId()));
    }

    if (actionId.contentEquals(closeOutEicrStatus.getActionId())
        && closeOutEicrStatus.getJobStatus() == JobStatus.COMPLETED) {
      ids.add(Integer.valueOf(closeOutEicrStatus.geteICRId()));
    }

    for (PeriodicUpdateEicrStatus pd : periodicUpdateStatus) {

      if (actionId.contentEquals(pd.getActionId()) && pd.getJobStatus() == JobStatus.COMPLETED) {
        ids.add(Integer.valueOf(pd.geteICRId()));
      }
    }

    return ids;
  }

  public Set<Integer> getEicrsReadyForValidation() {

    Set<Integer> ids = new HashSet<>();

    // Get the EICRs already validated.
    Set<Integer> valIds = new HashSet<>();
    Set<ValidateEicrStatus> vals = this.getValidateEicrStatus();
    for (ValidateEicrStatus val : vals) {

      // Collect the Ids.
      valIds.add(Integer.valueOf(val.geteICRId()));
    }

    if (this.getCreateEicrStatus().getJobStatus() == JobStatus.COMPLETED
        && !valIds.contains(Integer.valueOf(this.getCreateEicrStatus().geteICRId()))) {
      ids.add(Integer.valueOf(this.getCreateEicrStatus().geteICRId()));
    }

    if (this.getCloseOutEicrStatus().getJobStatus() == JobStatus.COMPLETED
        && !valIds.contains(Integer.valueOf(this.getCloseOutEicrStatus().geteICRId()))) {
      ids.add(Integer.valueOf(getCloseOutEicrStatus().geteICRId()));
    }

    for (PeriodicUpdateEicrStatus pd : periodicUpdateStatus) {

      if (pd.getJobStatus() == JobStatus.COMPLETED
          && !valIds.contains(Integer.valueOf(pd.geteICRId()))) {
        ids.add(Integer.valueOf(pd.geteICRId()));
      }
    }

    return ids;
  }

  public Set<Integer> getEicrsReadyForSubmission() {

    Set<Integer> ids = new HashSet<>();

    // Get the EICRs already validated.
    Set<Integer> valIds = new HashSet<>();
    Set<SubmitEicrStatus> vals = this.getSubmitEicrStatus();
    for (SubmitEicrStatus val : vals) {
      // Collect the Ids.
      valIds.add(Integer.valueOf(val.geteICRId()));
    }

    for (ValidateEicrStatus pd : validateEicrStatus) {

      if (pd.getJobStatus() == JobStatus.COMPLETED
          && !valIds.contains(Integer.valueOf(pd.geteICRId()))) {
        ids.add(Integer.valueOf(pd.geteICRId()));
      }
    }

    return ids;
  }

  public Set<Integer> getEicrsForRRCheck() {

    Set<Integer> ids = new HashSet<>();

    // Get the EICRs already validated.
    Set<Integer> valIds = new HashSet<>();
    Set<RRStatus> vals = this.getRrStatus();
    for (RRStatus val : vals) {
      // Collect the Ids.
      valIds.add(Integer.valueOf(val.geteICRId()));
    }

    for (SubmitEicrStatus pd : submitEicrStatus) {

      if (pd.getJobStatus() == JobStatus.COMPLETED
          && !valIds.contains(Integer.valueOf(pd.geteICRId()))) {
        ids.add(Integer.valueOf(pd.geteICRId()));
      }
    }

    return ids;
  }
}
