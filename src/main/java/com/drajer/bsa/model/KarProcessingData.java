package com.drajer.bsa.model;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.bsa.service.KarExecutionStateService;
import com.drajer.sof.utils.ResourceUtils;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 * <h1>KarProcessingData</h1>
 *
 * The KarProcessingData holds all the input and output data that is relevant during the application
 * of a KAR to a Patient.
 *
 * @author nbashyam
 */
public class KarProcessingData {

  private final Logger logger = LoggerFactory.getLogger(KarProcessingData.class);

  /** The Knowledge Artifact to be processed. */
  KnowledgeArtifact kar;

  /**
   * The KnowledgeArtifactStatus containing detailed information on how the knowledge artifact has
   * to be processed
   */
  KnowledgeArtifactStatus karStatus;

  /** The context data that was received via notification. */
  NotificationContext notificationContext;

  /** The context data that was received via notification. */
  ScheduledJobData scheduledJobData;

  /**
   * The healthcare setting which will provide the necessary data to access for applying the Kar.
   */
  HealthcareSetting healthcareSetting;

  /** The raw bundle that is received as part of the notification. */
  Bundle notificationBundle;

  /**
   * The data accessed and collected from the healthcare setting for applying the KAR by Resource
   * type.
   */
  HashMap<ResourceType, Set<Resource>> fhirInputDataByType;

  /**
   * The data accessed and collected from the healthcare setting for applying the KAR by FHIR Path
   * Context Variable. These are typically the variable ids used for the Data Requirement classes
   * specified in the PlanDefinition.
   */
  HashMap<String, Set<Resource>> fhirInputDataById;

  /**
   * These are the resources that are received by notification, stored by he FHIR Path context
   * variables. (For e.g an encounter received via Subscription notification would be called
   * %encounter)
   */
  HashMap<String, Resource> notificationContextResources;

  /**
   * The output produced by applying the KAR actions. The Key for the outer map is the Action Id
   * that produced the data. The Key for the inner map is the Resource Id of the Resource produced
   * by the Action
   */
  HashMap<String, HashMap<String, Resource>> actionOutputData;

  /**
   * The data accessed and collected from the healthcare setting for applying the KAR by FHIR Path
   * Context Variable. These are typically the variable ids used for the Data Requirement classes
   * specified in the PlanDefinition.
   */
  HashMap<String, Set<Resource>> actionOutputDataById;

  /** The status of each Action after its execution. */
  HashMap<String, BsaActionStatus> actionStatus;

  /** The data actually submitted to the TTP/PHA. */
  String submittedData;

  /** The response data received from the TTP/PHA. */
  String responseData;

  /** The Dao Helper that will help us save the ExecutionState */
  KarExecutionStateService karExecutionStateService;

  /** The Service Helper that will help us save the ExecutionState */
  EhrQueryService ehrQueryService;

  public void addActionOutput(String actionId, Resource res) {

    if (actionOutputData.containsKey(actionId)) {

      actionOutputData.get(actionId).put(res.getIdElement().getId(), res);

    } else {
      HashMap<String, Resource> resMap = new HashMap<>();
      resMap.put(res.getIdElement().getIdPart(), res);
      actionOutputData.put(actionId, resMap);
    }
  }

  public void addActionOutputById(String id, Resource res) {

    if (res != null) {

      if (actionOutputDataById.containsKey(id)) actionOutputDataById.get(id).add(res);
      else {
        Set<Resource> resources = new HashSet<Resource>();
        resources.add(res);
        actionOutputDataById.put(id, resources);
      }
    }
  }

  public void addNotifiedResource(String resId, Resource res) {}

  public Set<Resource> getResourcesByType(String type) {

    for (Map.Entry<ResourceType, Set<Resource>> entry : fhirInputDataByType.entrySet()) {

      if (entry.getKey().toString().contentEquals(type)) {
        return entry.getValue();
      }
    }

    return null;
  }

  public Set<Resource> getOutputDataById(String id) {

    if (actionOutputDataById != null && actionOutputDataById.containsKey(id)) {
      return actionOutputDataById.get(id);
    } else return null;
  }

  public void addActionStatus(String id, BsaActionStatus status) {

    if (actionStatus.containsKey(id)) {

      logger.error(" Should not have the same action already ");
    } else {

      actionStatus.put(id, status);
    }
  }

  public BsaActionStatus getActionStatusByType(ActionType type) {

    for (Map.Entry<String, BsaActionStatus> entry : actionStatus.entrySet()) {

      if (entry.getValue().getActionType() == ActionType.CheckTriggerCodes) {

        return entry.getValue();
      }
    }

    return null;
  }

  public void addResourcesByType(HashMap<ResourceType, Set<Resource>> res) {

    if (res != null && res.size() > 0) {

      logger.info(" Resource Sizes : {}", res.size());
      for (Map.Entry<ResourceType, Set<Resource>> entry : res.entrySet()) {

        if (fhirInputDataByType.containsKey(entry.getKey())) {
          Set<Resource> resources = fhirInputDataByType.get(entry.getKey());
          resources.addAll(entry.getValue());
          Set<Resource> uniqueResources =
              ResourceUtils.deduplicate(resources).stream().collect(Collectors.toSet());
          fhirInputDataByType.put(entry.getKey(), uniqueResources);
        } else fhirInputDataByType.put(entry.getKey(), entry.getValue());
      }
    }
  }

  public void addResourcesById(HashMap<String, Set<Resource>> res) {

    if (res != null && res.size() > 0) {

      logger.info(" Resource Sizes : {}", res.size());
      for (Map.Entry<String, Set<Resource>> entry : res.entrySet()) {

        if (fhirInputDataById.containsKey(entry.getKey())) {
          Set<Resource> resources = fhirInputDataById.get(entry.getKey());
          resources.addAll(entry.getValue());
          Set<Resource> uniqueResources =
              ResourceUtils.deduplicate(resources).stream().collect(Collectors.toSet());
          fhirInputDataById.put(entry.getKey(), uniqueResources);
        } else fhirInputDataById.put(entry.getKey(), entry.getValue());
      }
    }
  }

  public void resetResourcesById(HashMap<String, Set<Resource>> res) {

    if (res != null && res.size() > 0) {

      logger.info(" Resource Sizes : {}", res.size());
      for (Map.Entry<String, Set<Resource>> entry : res.entrySet()) {

        if (fhirInputDataById.containsKey(entry.getKey())) {
          fhirInputDataById.put(entry.getKey(), entry.getValue());
        } else fhirInputDataById.put(entry.getKey(), entry.getValue());
      }
    }
  }

  public KarProcessingData() {

    fhirInputDataByType = new HashMap<>();
    fhirInputDataById = new HashMap<>();
    actionOutputData = new HashMap<>();
    actionOutputDataById = new HashMap<>();
    actionStatus = new HashMap<>();
  }

  /**
   * The method returns a KarExecutionState object that can be used to store execution state in the
   * database when current processing is delayed due to timers and scheduled job constraints present
   * in the PlanDefinition.
   *
   * @return KarExecutionState
   */
  public KarExecutionState getKarExecutionState() {

    KarExecutionState st = new KarExecutionState();

    st.setNcId(this.getNotificationContext().getId());
    st.setHsFhirServerUrl(this.getHealthcareSetting().getFhirServerBaseURL());
    st.setKarUniqueId(this.getKar().getVersionUniqueId());

    return st;
  }

  public Bundle getInputResourcesAsBundle() {

    Bundle bund = null;

    if (fhirInputDataByType != null && fhirInputDataByType.size() > 0) {

      bund = new Bundle();

      for (Map.Entry<ResourceType, Set<Resource>> entry : fhirInputDataByType.entrySet()) {

        Set<Resource> res = entry.getValue();

        List<Resource> uniqueResources = ResourceUtils.deduplicate(res);

        for (Resource r : uniqueResources) {
          bund.addEntry(new BundleEntryComponent().setResource(r));
        }
      }
    }

    return bund;
  }

  public KnowledgeArtifact getKar() {
    return kar;
  }

  public void setKar(KnowledgeArtifact kar) {
    this.kar = kar;
  }

  public HashMap<ResourceType, Set<Resource>> getFhirInputData() {
    return fhirInputDataByType;
  }

  public void setFhirInputData(HashMap<ResourceType, Set<Resource>> fhirInputData) {
    this.fhirInputDataByType = fhirInputData;
  }

  public HashMap<String, HashMap<String, Resource>> getActionOutputData() {
    return actionOutputData;
  }

  public void setActionOutputData(HashMap<String, HashMap<String, Resource>> actionOutputData) {
    this.actionOutputData = actionOutputData;
  }

  public HashMap<String, BsaActionStatus> getActionStatus() {
    return actionStatus;
  }

  public void setActionStatus(HashMap<String, BsaActionStatus> actionStatus) {
    this.actionStatus = actionStatus;
  }

  public NotificationContext getNotificationContext() {
    return notificationContext;
  }

  public void setNotificationContext(NotificationContext notificationContext) {
    this.notificationContext = notificationContext;
  }

  public HealthcareSetting getHealthcareSetting() {
    return healthcareSetting;
  }

  public void setHealthcareSetting(HealthcareSetting healthcareSetting) {
    this.healthcareSetting = healthcareSetting;
  }

  public Bundle getNotificationBundle() {
    return notificationBundle;
  }

  public void setNotificationBundle(Bundle notificationBundle) {
    this.notificationBundle = notificationBundle;
  }

  public HashMap<ResourceType, Set<Resource>> getFhirInputDataByType() {
    return fhirInputDataByType;
  }

  public void setFhirInputDataByType(HashMap<ResourceType, Set<Resource>> fhirInputDataByType) {
    this.fhirInputDataByType = fhirInputDataByType;
  }

  public HashMap<String, Set<Resource>> getFhirInputDataById() {
    return fhirInputDataById;
  }

  public void setFhirInputDataById(HashMap<String, Set<Resource>> fhirInputDataById) {
    this.fhirInputDataById = fhirInputDataById;
  }

  public String getSubmittedData() {
    return submittedData;
  }

  public void setSubmittedData(String submittedData) {
    this.submittedData = submittedData;
  }

  public String getResponseData() {
    return responseData;
  }

  public void setResponseData(String responseData) {
    this.responseData = responseData;
  }

  public KarExecutionStateService getKarExecutionStateService() {
    return karExecutionStateService;
  }

  public void setKarExecutionStateService(KarExecutionStateService karExecutionStateService) {
    this.karExecutionStateService = karExecutionStateService;
  }

  public EhrQueryService getEhrQueryService() {
    return ehrQueryService;
  }

  public void setEhrQueryService(EhrQueryService ehrQueryService) {
    this.ehrQueryService = ehrQueryService;
  }

  public ScheduledJobData getScheduledJobData() {
    return scheduledJobData;
  }

  public void setScheduledJobData(ScheduledJobData scheduledJobData) {
    this.scheduledJobData = scheduledJobData;
  }

  public HashMap<String, Resource> getNotificationContextResources() {
    return notificationContextResources;
  }

  public void setNotificationContextResources(
      HashMap<String, Resource> notificationContextResources) {
    this.notificationContextResources = notificationContextResources;
  }

  public KnowledgeArtifactStatus getKarStatus() {
    return karStatus;
  }

  public void setKarStatus(KnowledgeArtifactStatus karStatus) {
    this.karStatus = karStatus;
  }

  public HashMap<String, Set<Resource>> getActionOutputDataById() {
    return actionOutputDataById;
  }

  public void setActionOutputDataById(HashMap<String, Set<Resource>> actionOutputDataById) {
    this.actionOutputDataById = actionOutputDataById;
  }
}
