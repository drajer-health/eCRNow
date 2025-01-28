package com.drajer.bsa.model;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatusList;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.BsaJobType;
import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.bsa.service.KarExecutionStateService;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.sof.utils.ResourceUtils;
import java.util.*;
import java.util.stream.Collectors;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Parameters;
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

  public static final String RESOURCE_SIZES = "Resource Sizes : {}";
  private final Logger logger = LoggerFactory.getLogger(KarProcessingData.class);

  public enum DebugDataType {
    TRIGGER,
    LOADING,
    STATE,
    ALL_INFO
  }

  public static final String TRIGGER_QUERY_FILE_NAME = "TriggerQueryBundle";
  public static final String LOADING_QUERY_FILE_NAME = "LoadingQueryBundle";
  public static final String ALL_DATA_FILE_NAME = "AllDataFile";

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
   * The data accessed and collected from the healthcare setting for applying the KAR by FHIR Path
   * Context Variable. The HashMap stores the ResourceType, to Pair of <ResourceId, Resource>, so
   * that we dont have to search for it again if needed.
   */
  HashMap<ResourceType, HashMap<String, Resource>> resourcesById;

  /**
   * The data to be used for specific condition evaluation. The map contains a mapping between the
   * actionId and the Parameters that will be used for evaluating the condition associated with the
   * action.
   */
  Map<String, Parameters> parametersForConditionEvaluation;

  /**
   * These are the resources that are received by notification, stored by the FHIR Path context
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

  /** The current status of each Action after its execution. */
  HashMap<String, List<BsaActionStatus>> actionStatus;

  /** The previous status of the actions that can be used for comparison */
  KarExecutionState previousState;

  /** The data actually submitted to the TTP/PHA in FHIR format. */
  String submittedFhirData;

  /** The data actually submitted to the TTP/PHA in CDA format. */
  String submittedCdaData;

  /** The public health message logged into the db as per the execution */
  PublicHealthMessage phm;

  /** The response data received from the TTP/PHA in FHIR format. */
  String fhirResponseData;

  /** The response data received from the TTP/PHA in CDA format. */
  String cdaResponseData;

  /** The Dao Helper that will help us save the ExecutionState */
  KarExecutionStateService karExecutionStateService;

  /** The Service Helper that will help us save the ExecutionState */
  EhrQueryService ehrQueryService;

  /** The X-Request ID header from the incoming request */
  String xRequestId;

  /** The X-Correlation ID header from the incoming request */
  String xCorrelationId;

  /**
   * The attribute helps to track the execution sequence of all the action statuses based on the
   * type of notification and indicates the NotificaitonContext Id or the Schedule Job Id.
   */
  private String executionSequenceId;

  /** The attribute holds the current trigger match status based on previous matches. */
  private CheckTriggerCodeStatusList currentTriggerMatchStatus;

  /** The attribute holds the last trigger match status based on previous matches. */
  private CheckTriggerCodeStatusList previousTriggerMatchStatus;

  /** The attribute holds the context encounter */
  private Encounter contextEncounter;

  /** The type of job to be executed on the infrastructure */
  private BsaJobType jobType;

  /** The token refresh threshold value for refreshing access tokens */
  private Integer tokenRefreshThreshold;

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
        Set<Resource> resources = new HashSet<>();
        resources.add(res);
        actionOutputDataById.put(id, resources);
      }
    }
  }

  public void addNotifiedResource(String resId, Resource res) {
    logger.info("addNotifiedResource:TO DO:{}{}", resId, res);
  }

  public Set<Resource> getResourcesByType(String type) {

    for (Map.Entry<ResourceType, Set<Resource>> entry : fhirInputDataByType.entrySet()) {

      if (entry.getKey().toString().contentEquals(type)) {
        return entry.getValue();
      }
    }

    return Collections.emptySet();
  }

  public Set<Resource> getResourcesByType(ResourceType type) {

    for (Map.Entry<ResourceType, Set<Resource>> entry : fhirInputDataByType.entrySet()) {

      if (entry.getKey() == type) {
        return entry.getValue();
      }
    }

    return Collections.emptySet();
  }

  public Set<Resource> getOutputDataById(String id) {

    if (actionOutputDataById != null && actionOutputDataById.containsKey(id)) {
      return actionOutputDataById.get(id);
    } else return Collections.emptySet();
  }

  public void addActionStatus(String id, BsaActionStatus status) {

    if (actionStatus.containsKey(id)) {

      actionStatus.get(id).add(status);
    } else {

      List<BsaActionStatus> statuses = new ArrayList<>();
      statuses.add(status);
      actionStatus.put(id, statuses);
    }
  }

  public List<BsaActionStatus> getActionStatusByType(ActionType type) {

    List<BsaActionStatus> statuses = new ArrayList<>();
    for (Map.Entry<String, List<BsaActionStatus>> entry : actionStatus.entrySet()) {

      List<BsaActionStatus> statusValues = entry.getValue();

      for (BsaActionStatus stat : statusValues) {

        if (stat.getActionType() == type) {

          statuses.add(stat);
        }
      }
    }

    return statuses;
  }

  public void addResourcesByType(Map<ResourceType, Set<Resource>> res) {

    if (res != null && res.size() > 0) {

      logger.info(RESOURCE_SIZES, res.size());

      for (Map.Entry<ResourceType, Set<Resource>> entry : res.entrySet()) {
        addResourcesByType(entry.getKey(), entry.getValue());
      }
    }
  }

  public void addResourcesByType(ResourceType type, Set<Resource> res) {

    if (res != null && !res.isEmpty()) {

      logger.info(" Resource Sizes : {}", res.size());

      if (fhirInputDataByType.containsKey(type)) {
        Set<Resource> resources = fhirInputDataByType.get(type);
        resources.addAll(res);
        Set<Resource> uniqueResources =
            ResourceUtils.deduplicate(resources).stream().collect(Collectors.toSet());

        fhirInputDataByType.put(type, uniqueResources);
      } else fhirInputDataByType.put(type, res);
    }
  }

  public void addResourceByType(ResourceType type, Resource res) {

    if (res != null) {

      if (fhirInputDataByType.containsKey(type)) {
        Set<Resource> resources = fhirInputDataByType.get(type);
        resources.add(res);
        Set<Resource> uniqueResources =
            ResourceUtils.deduplicate(resources).stream().collect(Collectors.toSet());

        fhirInputDataByType.put(type, uniqueResources);
      } else {
        Set<Resource> resources = new HashSet<>();
        resources.add(res);
        fhirInputDataByType.put(type, resources);
      }
    }
  }

  public void addResourcesById(String id, Set<Resource> res) {
    if (res != null && !res.isEmpty()) {
      fhirInputDataById.put(id, res);
    }
  }

  public void addResourceById(String dataReqId, Resource res) {

    if (fhirInputDataById.containsKey(dataReqId)) {
      Set<Resource> resources = fhirInputDataById.get(dataReqId);
      resources.add(res);
      Set<Resource> uniqueResources =
          ResourceUtils.deduplicate(resources).stream().collect(Collectors.toSet());
      fhirInputDataById.put(dataReqId, uniqueResources);
    } else {
      Set<Resource> resources = new HashSet<>();
      resources.add(res);
      fhirInputDataById.put(dataReqId, resources);
    }
  }

  public void addResourcesById(HashMap<String, Set<Resource>> res) {

    if (res != null && res.size() > 0) {

      logger.info(RESOURCE_SIZES, res.size());
      for (Map.Entry<String, Set<Resource>> entry : res.entrySet()) {

        if (fhirInputDataById.containsKey(entry.getKey())) {
          Set<Resource> resources = fhirInputDataById.get(entry.getKey());
          resources.addAll(entry.getValue());
          Set<Resource> uniqueResources =
              ResourceUtils.deduplicate(resources).stream().collect(Collectors.toSet());
          fhirInputDataById.put(entry.getKey(), uniqueResources);

          // store the resources by Id also so that we can retrieve faster.
          for (Resource r : uniqueResources) {
            storeResourceById(r.getIdElement().getIdPart(), r);
          }
        } else {
          fhirInputDataById.put(entry.getKey(), entry.getValue());

          for (Resource r : entry.getValue()) {
            storeResourceById(r.getIdElement().getIdPart(), r);
          }
        }
      }
    }
  }

  public void resetResourcesById(HashMap<String, Set<Resource>> res) {

    if (res != null && res.size() > 0) {

      logger.info(RESOURCE_SIZES, res.size());
      for (Map.Entry<String, Set<Resource>> entry : res.entrySet()) {

        if (fhirInputDataById.containsKey(entry.getKey())) {
          fhirInputDataById.put(entry.getKey(), entry.getValue());
        } else fhirInputDataById.put(entry.getKey(), entry.getValue());
      }
    }
  }

  public void storeResourceById(String id, Resource r) {

    if (resourcesById.containsKey(r.getResourceType())) {
      if (!resourcesById.get(r.getResourceType()).containsKey(id)) {
        resourcesById.get(r.getResourceType()).put(id, r);
      } else {
        logger.debug(" Resource already exists with id {}", id);
      }
    } else {
      HashMap<String, Resource> resources = new HashMap<>();
      resources.put(id, r);
      resourcesById.put(r.getResourceType(), resources);
    }
  }

  public Resource getResourceById(String id, ResourceType type) {

    if (resourcesById.containsKey(type)) {
      if (resourcesById.get(type).containsKey(id)) {
        return resourcesById.get(type).get(id);
      }
    }

    return null;
  }

  public KarProcessingData() {

    fhirInputDataByType = new HashMap<>();
    fhirInputDataById = new HashMap<>();
    actionOutputData = new HashMap<>();
    actionOutputDataById = new HashMap<>();
    actionStatus = new HashMap<>();
    parametersForConditionEvaluation = new HashMap<>();
    resourcesById = new HashMap<>();
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

  public HashMap<String, HashMap<String, Resource>> getActionOutputData() {
    return actionOutputData;
  }

  public void setActionOutputData(HashMap<String, HashMap<String, Resource>> actionOutputData) {
    this.actionOutputData = actionOutputData;
  }

  public HashMap<String, List<BsaActionStatus>> getActionStatus() {
    return actionStatus;
  }

  public void setActionStatus(HashMap<String, List<BsaActionStatus>> actionStatus) {
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

  public String getSubmittedCdaData() {
    return submittedCdaData;
  }

  public void setSubmittedCdaData(String submittedCdaData) {
    this.submittedCdaData = submittedCdaData;
  }

  public String getFhirResponseData() {
    return fhirResponseData;
  }

  public void setFhirResponseData(String fhirResponseData) {
    this.fhirResponseData = fhirResponseData;
  }

  public String getSubmittedFhirData() {
    return submittedFhirData;
  }

  public void setSubmittedFhirData(String submittedData) {
    this.submittedFhirData = submittedData;
  }

  public String getCdaResponseData() {
    return cdaResponseData;
  }

  public void setCdaResponseData(String cdaResponseData) {
    this.cdaResponseData = cdaResponseData;
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
    return this.notificationContextResources;
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

  public String getxRequestId() {
    return xRequestId;
  }

  public void setxRequestId(String xRequestId) {
    this.xRequestId = xRequestId;
  }

  public String getxCorrelationId() {
    return xCorrelationId;
  }

  public void setxCorrelationId(String xCorrelationId) {
    this.xCorrelationId = xCorrelationId;
  }

  public KarExecutionState getPreviousState() {
    return previousState;
  }

  public void setPreviousState(KarExecutionState previousState) {
    this.previousState = previousState;
  }

  public String getExecutionSequenceId() {
    return executionSequenceId;
  }

  public void setExecutionSequenceId(String executionSequenceId) {
    this.executionSequenceId = executionSequenceId;
  }

  public PublicHealthMessage getPhm() {
    return phm;
  }

  public void setPhm(PublicHealthMessage phm) {
    this.phm = phm;
  }

  public String getKarIdForCustomQueries() {

    return kar.getKarId() + "-" + kar.getKarVersion();
  }

  public String getContextPatientId() {

    return notificationContext.getPatientId();
  }

  public Encounter getContextEncounter() {
    return contextEncounter;
  }

  public void setContextEncounter(Encounter contextEncounter) {
    this.contextEncounter = contextEncounter;
  }

  public CheckTriggerCodeStatusList getCurrentTriggerMatchStatus() {
    return currentTriggerMatchStatus;
  }

  public void setCurrentTriggerMatchStatus(CheckTriggerCodeStatusList currentTriggerMatchStatus) {
    this.currentTriggerMatchStatus = currentTriggerMatchStatus;
  }

  public CheckTriggerCodeStatusList getPreviousTriggerMatchStatus() {
    return previousTriggerMatchStatus;
  }

  public void setPreviousTriggerMatchStatus(CheckTriggerCodeStatusList previousTriggerMatchStatus) {
    this.previousTriggerMatchStatus = previousTriggerMatchStatus;
  }

  public BsaJobType getJobType() {
    return jobType;
  }

  public void setJobType(BsaJobType jobType) {
    this.jobType = jobType;
  }

  public Integer getTokenRefreshThreshold() {
    return tokenRefreshThreshold;
  }

  public void setTokenRefreshThreshold(Integer tokenRefreshThreshold) {
    this.tokenRefreshThreshold = tokenRefreshThreshold;
  }

  public boolean isDataAlreadyFetched(String dataReqId, String relatedDataId) {

    boolean returnVal = false;

    String finalRelatedDataId = this.getKar().getFirstClassRelatedDataId(dataReqId);

    // Check if the data is already retrieved.
    if (fhirInputDataById.containsKey(dataReqId)
        || (relatedDataId != null && fhirInputDataById.containsKey(relatedDataId))
        || fhirInputDataById.containsKey(finalRelatedDataId)) {
      returnVal = true;
    }

    return returnVal;
  }

  public String getContextEncounterId() {

    if (notificationContext
        .getNotificationResourceType()
        .contentEquals(ResourceType.Encounter.toString()))
      return notificationContext.getNotificationResourceId();
    else return "";
  }

  public boolean hasValidAccessToken() {

    // Check to see if the token is at least valid for 20 seconds before reusing the token.
    Date expirationTimeThreshold =
        Date.from(new Date().toInstant().plusSeconds(tokenRefreshThreshold));
    Date tokenExpirationTime = this.getHealthcareSetting().getEhrAccessTokenExpirationTime();

    if (tokenExpirationTime != null
        && (tokenExpirationTime.compareTo(expirationTimeThreshold) > 0)) {
      logger.info("Access Token is Valid");

      return true;

    } else {
      return false;
    }
  }

  public String getAccessToken() {

    return this.getHealthcareSetting().getEhrAccessToken();
  }

  public Set<Resource> getResourcesById(String id) {
    return getFhirInputDataById().get(id);
  }

  public Set<Resource> getDataForId(String dataReqId, Map<String, String> relatedDataIds) {

    Set<Resource> resources = null;

    if (relatedDataIds != null && relatedDataIds.containsKey(dataReqId)) {

      resources = getDataForId(dataReqId, relatedDataIds.get(dataReqId));
    } else {
      resources = getDataForId(dataReqId, "");
    }

    return resources;
  }

  public Set<Resource> getDataForId(String id, String relatedDataId) {
    Set<Resource> resources = null;

    if (actionOutputDataById.containsKey(id)) {
      resources = actionOutputDataById.get(id);
    }

    if (relatedDataId != null && actionOutputDataById.containsKey(relatedDataId)) {

      resources = actionOutputDataById.get(relatedDataId);
    }

    if (resources == null) {

      String finalRelatedDataId = this.getKar().getFirstClassRelatedDataId(id);

      if (finalRelatedDataId != null
          && !finalRelatedDataId.isEmpty()
          && fhirInputDataById.containsKey(finalRelatedDataId)) {
        resources = fhirInputDataById.get(finalRelatedDataId);
      }
    }

    if (resources == null && fhirInputDataById.containsKey(id)) {
      resources = fhirInputDataById.get(id);
    }

    return resources;
  }

  public Map<String, Parameters> getParametersForConditionEvaluation() {
    return parametersForConditionEvaluation;
  }

  public void setParametersForConditionEvaluation(
      Map<String, Parameters> parametersForConditionEvaluation) {
    this.parametersForConditionEvaluation = parametersForConditionEvaluation;
  }

  public HashMap<String, Set<Resource>> getActionOutputDataById() {
    return actionOutputDataById;
  }

  public void setActionOutputDataById(HashMap<String, Set<Resource>> actionOutputDataById) {
    this.actionOutputDataById = actionOutputDataById;
  }

  public void addParameters(String actionId, Parameters params) {

    if (parametersForConditionEvaluation.containsKey(actionId)) {
      parametersForConditionEvaluation.replace(actionId, params);
    } else {
      parametersForConditionEvaluation.put(actionId, params);
    }
  }

  public Parameters getParametersByActionId(String actionId) {

    if (parametersForConditionEvaluation.containsKey(actionId)) {
      return parametersForConditionEvaluation.get(actionId);
    } else {
      return null;
    }
  }

  public Boolean hasNewTriggerCodeMatches() {

    Boolean retVal = false;

    if (currentTriggerMatchStatus != null && previousTriggerMatchStatus == null) {
      logger.info(" New Matches Found where as there were no matches previously");
      retVal = true;
    } else if (currentTriggerMatchStatus == null && previousTriggerMatchStatus != null) {
      logger.info(" No New Matches Found where as there were matches previously");
      retVal = false;
    } else if (currentTriggerMatchStatus == null && previousTriggerMatchStatus == null) {
      logger.info(" No New or Old Matches");
      retVal = false;
    } else {

      // Check if each of the new matches are present in the old ones.
      retVal = previousTriggerMatchStatus.compareCodes(currentTriggerMatchStatus);

      logger.info(
          ((retVal == true)
              ? "New Matches Found compared to old matches"
              : "No New Matches when compared to old Matches"));
    }

    return retVal;
  }

  public void saveDataToFile(DebugDataType dt, List<DataRequirement> dr, String filename) {

    Bundle bund = new Bundle();

    if (dr != null && (dt == DebugDataType.TRIGGER || dt == DebugDataType.LOADING)) {

      for (DataRequirement d : dr) {

        Set<Resource> resources = this.getDataForId(d.getId(), "");

        if (resources != null) {

          resources.forEach(
              resource -> bund.addEntry(new BundleEntryComponent().setResource(resource)));
        }
      }

      String outputFileName =
          filename
              + "_"
              + this.getNotificationContext().getPatientId()
              + "_"
              + this.getNotificationContext().getNotificationResourceId();

      logger.info(" Bundle Size for filename {} {}= ", outputFileName, bund.getEntry().size());

      BsaServiceUtils.saveFhirResourceToFile(bund, outputFileName);
    }
  }

  public boolean containsResourceWithId(String resourceId, ResourceType type) {

    if (resourcesById.containsKey(type)) {
      if (resourcesById.get(type).containsKey(resourceId)) {
        logger.debug(" Found resource Id for type {} and id {}", type, resourceId);
        return true;
      }
    }

    return false;
  }
}
