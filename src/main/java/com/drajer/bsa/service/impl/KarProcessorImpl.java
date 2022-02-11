package com.drajer.bsa.service.impl;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KarExecutionState;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.bsa.service.HealthcareSettingsService;
import com.drajer.bsa.service.KarExecutionStateService;
import com.drajer.bsa.service.KarProcessor;
import com.drajer.bsa.service.NotificationContextService;
import com.drajer.bsa.utils.BsaServiceUtils;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>KarProcessorImpl</h1>
 *
 * This interface declares methods to apply a (Knowledge Artifact) KAR to notifications received.
 *
 * @author nbashyam
 */
@Service
@Transactional
public class KarProcessorImpl implements KarProcessor {

  private final Logger logger = LoggerFactory.getLogger(KarProcessorImpl.class);

  @Autowired EhrQueryService ehrInterface;

  @Autowired BsaServiceUtils serviceUtils;

  @Autowired KarExecutionStateService karExecutionStateService;

  @Autowired NotificationContextService ncService;

  @Autowired HealthcareSettingsService hsService;

  @Autowired
  @Qualifier("jsonParser")
  IParser jsonParser;

  /**
   * The method that applies a KAR to a specific notification context.
   *
   * @param data The complete context required including the KAR to be applied for the notification.
   */
  @Override
  public void applyKarForNotification(KarProcessingData data) {

    // Get Kar for processing.
    KnowledgeArtifact kar = data.getKar();
    NotificationContext nc = data.getNotificationContext();
    String namedEvent = nc.getTriggerEvent();
    data.setEhrQueryService(ehrInterface);
    data.setKarExecutionStateService(karExecutionStateService);
    logger.info(" *** START Executing Trigger Actions *** ");
    Set<BsaAction> actions = kar.getActionsForTriggerEvent(namedEvent);

    for (BsaAction action : actions) {

      logger.info(" **** Executing Action Id {} **** ", action.getActionId());

      // Get the Resources that need to be retrieved.
      HashMap<String, ResourceType> resourceTypes = action.getInputResourceTypes();

      // Get necessary data to process.
      HashMap<ResourceType, Set<Resource>> res = ehrInterface.getFilteredData(data, resourceTypes);

      BsaActionStatus status = action.process(data, ehrInterface);

      data.addActionStatus(action.getActionId(), status);

      logger.info(" **** Finished Executing Action Id {} **** ", action.getActionId());
    }

    saveDataForDebug(data);

    logger.info(" *** END Executing Trigger Actions *** ");
  }

  /**
   * The method is used to save the data to a file so that it can help in debugging. This can be
   * turned on only during development and is not to be used for production purposes.
   *
   * @param kd The processing state captured during Knowledge Artifact processing.
   */
  public void saveDataForDebug(KarProcessingData kd) {

    HashMap<String, HashMap<String, Resource>> res = kd.getActionOutputData();

    for (Map.Entry<String, HashMap<String, Resource>> entry : res.entrySet()) {

      logger.info("Saving data to file for {}", entry.getKey());

      HashMap<String, Resource> resOutput = entry.getValue();

      for (Map.Entry<String, Resource> resEnt : resOutput.entrySet()) {

        logger.info(" Saving Data to file for {}", resEnt.getKey());
        serviceUtils.saveResourceToFile(resEnt.getValue());
      }
    }
  }

  /**
   * This method is the call back method that is provided for persistent timers that are scheduled
   * by the BSA. The timers provide the necessary contextual data from which the KarProcessingData
   * can be created and then the KAR can be applied based on the actions that need to be executed.
   *
   * @param data This is the ScheduledJobData context that is provided to the timer scheduler and
   *     retrieved as part of the call back.
   */
  @Override
  public void applyKarForScheduledJob(ScheduledJobData data) {

    logger.info(" Scheduled Job invoked via scheduler, Job Id : {}", data.getJobId());

    KarProcessingData kd = new KarProcessingData();
    KarExecutionState state =
        karExecutionStateService.getKarExecutionStateById(data.getKarExecutionStateId());

    NotificationContext nc = ncService.getNotificationContext(state.getNcId());

    // Setup Processing data
    kd.setNotificationContext(nc);
    kd.setHealthcareSetting(hsService.getHealthcareSettingByUrl(state.getHsFhirServerUrl()));
    kd.setKar(KnowledgeArtifactRepositorySystem.getInstance().getById(state.getKarUniqueId()));

    // Setup the Kar Status for the specific job.
    if (kd.getHealthcareSetting() != null && kd.getHealthcareSetting().getKars() != null) {

      // Get the Active Kars and process it.
      HealthcareSettingOperationalKnowledgeArtifacts arfts = kd.getHealthcareSetting().getKars();

      logger.info(
          " Processing HealthcareSetting Operational Knowledge Artifact Status Id : {}",
          arfts.getId());

      Set<KnowledgeArtifactStatus> stat = arfts.getArtifactStatus();

      for (KnowledgeArtifactStatus ks : stat) {

        if (ks.getIsActive() && ks.getVersionUniqueKarId().contentEquals(state.getKarUniqueId())) {

          logger.info(" Found unique Kar Status for KarId {}", state.getKarUniqueId());
          kd.setKarStatus(ks);
        }
      }

      if (kd.getKarStatus() == null) {

        logger.error("Cannot process job properly as KarStatus would be invalid.");
      }
    } else {
      logger.error("Cannot process job properly as KarStatus would be invalid.");
    }

    // Setup Notification Data
    Bundle nb = (Bundle) jsonParser.parseResource(nc.getNotificationData());
    kd.setNotificationBundle(nb);
    nc.setNotifiedResource(nb.getEntry().get(1).getResource());

    kd.setEhrQueryService(ehrInterface);
    kd.setKarExecutionStateService(karExecutionStateService);
    kd.setScheduledJobData(data);

    // Get the action that needs to be executed.
    BsaAction action = kd.getKar().getAction(data.getActionId());

    if (action != null) {
      logger.info(
          " **** START Executing Action with id {} based on scheduled job notification. **** ",
          action.getActionId());
      action.process(kd, ehrInterface);
      saveDataForDebug(kd);
      logger.info(
          " **** Finished Executing Action with id {} based on scheduled job notification. **** ",
          action.getActionId());
    } else {
      logger.error(
          " Cannot apply KAR for the scheduled job notification because action with id {} does not exist ",
          data.getActionId());
    }
  }
}
