package com.drajer.bsa.service.impl;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.service.KarProcessor;
import com.drajer.bsa.utils.BsaServiceUtils;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>KarProcessor</h1>
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
}
