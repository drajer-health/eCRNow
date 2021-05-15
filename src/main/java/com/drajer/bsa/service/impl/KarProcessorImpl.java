package com.drajer.bsa.service.impl;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.service.KarProcessor;
import java.util.Set;
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

    Set<BsaAction> actions = kar.getActionsForTriggerEvent(namedEvent);

    for (BsaAction action : actions) {

      Set<ResourceType> types = action.getInputResourceTypes();

      // Get necessary data to process.
      ehrInterface.getFilteredData(data);

      action.process(data);
    }
  }
}
