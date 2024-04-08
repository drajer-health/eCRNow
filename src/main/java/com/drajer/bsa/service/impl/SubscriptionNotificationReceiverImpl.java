package com.drajer.bsa.service.impl;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes.NotificationProcessingStatusType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PatientLaunchContext;
import com.drajer.bsa.service.KarProcessor;
import com.drajer.bsa.service.SubscriptionNotificationReceiver;
import com.drajer.bsa.utils.SubscriptionUtils;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * The implementation for processing subscription notifications.
 *
 * @author nbashyam
 */
@Service
@Transactional
public class SubscriptionNotificationReceiverImpl implements SubscriptionNotificationReceiver {

  @Autowired NotificationContextDao ncDao;

  @Autowired HealthcareSettingsDao hsDao;

  @Autowired KarProcessor karProcessor;

  @Autowired KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;

  @Autowired
  @Qualifier("jsonParser")
  IParser jsonParser;

  private final Logger logger = LoggerFactory.getLogger(SubscriptionNotificationReceiverImpl.class);

  /** The method that processes the notification. */
  @Override
  public List<KarProcessingData> processNotification(
      Bundle notificationBundle,
      HttpServletRequest request,
      HttpServletResponse response,
      PatientLaunchContext launchContext) {

    List<KarProcessingData> dataList = new ArrayList<>();
    logger.info(" Starting to process launch notification ");

    NotificationContext nc =
        SubscriptionUtils.getNotificationContext(
            notificationBundle, request, response, false, launchContext);

    if (nc != null) {

      logger.info(" Notification Context exists for processing the notification ");
      nc.setNotificationData(jsonParser.encodeResourceToString(notificationBundle));

      if (launchContext != null && launchContext.getThrottleContext() != null)
        nc.setThrottleContext(launchContext.getThrottleContext());

      ncDao.saveOrUpdate(nc);

      try {

        // Start processing the relaunch notification.

        // Retrieve the settings for the FHIR Server.
        HealthcareSetting hs = hsDao.getHealthcareSettingByUrl(nc.getFhirServerBaseUrl());

        if (hs != null) {

          logger.info(" Found the Healthcare Settings necessary to process notifications ");

          // Find the KAR's active for the Healthcare Setting.
          if (hs.getKars() != null) {

            // Get the Active Kars and process it.
            HealthcareSettingOperationalKnowledgeArtifacts arfts = hs.getKars();

            logger.info(
                " Processing HealthcareSetting Operational Knowledge Artifact Status Id : {}",
                arfts.getId());

            Set<KnowledgeArtifactStatus> stat = arfts.getArtifactStatus();

            for (KnowledgeArtifactStatus ks : stat) {

              if (ks.getIsActive().booleanValue()) {

                logger.info(
                    " Processing KAR with Id {} and version {}", ks.getKarId(), ks.getKarVersion());

                KnowledgeArtifact kar =
                    knowledgeArtifactRepositorySystem.getById(ks.getVersionUniqueKarId());

                if (kar != null) {

                  logger.info(" Processing KAR since we found the one that we needed. ");

                  // Setup the initial Kar
                  KarProcessingData kd = new KarProcessingData();
                  kd.setNotificationContext(nc);
                  kd.setHealthcareSetting(hs);
                  kd.setKar(kar);
                  kd.setNotificationBundle(notificationBundle);
                  kd.setScheduledJobData(null);
                  kd.setKarStatus(ks);
                  kd.setxRequestId(nc.getxRequestId());
                  kd.setxCorrelationId(nc.getxCorrelationId());

                  if (nc.getNotifiedResource() != null) {
                    logger.info("Adding notified resource to the set of inputs ");
                    Map<ResourceType, Set<Resource>> res = new EnumMap<>(ResourceType.class);
                    Set<Resource> results = new HashSet<>();
                    results.add(nc.getNotifiedResource());
                    res.put(nc.getNotifiedResource().getResourceType(), results);
                    kd.addResourcesByType(res);

                    if (nc.getNotifiedResource().getResourceType() == ResourceType.Encounter) {
                      kd.setContextEncounter((Encounter) nc.getNotifiedResource());
                    }
                  }

                  karProcessor.applyKarForNotification(kd);
                  dataList.add(kd);
                } else {

                  logger.error(
                      " Unable to process notification, as the KAR is not found {}",
                      ks.getVersionUniqueKarId());
                }

              } else {

                logger.info(
                    " Skipping processing of KAR as it is inactive. {}",
                    ks.getVersionUniqueKarId());
              }
            }

          } else {
            logger.error(
                " Cannot proceed with the processing because the Healthcare Settings does not contain any Knowledge Artifacts that are operational.");
          }

        } else {

          logger.error(
              " Cannot proceed with the processing because the Healthcare Settings does not exist for {}",
              nc.getFhirServerBaseUrl());
        }

      } catch (Exception e) {

        logger.error(" Error during processing of notification.", e);
      }

    } else {

      logger.error(
          " Cannot process notification because the Notification context is not derivable. ");
    }

    logger.info(" End processing notification ");
    return dataList;
  }

  @Override
  public List<KarProcessingData> processRelaunchNotification(
      Bundle notificationBundle,
      HttpServletRequest request,
      HttpServletResponse response,
      PatientLaunchContext launchContext,
      Boolean relaunch) {

    List<KarProcessingData> dataList = new ArrayList<>();
    logger.info(" Stating to process notification ");

    NotificationContext nc =
        SubscriptionUtils.getNotificationContext(
            notificationBundle, request, response, true, launchContext);

    if (nc != null) {

      logger.info(" Notification Context exists for processing the notification ");
      nc.setNotificationData(jsonParser.encodeResourceToString(notificationBundle));
      nc.setNotificationProcessingStatus(NotificationProcessingStatusType.RELAUNCHED.toString());

      if (launchContext != null && launchContext.getThrottleContext() != null)
        nc.setThrottleContext(launchContext.getThrottleContext());

      ncDao.saveOrUpdate(nc);

      try {

        // Start processing the notification.

        // Retrieve the settings for the FHIR Server.
        HealthcareSetting hs = hsDao.getHealthcareSettingByUrl(nc.getFhirServerBaseUrl());

        if (hs != null) {

          logger.info(" Found the Healthcare Settings necessary to process notifications ");

          // Find the KAR's active for the Healthcare Setting.
          if (hs.getKars() != null) {

            // Get the Active Kars and process it.
            HealthcareSettingOperationalKnowledgeArtifacts arfts = hs.getKars();

            logger.info(
                " Processing HealthcareSetting Operational Knowledge Artifact Status Id : {}",
                arfts.getId());

            Set<KnowledgeArtifactStatus> stat = arfts.getArtifactStatus();

            for (KnowledgeArtifactStatus ks : stat) {

              if (ks.getIsActive().booleanValue()) {

                logger.info(
                    " Processing KAR with Id {} and version {}", ks.getKarId(), ks.getKarVersion());

                KnowledgeArtifact kar =
                    knowledgeArtifactRepositorySystem.getById(ks.getVersionUniqueKarId());

                if (kar != null) {

                  logger.info(" Processing KAR since we found the one that we needed. ");

                  // Setup the initial Kar
                  KarProcessingData kd = new KarProcessingData();
                  kd.setNotificationContext(nc);
                  kd.setHealthcareSetting(hs);
                  kd.setKar(kar);
                  kd.setNotificationBundle(notificationBundle);
                  kd.setScheduledJobData(null);
                  kd.setKarStatus(ks);
                  kd.setxRequestId(nc.getxRequestId());
                  kd.setxCorrelationId(nc.getxCorrelationId());

                  if (nc.getNotifiedResource() != null) {
                    logger.info("Adding notified resource to the set of inputs ");
                    Map<ResourceType, Set<Resource>> res = new EnumMap<>(ResourceType.class);
                    Set<Resource> results = new HashSet<>();
                    results.add(nc.getNotifiedResource());
                    res.put(nc.getNotifiedResource().getResourceType(), results);
                    kd.addResourcesByType(res);

                    if (nc.getNotifiedResource().getResourceType() == ResourceType.Encounter) {
                      kd.setContextEncounter((Encounter) nc.getNotifiedResource());
                    }
                  }

                  karProcessor.applyKarForNotification(kd);
                  dataList.add(kd);
                } else {

                  logger.error(
                      " Unable to process notification, as the KAR is not found {}",
                      ks.getVersionUniqueKarId());
                }

              } else {

                logger.info(
                    " Skipping processing of KAR as it is inactive. {}",
                    ks.getVersionUniqueKarId());
              }
            }

          } else {
            logger.error(
                " Cannot proceed with the processing because the Healthcare Settings does not contain any Knowledge Artifacts that are operational.");
          }

        } else {

          logger.error(
              " Cannot proceed with the processing because the Healthcare Settings does not exist for {}",
              nc.getFhirServerBaseUrl());
        }

      } catch (Exception e) {

        logger.error(" Error during processing of notification.", e);
      }

    } else {

      logger.error(
          " Cannot process notification because the Notification context is not derivable. ");
    }

    logger.info(" End processing notification ");
    return dataList;
  }
}
