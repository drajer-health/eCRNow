package com.drajer.bsa.service.impl;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.exceptions.InvalidLaunchContext;
import com.drajer.bsa.exceptions.InvalidNotification;
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
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
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

  private final NotificationContextDao ncDao;
  private final HealthcareSettingsDao hsDao;
  private final KarProcessor karProcessor;
  private final KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;
  private final IParser jsonParser;

  /** The token refresh threshold value for refreshing access tokens */
  @Value("${token.refresh.threshold:25}")
  private Integer tokenRefreshThreshold;

  private final Logger logger = LoggerFactory.getLogger(SubscriptionNotificationReceiverImpl.class);

  /**
   * Instantiates a new subscription notification receiver implementation.
   *
   * @param ncDao the notification context DAO
   * @param hsDao the healthcare settings DAO
   * @param karProcessor the KAR processor
   * @param knowledgeArtifactRepositorySystem the knowledge artifact repository system
   * @param jsonParser the JSON parser (qualified as jsonParser)
   */
  @Autowired
  public SubscriptionNotificationReceiverImpl(
      NotificationContextDao ncDao,
      HealthcareSettingsDao hsDao,
      KarProcessor karProcessor,
      KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem,
      @Qualifier("jsonParser") IParser jsonParser) {
    this.ncDao = ncDao;
    this.hsDao = hsDao;
    this.karProcessor = karProcessor;
    this.knowledgeArtifactRepositorySystem = knowledgeArtifactRepositorySystem;
    this.jsonParser = jsonParser;
  }

  /**
   * Setup notification context with bundle data and throttle context.
   *
   * @param nc the notification context
   * @param notificationBundle the notification bundle
   * @param launchContext the launch context
   */
  private void setupNotificationContext(
      NotificationContext nc, Bundle notificationBundle, PatientLaunchContext launchContext) {
    nc.setNotificationData(jsonParser.encodeResourceToString(notificationBundle));
    if (launchContext != null && launchContext.getThrottleContext() != null) {
      nc.setThrottleContext(launchContext.getThrottleContext());
    }
    ncDao.saveOrUpdate(nc);
  }

  /**
   * Process knowledge artifact status.
   *
   * @param ks the knowledge artifact status
   * @param nc the notification context
   * @param hs the healthcare setting
   * @param notificationBundle the notification bundle
   * @param dataList the list to add processing data to
   */
  private void processKarStatus(
      KnowledgeArtifactStatus ks,
      NotificationContext nc,
      HealthcareSetting hs,
      Bundle notificationBundle,
      List<KarProcessingData> dataList) {
    if (!ks.getIsActive().booleanValue()) {
      logger.info(" Skipping processing of KAR as it is inactive. {}", ks.getVersionUniqueKarId());
      return;
    }

    logger.info(" Processing KAR with Id {} and version {}", ks.getKarId(), ks.getKarVersion());
    KnowledgeArtifact kar = knowledgeArtifactRepositorySystem.getById(ks.getVersionUniqueKarId());

    if (kar == null) {
      logger.error(
          " Unable to process notification, as the KAR is not found {}",
          ks.getVersionUniqueKarId());
      return;
    }

    logger.info(" Processing KAR since we found the one that we needed. ");
    KarProcessingData kd = setupKarProcessingData(ks, nc, hs, notificationBundle);
    addNotifiedResourceToKd(kd, nc);
    karProcessor.applyKarForNotification(kd);
    dataList.add(kd);
  }

  /**
   * Setup KAR processing data.
   *
   * @param ks the knowledge artifact status
   * @param nc the notification context
   * @param hs the healthcare setting
   * @param notificationBundle the notification bundle
   * @return the kar processing data
   */
  private KarProcessingData setupKarProcessingData(
      KnowledgeArtifactStatus ks,
      NotificationContext nc,
      HealthcareSetting hs,
      Bundle notificationBundle) {
    KarProcessingData kd = new KarProcessingData();
    KnowledgeArtifact kar = knowledgeArtifactRepositorySystem.getById(ks.getVersionUniqueKarId());
    kd.setNotificationContext(nc);
    kd.setHealthcareSetting(hs);
    kd.setKar(kar);
    kd.setNotificationBundle(notificationBundle);
    kd.setScheduledJobData(null);
    kd.setKarStatus(ks);
    kd.setxRequestId(nc.getxRequestId());
    kd.setxCorrelationId(nc.getxCorrelationId());
    kd.setTokenRefreshThreshold(tokenRefreshThreshold);
    return kd;
  }

  /**
   * Add notified resource to KAR processing data.
   *
   * @param kd the kar processing data
   * @param nc the notification context
   */
  private void addNotifiedResourceToKd(KarProcessingData kd, NotificationContext nc) {
    if (nc.getNotifiedResource() == null) {
      return;
    }

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

  /**
   * Process healthcare setting KARs.
   *
   * @param hs the healthcare setting
   * @param nc the notification context
   * @param notificationBundle the notification bundle
   * @param dataList the list to add processing data to
   */
  private void processHealthcareSettingKars(
      HealthcareSetting hs,
      NotificationContext nc,
      Bundle notificationBundle,
      List<KarProcessingData> dataList) {
    if (hs.getKars() == null) {
      logger.error(
          " Cannot proceed with the processing because the Healthcare Settings does not contain any Knowledge Artifacts that are operational.");
      return;
    }

    HealthcareSettingOperationalKnowledgeArtifacts arfts = hs.getKars();
    logger.info(
        " Processing HealthcareSetting Operational Knowledge Artifact Status Id : {}",
        arfts.getId());

    Set<KnowledgeArtifactStatus> stat = arfts.getArtifactStatus();
    for (KnowledgeArtifactStatus ks : stat) {
      processKarStatus(ks, nc, hs, notificationBundle, dataList);
    }
  }

  /** The method that processes the notification. */
  @Override
  public List<KarProcessingData> processNotification(
      Bundle notificationBundle,
      HttpServletRequest request,
      HttpServletResponse response,
      PatientLaunchContext launchContext)
      throws InvalidLaunchContext, InvalidNotification {

    List<KarProcessingData> dataList = new ArrayList<>();
    logger.info(" Starting to process launch notification ");

    NotificationContext nc =
        SubscriptionUtils.getNotificationContext(
            notificationBundle, request, response, false, false, launchContext);

    if (nc == null) {
      logger.error(
          " Cannot process notification because the Notification context is not derivable. ");
      return dataList;
    }

    logger.info(" Notification Context exists for processing the notification ");
    setupNotificationContext(nc, notificationBundle, launchContext);

    try {
      HealthcareSetting hs = hsDao.getHealthcareSettingByUrl(nc.getFhirServerBaseUrl());

      if (hs == null) {
        logger.error(
            " Cannot proceed with the processing because the Healthcare Settings does not exist for {}",
            nc.getFhirServerBaseUrl());
        return dataList;
      }

      logger.info(" Found the Healthcare Settings necessary to process notifications ");
      processHealthcareSettingKars(hs, nc, notificationBundle, dataList);

    } catch (Exception e) {
      logger.error(" Error during processing of notification.", e);
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
      Boolean relaunch)
      throws InvalidLaunchContext, InvalidNotification {
    return processNotificationInternal(
        notificationBundle,
        request,
        response,
        launchContext,
        true,
        false,
        NotificationProcessingStatusType.RELAUNCHED,
        "process");
  }

  @Override
  public List<KarProcessingData> reProcessNotification(
      Bundle notificationBundle,
      HttpServletRequest request,
      HttpServletResponse response,
      PatientLaunchContext launchContext,
      Boolean relaunch)
      throws InvalidLaunchContext, InvalidNotification {
    return processNotificationInternal(
        notificationBundle,
        request,
        response,
        launchContext,
        false,
        true,
        NotificationProcessingStatusType.REPROCESSED,
        "re-process");
  }

  private List<KarProcessingData> processNotificationInternal(
      Bundle notificationBundle,
      HttpServletRequest request,
      HttpServletResponse response,
      PatientLaunchContext launchContext,
      Boolean relaunch,
      Boolean reprocess,
      NotificationProcessingStatusType processingStatus,
      String operationType)
      throws InvalidLaunchContext, InvalidNotification {

    List<KarProcessingData> dataList = new ArrayList<>();
    logger.info(" Stating to {} notification ", operationType);

    NotificationContext nc =
        SubscriptionUtils.getNotificationContext(
            notificationBundle, request, response, relaunch, reprocess, launchContext);

    if (!validateNotificationContextExists(nc, operationType)) {
      throw new InvalidNotification(
          String.format(
              "Cannot %s notification because the Notification context is not derivable.",
              operationType));
    }

    setupNotificationContext(nc, notificationBundle, processingStatus, launchContext);

    try {
      processNotificationKars(nc, notificationBundle, dataList);
    } catch (Exception e) {
      logger.error(" Error during {} of notification.", operationType, e);
      throw e;
    }

    logger.info(" End {} notification ", operationType);
    return dataList;
  }

  private boolean validateNotificationContextExists(NotificationContext nc, String operationType) {
    if (nc == null) {
      logger.error(
          " Cannot {} notification because the Notification context is not derivable. ",
          operationType);
      return false;
    }
    return true;
  }

  private void setupNotificationContext(
      NotificationContext nc,
      Bundle notificationBundle,
      NotificationProcessingStatusType processingStatus,
      PatientLaunchContext launchContext) {
    logger.info(
        " Notification Context exists for {} the notification ",
        processingStatus.toString().toLowerCase());
    nc.setNotificationData(jsonParser.encodeResourceToString(notificationBundle));
    nc.setNotificationProcessingStatus(processingStatus.toString());

    if (launchContext != null && launchContext.getThrottleContext() != null) {
      nc.setThrottleContext(launchContext.getThrottleContext());
    }

    ncDao.saveOrUpdate(nc);
  }

  private void processNotificationKars(
      NotificationContext nc, Bundle notificationBundle, List<KarProcessingData> dataList)
      throws InvalidNotification {
    HealthcareSetting hs = hsDao.getHealthcareSettingByUrl(nc.getFhirServerBaseUrl());

    if (hs == null) {
      String err =
          " Cannot proceed with the processing because the Healthcare Settings does not exist for "
              + nc.getFhirServerBaseUrl();
      logger.error(err);
      throw new InvalidNotification(err);
    }

    logger.info(" Found the Healthcare Settings necessary to process notifications ");

    if (hs.getKars() == null) {
      String err =
          " Cannot proceed with the processing because the Healthcare Settings does not contain any Knowledge Artifacts that are operational.";
      logger.error(err);
      throw new InvalidNotification(err);
    }

    HealthcareSettingOperationalKnowledgeArtifacts arfts = hs.getKars();
    logger.info(
        " Processing HealthcareSetting Operational Knowledge Artifact Status Id : {}",
        arfts.getId());

    Set<KnowledgeArtifactStatus> stat = arfts.getArtifactStatus();
    for (KnowledgeArtifactStatus ks : stat) {
      processKarIfActive(ks, nc, notificationBundle, hs, dataList);
    }
  }

  private void processKarIfActive(
      KnowledgeArtifactStatus ks,
      NotificationContext nc,
      Bundle notificationBundle,
      HealthcareSetting hs,
      List<KarProcessingData> dataList)
      throws InvalidNotification {
    if (!ks.getIsActive().booleanValue()) {
      logger.info(" Skipping processing of KAR as it is inactive. {}", ks.getVersionUniqueKarId());
      return;
    }

    logger.info(" Processing KAR with Id {} and version {}", ks.getKarId(), ks.getKarVersion());

    KnowledgeArtifact kar = knowledgeArtifactRepositorySystem.getById(ks.getVersionUniqueKarId());

    if (kar == null) {
      String err =
          " Unable to process notification, as the KAR is not found " + ks.getVersionUniqueKarId();
      logger.error(err);
      throw new InvalidNotification(err);
    }

    logger.info(" Processing KAR since we found the one that we needed. ");
    KarProcessingData kd = setupKarProcessingData(nc, hs, kar, ks, notificationBundle);
    addNotifiedResourceToKarData(kd, nc);
    karProcessor.applyKarForNotification(kd);
    dataList.add(kd);
  }

  private KarProcessingData setupKarProcessingData(
      NotificationContext nc,
      HealthcareSetting hs,
      KnowledgeArtifact kar,
      KnowledgeArtifactStatus ks,
      Bundle notificationBundle) {
    KarProcessingData kd = new KarProcessingData();
    kd.setNotificationContext(nc);
    kd.setHealthcareSetting(hs);
    kd.setKar(kar);
    kd.setNotificationBundle(notificationBundle);
    kd.setScheduledJobData(null);
    kd.setKarStatus(ks);
    kd.setxRequestId(nc.getxRequestId());
    kd.setxCorrelationId(nc.getxCorrelationId());
    kd.setTokenRefreshThreshold(tokenRefreshThreshold);
    return kd;
  }

  private void addNotifiedResourceToKarData(KarProcessingData kd, NotificationContext nc) {
    if (nc.getNotifiedResource() == null) {
      return;
    }

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
}
