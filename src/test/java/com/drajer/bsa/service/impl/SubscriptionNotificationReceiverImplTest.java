package com.drajer.bsa.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.dao.NotificationContextDao;
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
import com.drajer.test.util.TestUtils;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.util.ReflectionTestUtils;

public class SubscriptionNotificationReceiverImplTest {

  private static final String ACTIVE_KAR = "rctc-release-2023-02-03-Bundle-rctc|1.0.0";
  private static final String INACTIVE_KAR = "rctc-release-2022-01-01-Bundle-rctc|0.9.0";

  private NotificationContextDao ncDao;
  private HealthcareSettingsDao hsDao;
  private KarProcessor karProcessor;
  private KnowledgeArtifactRepositorySystem karRepository;
  private SubscriptionNotificationReceiverImpl receiver;
  private Bundle notification;
  private MockHttpServletRequest request;
  private MockHttpServletResponse response;
  private HealthcareSetting setting;
  private KnowledgeArtifact kar;

  @Before
  public void setUp() {
    ncDao = mock(NotificationContextDao.class);
    hsDao = mock(HealthcareSettingsDao.class);
    karProcessor = mock(KarProcessor.class);
    karRepository = mock(KnowledgeArtifactRepositorySystem.class);
    //    receiver =
    //     new SubscriptionNotificationReceiverImpl(
    //        ncDao, hsDao, karProcessor, karRepository, FhirContext.forR4Cached().newJsonParser());
    receiver = new SubscriptionNotificationReceiverImpl();
    receiver.ncDao = ncDao;
    receiver.hsDao = hsDao;
    receiver.karProcessor = karProcessor;
    receiver.knowledgeArtifactRepositorySystem = karRepository;
    receiver.jsonParser = FhirContext.forR4Cached().newJsonParser();
    ReflectionTestUtils.setField(receiver, "tokenRefreshThreshold", 25);

    notification =
        TestUtils.loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");
    notification.setType(BundleType.HISTORY);
    request = new MockHttpServletRequest();
    request.addHeader("X-Request-ID", "ecr-request-1");
    request.addHeader("X-Correlation-ID", "ecr-correlation-1");
    response = new MockHttpServletResponse();

    setting =
        (HealthcareSetting)
            TestUtils.getResourceAsObject("Bsa/HealthCareSettings.json", HealthcareSetting.class);
    kar = new KnowledgeArtifact();
    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(setting);
  }

  @Test
  public void testProcessNotification_appliesTheActiveKarToTheNotifiedEncounter() throws Exception {
    setting.setKars(kars(status(1, ACTIVE_KAR, true), status(2, INACTIVE_KAR, false)));
    when(karRepository.getById(ACTIVE_KAR)).thenReturn(kar);
    PatientLaunchContext launchContext = new PatientLaunchContext();
    launchContext.setThrottleContext("throttle-ctx");

    List<KarProcessingData> processed =
        receiver.processNotification(notification, request, response, launchContext);

    assertEquals(1, processed.size());
    KarProcessingData kd = processed.get(0);
    verify(karProcessor).applyKarForNotification(kd);
    verify(karRepository, never()).getById(INACTIVE_KAR);
    assertSame(kar, kd.getKar());
    assertSame(setting, kd.getHealthcareSetting());
    assertSame(notification, kd.getNotificationBundle());
    assertEquals(ACTIVE_KAR, kd.getKarStatus().getVersionUniqueKarId());
    assertEquals(Integer.valueOf(25), kd.getTokenRefreshThreshold());
    assertEquals("ecr-request-1", kd.getxRequestId());

    NotificationContext nc = kd.getNotificationContext();
    assertEquals("throttle-ctx", nc.getThrottleContext());
    assertEquals(
        NotificationProcessingStatusType.IN_PROGRESS.toString(),
        nc.getNotificationProcessingStatus());
    assertNotNull(nc.getNotificationData());
    verify(ncDao).saveOrUpdate(nc);

    Encounter notified = (Encounter) nc.getNotifiedResource();
    assertSame(notified, kd.getContextEncounter());
    assertTrue(kd.getResourcesByType(ResourceType.Encounter).contains(notified));
  }

  @Test
  public void testProcessNotification_returnsNothingWhenThereIsNothingToProcess() throws Exception {
    setting.setKars(kars(status(1, ACTIVE_KAR, true)));
    when(karRepository.getById(ACTIVE_KAR)).thenReturn(null);
    assertTrue(receiver.processNotification(notification, request, response, null).isEmpty());

    setting.setKars(null);
    assertTrue(receiver.processNotification(notification, request, response, null).isEmpty());

    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(null);
    assertTrue(receiver.processNotification(notification, request, response, null).isEmpty());

    verify(karProcessor, never()).applyKarForNotification(any());
  }

  @Test
  public void testProcessNotification_swallowsAKarProcessingFailure() throws Exception {
    setting.setKars(kars(status(1, ACTIVE_KAR, true)));
    when(karRepository.getById(ACTIVE_KAR)).thenReturn(kar);
    doThrow(new IllegalStateException("kar failed"))
        .when(karProcessor)
        .applyKarForNotification(any());

    assertTrue(receiver.processNotification(notification, request, response, null).isEmpty());
  }

  @Test
  public void testProcessRelaunchNotification_marksTheContextRelaunchedWithAFreshTriggerEvent()
      throws Exception {
    setting.setKars(kars(status(1, ACTIVE_KAR, true), status(2, INACTIVE_KAR, false)));
    when(karRepository.getById(ACTIVE_KAR)).thenReturn(kar);

    List<KarProcessingData> processed =
        receiver.processRelaunchNotification(notification, request, response, null, true);

    assertEquals(1, processed.size());
    NotificationContext nc = processed.get(0).getNotificationContext();
    assertEquals(
        NotificationProcessingStatusType.RELAUNCHED.toString(),
        nc.getNotificationProcessingStatus());
    assertTrue(nc.getTriggerEvent().contains("|relaunch-id:"));
    verify(ncDao).saveOrUpdate(nc);
    verify(karProcessor).applyKarForNotification(processed.get(0));
  }

  @Test
  public void testReProcessNotification_marksTheContextReprocessedAndKeepsTheLaunchContext()
      throws Exception {
    setting.setKars(kars(status(1, ACTIVE_KAR, true)));
    when(karRepository.getById(ACTIVE_KAR)).thenReturn(kar);
    PatientLaunchContext launchContext = new PatientLaunchContext();
    launchContext.setThrottleContext("throttle-ctx");
    launchContext.setEhrLaunchContext(Map.of("encounterId", "551683b3"));

    NotificationContext nc =
        receiver
            .reProcessNotification(notification, request, response, launchContext, false)
            .get(0)
            .getNotificationContext();

    assertEquals(
        NotificationProcessingStatusType.REPROCESSED.toString(),
        nc.getNotificationProcessingStatus());
    assertTrue(nc.getTriggerEvent().endsWith("|reprocessed"));
    assertEquals("throttle-ctx", nc.getThrottleContext());
    assertTrue(nc.getEhrLaunchContext().contains("\"encounterId\":\"551683b3\""));
  }

  /** forceReprocessing=false asks for a brand new trigger event instead of the shared one. */
  @Test
  public void testReProcessNotification_withForceReprocessingDisabledUsesAUniqueTriggerEvent()
      throws Exception {
    setting.setKars(kars(status(1, ACTIVE_KAR, true)));
    when(karRepository.getById(ACTIVE_KAR)).thenReturn(kar);
    PatientLaunchContext launchContext = new PatientLaunchContext();
    launchContext.setEhrLaunchContext(Map.of("forceReprocessing", "false"));

    NotificationContext nc =
        receiver
            .reProcessNotification(notification, request, response, launchContext, false)
            .get(0)
            .getNotificationContext();

    assertTrue(nc.getTriggerEvent().contains("|reprocessed:"));
  }

  //  @Test
  //  public void testInternalProcessing_failsWhenNoContextHealthcareSettingOrKarIsAvailable()
  //      throws Exception {
  //    Bundle invalid = new Bundle();
  //    assertThrows(
  //        InvalidNotification.class,
  //        () -> receiver.reProcessNotification(invalid, request, response, null, false));
  //
  //    setting.setKars(kars(status(1, ACTIVE_KAR, true)));
  //    when(karRepository.getById(ACTIVE_KAR)).thenReturn(null);
  //    assertThrows(
  //        InvalidNotification.class,
  //        () -> receiver.processRelaunchNotification(notification, request, response, null,
  // true));
  //
  //    setting.setKars(null);
  //    assertThrows(
  //        InvalidNotification.class,
  //        () -> receiver.processRelaunchNotification(notification, request, response, null,
  // true));
  //
  //    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(null);
  //    assertThrows(
  //        InvalidNotification.class,
  //        () -> receiver.reProcessNotification(notification, request, response, null, false));
  //
  //    verify(karProcessor, never()).applyKarForNotification(any());
  //  }

  @Test
  public void testInternalProcessing_failsWhenNoContextHealthcareSettingOrKarIsAvailable()
      throws Exception {
    PatientLaunchContext noLaunchContext = new PatientLaunchContext();

    Bundle invalid = new Bundle();
    assertThrows(
        InvalidNotification.class,
        () -> receiver.reProcessNotification(invalid, request, response, noLaunchContext, false));

    setting.setKars(kars(status(1, ACTIVE_KAR, true)));
    when(karRepository.getById(ACTIVE_KAR)).thenReturn(null);
    assertThrows(
        InvalidNotification.class,
        () -> receiver.processRelaunchNotification(notification, request, response, null, true));

    setting.setKars(null);
    assertThrows(
        InvalidNotification.class,
        () -> receiver.processRelaunchNotification(notification, request, response, null, true));

    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(null);
    assertThrows(
        InvalidNotification.class,
        () ->
            receiver.reProcessNotification(
                notification, request, response, noLaunchContext, false));

    verify(karProcessor, never()).applyKarForNotification(any());
  }

  //  @Test
  //  public void testInternalProcessing_rethrowsAKarProcessingFailure() throws Exception {
  //    setting.setKars(kars(status(1, ACTIVE_KAR, true)));
  //    when(karRepository.getById(ACTIVE_KAR)).thenReturn(kar);
  //    IllegalStateException failure = new IllegalStateException("kar failed");
  //    doThrow(failure).when(karProcessor).applyKarForNotification(any());
  //
  //    IllegalStateException thrown =
  //        assertThrows(
  //            IllegalStateException.class,
  //            () -> receiver.reProcessNotification(notification, request, response, null, false));
  //
  //    assertSame(failure, thrown);
  //  }
  @Test
  public void testInternalProcessing_rethrowsAKarProcessingFailure() throws Exception {
    setting.setKars(kars(status(1, ACTIVE_KAR, true)));
    when(karRepository.getById(ACTIVE_KAR)).thenReturn(kar);
    IllegalStateException failure = new IllegalStateException("kar failed");
    doThrow(failure).when(karProcessor).applyKarForNotification(any());

    IllegalStateException thrown =
        assertThrows(
            IllegalStateException.class,
            () ->
                receiver.reProcessNotification(
                    notification, request, response, new PatientLaunchContext(), false));

    assertSame(failure, thrown);
  }

  private KnowledgeArtifactStatus status(int id, String versionUniqueKarId, boolean active) {
    KnowledgeArtifactStatus status = new KnowledgeArtifactStatus();
    status.setId(id);
    status.setKarId(versionUniqueKarId.split("\\|")[0]);
    status.setKarVersion(versionUniqueKarId.split("\\|")[1]);
    status.setVersionUniqueKarId(versionUniqueKarId);
    status.setIsActive(active);
    return status;
  }

  private HealthcareSettingOperationalKnowledgeArtifacts kars(KnowledgeArtifactStatus... statuses) {
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.setId(setting.getId());
    Set<KnowledgeArtifactStatus> set = new LinkedHashSet<>(List.of(statuses));
    kars.setArtifactStatus(set);
    return kars;
  }
}
