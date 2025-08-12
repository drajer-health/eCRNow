package com.drajer.bsa.service.impl;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.exceptions.InvalidLaunchContext;
import com.drajer.bsa.exceptions.InvalidNotification;
import com.drajer.bsa.kar.model.*;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PatientLaunchContext;
import com.drajer.bsa.service.KarProcessor;
import com.drajer.test.util.TestUtils;
import java.util.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.hl7.fhir.r4.model.Bundle;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.beans.factory.annotation.Qualifier;

@RunWith(MockitoJUnitRunner.class)
public class SubscriptionNotificationReceiverImplTest {

  @InjectMocks private SubscriptionNotificationReceiverImpl subscriptionNotificationReceiver;

  @Mock private NotificationContextDao ncDao;
  @Mock private HealthcareSettingsDao hsDao;
  @Mock private KarProcessor karProcessor;
  @Mock private KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;

  @Mock
  @Qualifier("jsonParser")
  private IParser jsonParser;

  @Mock private HttpServletRequest request;
  @Mock private HttpServletResponse response;
  @Mock private PatientLaunchContext patientLaunchContext;
  @Mock private PatientLaunchContext launchContext;

  private Bundle notificationBundle;
  private NotificationContext notificationContext;
  private HealthcareSetting healthcareSetting;
  private KnowledgeArtifact knowledgeArtifact;
  private KnowledgeArtifactStatus karStatus;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    notificationBundle = new Bundle();
    notificationBundle =
        TestUtils.loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");
    Bundle.BundleEntryComponent encounterEntry = new Bundle.BundleEntryComponent();
    encounterEntry.setResource(notificationBundle);
    notificationBundle.addEntry(encounterEntry);
    notificationBundle.setType(Bundle.BundleType.HISTORY);

    notificationContext = mock(NotificationContext.class);
    healthcareSetting = mock(HealthcareSetting.class);
    knowledgeArtifact = mock(KnowledgeArtifact.class);
    karStatus = mock(KnowledgeArtifactStatus.class);

    when(jsonParser.encodeResourceToString(any())).thenReturn("mocked-json");

    when(ncDao.saveOrUpdate(any())).thenReturn(notificationContext);
    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(healthcareSetting);

    HealthcareSettingOperationalKnowledgeArtifacts artifacts =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    Set<KnowledgeArtifactStatus> karStatusSet = new HashSet<>();
    karStatusSet.add(karStatus);
    when(karStatus.getIsActive()).thenReturn(true);
    when(karStatus.getVersionUniqueKarId()).thenReturn("kar-1234");
    artifacts.setArtifactStatus(karStatusSet);
    when(healthcareSetting.getKars()).thenReturn(artifacts);

    when(knowledgeArtifactRepositorySystem.getById("kar-1234")).thenReturn(knowledgeArtifact);

    doNothing().when(karProcessor).applyKarForNotification(any());

    patientLaunchContext = createLaunchContext();
    when(launchContext.getThrottleContext()).thenReturn("test-throttle-context");
  }

  private PatientLaunchContext createLaunchContext() {
    PatientLaunchContext launchContext = new PatientLaunchContext();
    launchContext.setFhirServerURL("http://example.com");
    launchContext.setPatientId("123");
    launchContext.setEncounterId("456");
    return launchContext;
  }

  @Test
  public void testProcessNotification_Success() throws Exception {
    List<KarProcessingData> result =
        subscriptionNotificationReceiver.processNotification(
            notificationBundle, request, response, patientLaunchContext);

    assertNotNull(result);
    assertFalse(result.isEmpty());

    verify(ncDao, times(1)).saveOrUpdate(any());
    verify(hsDao, times(1)).getHealthcareSettingByUrl(anyString());
    verify(knowledgeArtifactRepositorySystem, times(1)).getById("kar-1234");
    verify(karProcessor, times(1)).applyKarForNotification(any());
  }

  @Test
  public void testProcessNotification_NoHealthcareSetting() throws Exception {
    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(null);
    List<KarProcessingData> result =
        subscriptionNotificationReceiver.processNotification(
            notificationBundle, request, response, null);

    assertNotNull(result);
    assertTrue(result.isEmpty());
    verify(hsDao, times(1)).getHealthcareSettingByUrl(anyString());
    verify(karProcessor, never()).applyKarForNotification(any());
  }

  @Test(expected = InvalidNotification.class)
  public void testProcessRelaunchNotification_NoHealthCareSetting()
      throws InvalidNotification, InvalidLaunchContext {
    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(null);
    subscriptionNotificationReceiver.processRelaunchNotification(
        notificationBundle, request, response, null, true);
  }

  @Test
  public void testProcessNotification_InactiveKar() throws Exception {
    when(karStatus.getIsActive()).thenReturn(false);
    List<KarProcessingData> result =
        subscriptionNotificationReceiver.processNotification(
            notificationBundle, request, response, patientLaunchContext);

    assertNotNull(result);
    assertTrue(result.isEmpty());
    verify(knowledgeArtifactRepositorySystem, never()).getById(anyString());
    verify(karProcessor, never()).applyKarForNotification(any());
  }

  @Test
  public void testProcessNotification_KarNotFound() throws Exception {
    when(knowledgeArtifactRepositorySystem.getById("kar-1234")).thenReturn(null);

    List<KarProcessingData> result =
        subscriptionNotificationReceiver.processNotification(
            notificationBundle, request, response, patientLaunchContext);

    assertNotNull(result);
    assertTrue(result.isEmpty());

    verify(knowledgeArtifactRepositorySystem, times(1)).getById("kar-1234");
    verify(karProcessor, never()).applyKarForNotification(any());
  }

  @Test
  public void testProcessNotification_NoKnowledgeArtifacts() throws Exception {
    when(healthcareSetting.getKars()).thenReturn(null);

    List<KarProcessingData> result =
        subscriptionNotificationReceiver.processNotification(
            notificationBundle, request, response, patientLaunchContext);

    assertNotNull(result);
    assertTrue(result.isEmpty());

    verify(knowledgeArtifactRepositorySystem, never()).getById(anyString());
    verify(karProcessor, never()).applyKarForNotification(any());
  }

  @Test
  public void testProcessNotification_ExceptionDuringProcessing() throws Exception {
    when(hsDao.getHealthcareSettingByUrl(anyString()))
        .thenThrow(new RuntimeException("Database error"));

    List<KarProcessingData> result =
        subscriptionNotificationReceiver.processNotification(
            notificationBundle, request, response, patientLaunchContext);

    assertNotNull(result);
    assertTrue(result.isEmpty());

    verify(ncDao, times(1)).saveOrUpdate(any());
    verify(hsDao, times(1)).getHealthcareSettingByUrl(anyString());

    verifyNoMoreInteractions(karProcessor);
  }

  @Test
  public void testProcessRelaunchNotification_Success() throws Exception {
    List<KarProcessingData> result =
        subscriptionNotificationReceiver.processRelaunchNotification(
            notificationBundle, request, response, launchContext, true);

    assertNotNull(result);
    assertFalse(result.isEmpty());
    verify(ncDao, times(1)).saveOrUpdate(any());
    verify(hsDao, times(1)).getHealthcareSettingByUrl(anyString());
    verify(karProcessor, times(1)).applyKarForNotification(any());
  }

  @Test(expected = InvalidNotification.class)
  public void testProcessRelaunchNotification_NoHealthcareSetting() throws Exception {
    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(null);

    subscriptionNotificationReceiver.processRelaunchNotification(
        notificationBundle, request, response, launchContext, true);
  }

  @Test
  public void testProcessRelaunchNotification_InactiveKar() throws Exception {
    when(karStatus.getIsActive()).thenReturn(false);
    List<KarProcessingData> result =
        subscriptionNotificationReceiver.processRelaunchNotification(
            notificationBundle, request, response, launchContext, true);

    assertNotNull(result);
    assertTrue(result.isEmpty());
    verify(karProcessor, never()).applyKarForNotification(any());
  }

  @Test(expected = InvalidNotification.class)
  public void testProcessRelaunchNotification_KarNotFound() throws Exception {
    when(knowledgeArtifactRepositorySystem.getById("kar-1234")).thenReturn(null);

    subscriptionNotificationReceiver.processRelaunchNotification(
        notificationBundle, request, response, launchContext, true);
  }

  @Test
  public void testProcessRelaunchNotification_NoKnowledgeArtifacts() throws Exception {
    when(healthcareSetting.getKars()).thenReturn(null);

    try {
      subscriptionNotificationReceiver.processRelaunchNotification(
          notificationBundle, request, response, launchContext, true);
      fail("Expected InvalidNotification exception was not thrown");
    } catch (InvalidNotification e) {

    }
  }

  @Test
  public void testProcessRelaunchNotification_ExceptionDuringProcessing() throws Exception {
    when(hsDao.getHealthcareSettingByUrl(anyString()))
        .thenThrow(new RuntimeException("Database error"));

    try {
      subscriptionNotificationReceiver.processRelaunchNotification(
          notificationBundle, request, response, launchContext, true);
      fail("Expected RuntimeException was not thrown");
    } catch (RuntimeException e) {
      assertEquals("Database error", e.getMessage());
    }
  }

  @Test
  public void testReProcessNotification_Success() throws Exception {

    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(healthcareSetting);

    KnowledgeArtifactStatus mockStatus = mock(KnowledgeArtifactStatus.class);
    when(mockStatus.getVersionUniqueKarId()).thenReturn("kar-1234");
    when(mockStatus.getIsActive()).thenReturn(true);

    Set<KnowledgeArtifactStatus> artifactStatuses = new HashSet<>();
    artifactStatuses.add(mockStatus);

    HealthcareSettingOperationalKnowledgeArtifacts mockKars =
        mock(HealthcareSettingOperationalKnowledgeArtifacts.class);
    when(mockKars.getArtifactStatus()).thenReturn(artifactStatuses);
    when(healthcareSetting.getKars()).thenReturn(mockKars);

    KnowledgeArtifact mockKar = mock(KnowledgeArtifact.class);
    when(knowledgeArtifactRepositorySystem.getById("kar-1234")).thenReturn(mockKar);

    List<KarProcessingData> result =
        subscriptionNotificationReceiver.reProcessNotification(
            notificationBundle, request, response, launchContext, false);

    assertNotNull(result);
    assertFalse(result.isEmpty());

    verify(ncDao, times(1)).saveOrUpdate(any());
    verify(hsDao, times(1)).getHealthcareSettingByUrl(anyString());
    verify(knowledgeArtifactRepositorySystem, times(1)).getById("kar-1234");
    verify(karProcessor, times(1)).applyKarForNotification(any());
  }

  @Test(expected = InvalidNotification.class)
  public void testReProcessNotification_NoHealthcareSetting() throws Exception {
    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(null);

    subscriptionNotificationReceiver.reProcessNotification(
        notificationBundle, request, response, launchContext, false);
  }

  @Test(expected = InvalidNotification.class)
  public void testReProcessNotification_KarNotFound() throws Exception {
    when(knowledgeArtifactRepositorySystem.getById("kar-1234")).thenReturn(null);

    subscriptionNotificationReceiver.reProcessNotification(
        notificationBundle, request, response, launchContext, false);
  }

  @Test
  public void testReProcessNotification_InactiveKar() throws Exception {
    when(hsDao.getHealthcareSettingByUrl(anyString())).thenReturn(healthcareSetting);
    when(healthcareSetting.getKars())
        .thenReturn(mock(HealthcareSettingOperationalKnowledgeArtifacts.class));

    List<KarProcessingData> result =
        subscriptionNotificationReceiver.reProcessNotification(
            notificationBundle, request, response, launchContext, false);

    assertNotNull(result);
    assertTrue(result.isEmpty());
    verify(knowledgeArtifactRepositorySystem, never()).getById(anyString());
    verify(karProcessor, never()).applyKarForNotification(any());
  }

  @Test(expected = RuntimeException.class)
  public void testReProcessNotification_ExceptionDuringProcessing() throws Exception {
    when(hsDao.getHealthcareSettingByUrl(anyString()))
        .thenThrow(new InvalidNotification("Error during re-processing of notification"));

    try {
      subscriptionNotificationReceiver.reProcessNotification(
          notificationBundle, request, response, launchContext, false);
    } catch (InvalidNotification e) {
      assertEquals("Error during re-processing of notification", e.getMessage());
    }
  }
}
