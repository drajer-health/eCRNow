package com.drajer.bsa.service.impl;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.model.NotificationContext;
import java.util.*;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class NotificationContextServiceImplTest {

  @Mock private NotificationContextDao ncDao;
  @InjectMocks private NotificationContextServiceImpl service;

  private NotificationContext context;
  private UUID id;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    id = UUID.randomUUID();
    context = new NotificationContext();
    context.setId(id);
  }

  @Test
  public void testSaveOrUpdate() {
    when(ncDao.saveOrUpdate(context)).thenReturn(context);

    NotificationContext result = service.saveOrUpdate(context);

    assertNotNull(result);
    assertEquals(context.getId(), result.getId());
    verify(ncDao, times(1)).saveOrUpdate(context);
  }

  @Test
  public void testGetNotificationContext() {
    UUID mockId = UUID.randomUUID();
    NotificationContext context = new NotificationContext();
    context.setId(mockId);
    context.setFhirServerBaseUrl("http://hosp.fhir.server");
    context.setNotificationResourceId("resource123");
    context.setPatientId("patient456");
    context.setTriggerEvent("trigger_event_test");
    context.setNotificationProcessingStatus("IN_PROGRESS");

    when(ncDao.getNotificationContextById(mockId)).thenReturn(context);

    NotificationContext result = service.getNotificationContext(mockId);

    assertNotNull(result);
    assertEquals(mockId, result.getId());
    assertEquals("http://hosp.fhir.server", result.getFhirServerBaseUrl());
    assertEquals("resource123", result.getNotificationResourceId());
    assertEquals("patient456", result.getPatientId());
    assertEquals("trigger_event_test", result.getTriggerEvent());
    assertEquals("IN_PROGRESS", result.getNotificationProcessingStatus());

    verify(ncDao, times(1)).getNotificationContextById(mockId);
  }

  @Test
  public void testGetNotificationContextData() {
    NotificationContext context = new NotificationContext();
    context.setId(UUID.randomUUID());
    context.setFhirServerBaseUrl("http://hosp.fhir.server");
    context.setNotificationResourceId("resource123");
    context.setPatientId("patient456");
    context.setTriggerEvent("trigger_event_test");

    List<NotificationContext> mockList = Arrays.asList(context);

    when(ncDao.getNotificationContextData(
            id, "http://hosp.fhir.server", "resource123", "patient456"))
        .thenReturn(mockList);

    List<NotificationContext> result =
        service.getNotificationContextData(
            id, "http://hosp.fhir.server", "resource123", "patient456");

    assertNotNull(result);
    assertFalse(result.isEmpty());
    assertEquals(1, result.size());
    assertEquals("http://hosp.fhir.server", result.get(0).getFhirServerBaseUrl());
    assertEquals("resource123", result.get(0).getNotificationResourceId());
    assertEquals("patient456", result.get(0).getPatientId());
    assertEquals("trigger_event_test", result.get(0).getTriggerEvent());

    verify(ncDao, times(1))
        .getNotificationContextData(id, "http://hosp.fhir.server", "resource123", "patient456");
  }

  @Test
  public void testGetAllNotificationContextData() {
    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("patientId", "12345");
    searchParams.put("status", "active");

    NotificationContext nc = new NotificationContext();
    nc.setPatientId("12345");
    nc.setNotificationProcessingStatus("active");

    List<NotificationContext> ncList = new ArrayList<>();
    ncList.add(nc);

    when(ncDao.getAllNotificationContext(id, searchParams)).thenReturn(ncList);

    List<NotificationContext> result = service.getAllNotificationContextData(id, searchParams);

    assertNotNull(result);
    assertFalse(result.isEmpty());
    assertEquals(1, result.size());
    assertEquals("12345", result.get(0).getPatientId());
    assertEquals("active", result.get(0).getNotificationProcessingStatus());

    verify(ncDao, times(1)).getAllNotificationContext(id, searchParams);
  }

  @Test
  public void testGetNotificationContextForReprocessing() {
    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("patientId", "67890");
    searchParams.put("status", "in_progress");

    NotificationContext nc = new NotificationContext();
    nc.setPatientId("67890");
    nc.setNotificationProcessingStatus("in_progress");

    List<NotificationContext> ncList = new ArrayList<>();
    ncList.add(nc);

    when(ncDao.getNotificationContextForReprocessing(id, searchParams)).thenReturn(ncList);

    List<NotificationContext> result =
        service.getNotificationContextForReprocessing(id, searchParams);

    assertNotNull(result);
    assertFalse(result.isEmpty());
    assertEquals(1, result.size());
    assertEquals("67890", result.get(0).getPatientId());
    assertEquals("in_progress", result.get(0).getNotificationProcessingStatus());

    verify(ncDao, times(1)).getNotificationContextForReprocessing(id, searchParams);
  }

  @Test
  public void testDelete() {
    NotificationContext nc = new NotificationContext();
    doNothing().when(ncDao).delete(nc);
    service.delete(nc);
    verify(ncDao, times(1)).delete(nc);
  }
}
