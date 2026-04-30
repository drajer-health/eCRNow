package com.drajer.bsa.model;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.drajer.bsa.kar.model.KnowledgeArtifact;
import java.util.*;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.Test;

public class KarProcessingDataTest {

  @Test
  public void testaddResourcesByType() {
    KarProcessingData data = new KarProcessingData();
    Patient p1 = new Patient();
    p1.setId("Patient/1");
    Patient p2 = new Patient();
    p2.setId("Patient/2");
    Set<Resource> patients = new HashSet<>();
    patients.add(p1);
    patients.add(p2);
    Map<ResourceType, Set<Resource>> input = new HashMap<>();
    input.put(ResourceType.Patient, patients);
    data.addResourcesByType(input);
    Set<Resource> stored = data.getFhirInputDataByType().get(ResourceType.Patient);
    assertTrue(stored.contains(p1));
    Patient p3 = new Patient();
    p3.setId("Patient/3");
    Patient pDuplicate = new Patient();
    pDuplicate.setId("Patient/2");
    Set<Resource> newPatients = new HashSet<>(Arrays.asList(p3, pDuplicate));
    data.addResourcesByType(ResourceType.Patient, newPatients);
    Set<Resource> merged = data.getFhirInputDataByType().get(ResourceType.Patient);
    assertNotNull(merged);
  }

  @Test
  public void testisDataAlreadyFetched() {
    KarProcessingData data = new KarProcessingData();

    KnowledgeArtifact karMock = mock(KnowledgeArtifact.class);
    data.setKar(karMock);
    when(karMock.getFirstClassRelatedDataId("dataReqX")).thenReturn("relatedX");
    when(karMock.getFirstClassRelatedDataId("dataReq1")).thenReturn(null);
    when(karMock.getFirstClassRelatedDataId("id12")).thenReturn(null);

    data.getFhirInputDataById().put("dataReq1", new HashSet<Resource>());
    assertTrue(data.isDataAlreadyFetched("dataReq1", null));
    data.getFhirInputDataById().put("related1", new HashSet<Resource>());
    assertTrue(data.isDataAlreadyFetched("dataReq2", "related1"));
    data.getFhirInputDataById().put("relatedX", new HashSet<Resource>());
    assertTrue(data.isDataAlreadyFetched("dataReqX", null));
    assertFalse(data.isDataAlreadyFetched("dataReqUnknown", "relatedUnknown"));
    assertTrue(data.isDataAlreadyFetched("dataReq1", null));
  }

  @Test
  public void testContainsResourceWithId() {
    KarProcessingData data = new KarProcessingData();
    Patient p1 = new Patient();
    p1.setId("Patient/1");
    data.storeResourceById(p1.getIdElement().getIdPart(), p1);
    assertTrue(data.containsResourceWithId("1", ResourceType.Patient));
    assertFalse(data.containsResourceWithId("2", ResourceType.Patient));
  }

  @Test
  public void testHasValidAccessToken_valid() {
    HealthcareSetting healthcareSettingMock;
    KarProcessingData data = new KarProcessingData();
    healthcareSettingMock = mock(HealthcareSetting.class);
    data.setHealthcareSetting(healthcareSettingMock);
    data.setTokenRefreshThreshold(20);
    Date futureTokenTime = new Date(System.currentTimeMillis() + 30_000);
    when(healthcareSettingMock.getEhrAccessTokenExpirationTime()).thenReturn(futureTokenTime);
    assertTrue(data.hasValidAccessToken());
    Date soonTokenTime = new Date(System.currentTimeMillis() + 10_000);

    when(healthcareSettingMock.getEhrAccessTokenExpirationTime()).thenReturn(soonTokenTime);
    assertFalse(data.hasValidAccessToken());
  }

  @Test
  public void testAddResourceById_newId() {
    KarProcessingData data = new KarProcessingData();
    Patient p1 = new Patient();
    p1.setId("Patient/1");
    data.addResourceById("dataReq1", p1);
    Set<Resource> stored = data.getFhirInputDataById().get("dataReq1");
    assertNotNull(stored);
    assertEquals(1, stored.size());
    assertTrue(stored.contains(p1));
  }

  @Test
  public void testAddResourceById_existingId() {
    KarProcessingData data = new KarProcessingData();
    Patient p1 = new Patient();
    p1.setId("Patient/1");
    Patient p2 = new Patient();
    p2.setId("Patient/2");

    data.addResourceById("dataReq1", p1);

    data.addResourceById("dataReq1", p2);

    Set<Resource> stored = data.getFhirInputDataById().get("dataReq1");
    assertNotNull(stored);
    assertEquals(2, stored.size());
    assertTrue(stored.contains(p1));
    assertTrue(stored.contains(p2));
    data.addResourceById("dataReq1", p1);
    stored = data.getFhirInputDataById().get("dataReq1");
    assertEquals(2, stored.size());
  }

  @Test
  public void testAddResourcesById_existingEntry_mergesAndDeduplicates() {
    KarProcessingData data = new KarProcessingData();
    Patient p1 = new Patient();
    p1.setId("Patient/1");
    HashMap<String, Set<Resource>> initial = new HashMap<>();
    Set<Resource> initialSet = new HashSet<>();
    initialSet.add(p1);
    initial.put("dataReq1", initialSet);
    data.addResourcesById(initial);

    Patient p2 = new Patient();
    p2.setId("Patient/2");
    Patient p1Duplicate = new Patient();
    p1Duplicate.setId("Patient/1");

    Set<Resource> newSet = new HashSet<>();
    newSet.add(p2);
    newSet.add(p1Duplicate);

    HashMap<String, Set<Resource>> newInput = new HashMap<>();
    newInput.put("dataReq1", newSet);

    data.addResourcesById(newInput);

    Set<Resource> stored = data.getFhirInputDataById().get("dataReq1");
    assertEquals(2, stored.size());
    assertTrue(data.containsResourceWithId("1", ResourceType.Patient));
    assertTrue(data.containsResourceWithId("2", ResourceType.Patient));
  }

  @Test
  public void testResetResourcesById_existingEntries() {
    KarProcessingData data = new KarProcessingData();
    Patient p1 = new Patient();
    p1.setId("Patient/1");
    Set<Resource> existingSet = new HashSet<>();
    existingSet.add(p1);
    data.getFhirInputDataById().put("dataReq1", existingSet);
    Patient p2 = new Patient();
    p2.setId("Patient/2");
    Set<Resource> newSet = new HashSet<>();
    newSet.add(p2);

    HashMap<String, Set<Resource>> resetInput = new HashMap<>();
    resetInput.put("dataReq1", newSet);

    data.resetResourcesById(resetInput);

    Set<Resource> stored = data.getFhirInputDataById().get("dataReq1");
    assertEquals(1, stored.size());
    assertTrue(stored.contains(p2));
    assertFalse(stored.contains(p1));
  }
}
