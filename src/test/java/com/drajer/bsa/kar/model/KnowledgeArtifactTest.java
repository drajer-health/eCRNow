package com.drajer.bsa.kar.model;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.*;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.UriType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class KnowledgeArtifactTest {

  private KnowledgeArtifact kar;
  private BsaAction action1;
  private BsaAction action2;
  private FhirQueryFilter filter1;
  private UriType receiver;

  @BeforeEach
  public void setUp() {
    kar = new KnowledgeArtifact();

    action1 = mock(BsaAction.class);
    action2 = mock(BsaAction.class);

    when(action1.getActionId()).thenReturn("ACT-001");
    when(action2.getActionId()).thenReturn("ACT-002");

    Map<String, BsaAction> actions = new HashMap<>();
    actions.put(action1.getActionId(), action1);
    actions.put(action2.getActionId(), action2);
    kar.setActionMap(new HashMap<>(actions));

    Set<BsaAction> actionsForEvent = new HashSet<>();
    actionsForEvent.add(action1);
    Map<String, Set<BsaAction>> triggerMap = new HashMap<>();
    triggerMap.put("EVENT-1", actionsForEvent);
    kar.setTriggerEventActionMap(new HashMap<>(triggerMap));

    filter1 = mock(FhirQueryFilter.class);
    when(filter1.getQueryString()).thenReturn("FAKE QUERY");

    Map<String, FhirQueryFilter> queries = new HashMap<>();
    queries.put("DATA-1", filter1);
    kar.setDefaultQueries(new HashMap<>(queries));

    Map<String, String> dataIdMap = new HashMap<>();
    dataIdMap.put("ID-1", "ID-2");
    dataIdMap.put("ID-2", "FINAL-ID");
    kar.setDataIdRelatedDataIdMap(new HashMap<>(dataIdMap));

    receiver = new UriType("http://test.receiver.com");
  }

  @Test
  public void testGetActionExists() {
    BsaAction result = kar.getAction("ACT-001");
    assertNotNull(result);
    assertEquals("ACT-001", result.getActionId());
  }

  @Test
  public void testGetActionDoesNotExist() {
    assertNull(kar.getAction("UNKNOWN"));
  }

  @Test
  public void testGetActionWithNullMap() {
    kar.setActionMap(null);
    assertNull(kar.getAction("ACT-001"));
  }

  @Test
  public void testAddReceiverAddress() {
    kar.addReceiverAddress(receiver);
    assertTrue(kar.getReceiverAddresses().contains(receiver));
  }

  @Test
  public void testGetOriginalKarBundle() {
    Bundle bundle = new Bundle();
    kar.setOriginalKarBundle(bundle);
    assertEquals(bundle, kar.getOriginalKarBundle());
  }

  @Test
  public void testGetActionMap() {
    Map<String, BsaAction> map = kar.getActionMap();
    assertEquals(2, map.size());
    assertTrue(map.containsKey("ACT-001"));
    assertTrue(map.containsKey("ACT-002"));
  }

  @Test
  public void testGetDependencies() {
    assertNotNull(kar.getDependencies());
    assertTrue(kar.getDependencies().isEmpty());
  }

  @Test
  public void testGetActionsForTriggerEventFound() {
    Set<BsaAction> result = kar.getActionsForTriggerEvent("EVENT-1");
    assertNotNull(result);
    assertTrue(result.contains(action1));
  }

  @Test
  public void testGetActionsForTriggerEventWithPipe() {
    Set<BsaAction> result = kar.getActionsForTriggerEvent("EVENT-1|EXTRA");
    assertNotNull(result);
    assertTrue(result.contains(action1));
  }

  @Test
  public void testGetActionsForTriggerEventNotFound() {
    Set<BsaAction> result = kar.getActionsForTriggerEvent("UNKNOWN");
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testSetFirstLevelActions() {
    List<BsaAction> firstLevel = new ArrayList<>();
    firstLevel.add(action2);
    kar.setFirstLevelActions(firstLevel);
    assertEquals(firstLevel, kar.getFirstLevelActions());
  }

  @Test
  public void testGetKarPath() {
    kar.setKarPath("test/path");
    assertEquals("test/path", kar.getKarPath());
  }

  @Test
  public void testGetQueryForDataRequirementWithDataReqId() {
    String query = kar.getQueryForDataRequirement("DATA-1", null);
    assertEquals("FAKE QUERY", query);
  }

  @Test
  public void testGetQueryForDataRequirementWithRelatedDataId() {
    String query = kar.getQueryForDataRequirement("UNKNOWN", "DATA-1");
    assertEquals("FAKE QUERY", query);
  }

  @Test
  public void testGetQueryForDataRequirementNotFound() {
    String query = kar.getQueryForDataRequirement("UNKNOWN", "UNKNOWN");
    assertEquals("", query);
  }

  @Test
  public void testGetFirstClassRelatedDataId() {
    String finalId = kar.getFirstClassRelatedDataId("ID-1");
    assertEquals("FINAL-ID", finalId);
  }

  @Test
  public void testGetFirstClassRelatedDataIdDirect() {
    String finalId = kar.getFirstClassRelatedDataId("ID-2");
    assertEquals("FINAL-ID", finalId);
  }

  @Test
  public void testGetFirstClassRelatedDataIdNotFound() {
    String finalId = kar.getFirstClassRelatedDataId("UNKNOWN");
    assertNull(finalId);
  }
}
