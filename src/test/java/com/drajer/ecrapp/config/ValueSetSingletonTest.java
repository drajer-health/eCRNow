package com.drajer.ecrapp.config;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;

import com.drajer.ecrapp.util.ApplicationUtils;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.ValueSet;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({ApplicationUtils.class, ValueSetSingleton.class})
public class ValueSetSingletonTest {

  private ValueSetSingleton singleton;

  @Before
  public void setUp() {
    singleton = ValueSetSingleton.getInstance();

    singleton.setGrouperToValueSetMap(new HashMap<>());
    singleton.setGrouperToEmergentValueSetMap(new HashMap<>());
    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.setEmergentValueSetsAsString(new HashSet<>());
    singleton.setGrouperValueSets(new HashSet<>());
  }

  @Test
  public void testgetValueSetsAsStringForGrouper() {
    PowerMockito.mockStatic(ApplicationUtils.class);
    Set<String> mockSet = new HashSet<String>();
    mockSet.add("mocked-value-set-url");
    when(ApplicationUtils.convertValueSetsToString(anySet())).thenReturn(mockSet);

    ValueSet valueSet = new ValueSet();
    valueSet.setId("val1");
    valueSet.setUrl("http://example.com/ValueSet/val1");
    Set<ValueSet> grouperValueSet = new HashSet<>();
    grouperValueSet.add(valueSet);

    Map<String, Set<ValueSet>> triggerPath = new HashMap<>();
    triggerPath.put("test", grouperValueSet);
    singleton.setTriggerPathToGrouperMap(triggerPath);
    Map<String, Set<ValueSet>> grouperToValueSetMap = new HashMap<>();
    grouperToValueSetMap.put("val1", grouperValueSet);
    singleton.setGrouperToValueSetMap(grouperToValueSetMap);
    Set<String> result = singleton.getValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertTrue(result.contains("mocked-value-set-url"));
    PowerMockito.verifyStatic(ApplicationUtils.class);
    ApplicationUtils.convertValueSetsToString(anySet());
  }

  @Test
  public void testGetValueSetsAsStringForGrouper_ElseBranch() {
    PowerMockito.mockStatic(ApplicationUtils.class);
    ValueSet valueSet = new ValueSet();
    valueSet.setId("val1");
    valueSet.setUrl("http://example.com/ValueSet/val1");

    Set<ValueSet> grouperValueSet = new HashSet<ValueSet>();
    grouperValueSet.add(valueSet);

    Map<String, Set<ValueSet>> triggerPath = new HashMap<String, Set<ValueSet>>();
    triggerPath.put("test", grouperValueSet);
    singleton.setTriggerPathToGrouperMap(triggerPath);

    Map<String, Set<ValueSet>> grouperToValueSetMap = new HashMap<String, Set<ValueSet>>();

    grouperToValueSetMap.put("someOtherId", new HashSet<ValueSet>());
    singleton.setGrouperToValueSetMap(grouperToValueSetMap);

    Set<String> result = singleton.getValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetEmergentValueSetsAsStringForGrouper_HappyPath() {
    PowerMockito.mockStatic(ApplicationUtils.class);

    Set<String> mockSet = new HashSet<String>();
    mockSet.add("emergent-url");
    when(ApplicationUtils.convertValueSetsToString(anySet())).thenReturn(mockSet);

    ValueSet grouper = new ValueSet();
    grouper.setId("grouper1");
    Set<ValueSet> grouperValueSet = new HashSet<ValueSet>();
    grouperValueSet.add(grouper);
    singleton.getTriggerPathToGrouperMap().put("testPath", grouperValueSet);
    ValueSet emergent = new ValueSet();
    emergent.setId("emergent1");
    Set<ValueSet> emergentSet = new HashSet<ValueSet>();
    emergentSet.add(emergent);
    singleton.getGrouperToEmergentValueSetMap().put("grouper1", emergentSet);
    Set<String> result = singleton.getEmergentValueSetsAsStringForGrouper("testPath");

    assertNotNull(result);
    assertFalse(result.isEmpty());
    assertTrue(result.contains("emergent-url"));
    PowerMockito.verifyStatic(ApplicationUtils.class);
    ApplicationUtils.convertValueSetsToString(anySet());
  }

  @Test
  public void testGetEmergentValueSetsAsStringForGrouper_PathNotFound() {
    PowerMockito.mockStatic(ApplicationUtils.class);

    Set<String> result = singleton.getEmergentValueSetsAsStringForGrouper("invalidPath");

    assertNotNull(result);
    assertTrue(result.isEmpty());

    PowerMockito.verifyStatic(ApplicationUtils.class, never());
    ApplicationUtils.convertValueSetsToString(anySet());
  }

  @Test
  public void testAddGrouperToEmergentValueSetMap_MapNull() {
    singleton.setGrouperToEmergentValueSetMap(null);
    ValueSet vs = new ValueSet();
    vs.setId("val1");
    Set<ValueSet> set = new HashSet<ValueSet>();
    set.add(vs);

    singleton.addGrouperToEmergentValueSetMap("grouper1", set);

    assertNotNull(singleton.getGrouperToEmergentValueSetMap());
    assertTrue(singleton.getGrouperToEmergentValueSetMap().containsKey("grouper1"));
    assertEquals(1, singleton.getGrouperToEmergentValueSetMap().get("grouper1").size());
  }

  @Test
  public void testAddGrouperToEmergentValueSetMap_ContainsGrouper() {
    ValueSet vs = new ValueSet();
    vs.setId("val1");
    Set<ValueSet> set = new HashSet<ValueSet>();
    set.add(vs);
    Map<String, Set<ValueSet>> map = new HashMap<String, Set<ValueSet>>();
    map.put("grouper1", new HashSet<ValueSet>());
    singleton.setGrouperToEmergentValueSetMap(map);
    singleton.addGrouperToEmergentValueSetMap("grouper1", set);
    assertEquals(1, singleton.getGrouperToEmergentValueSetMap().size());
    assertTrue(singleton.getGrouperToEmergentValueSetMap().get("grouper1").isEmpty());
  }

  @Test
  public void testAddGrouperToValueSetMap_MapNull() {
    singleton.setGrouperToValueSetMap(null);
    ValueSet vs = new ValueSet();
    vs.setId("val1");
    Set<ValueSet> set = new HashSet<ValueSet>();
    set.add(vs);

    singleton.addGrouperToValueSetMap("grouper1", set);
    assertNotNull(singleton.getGrouperToValueSetMap());
    assertTrue(singleton.getGrouperToValueSetMap().containsKey("grouper1"));
    assertEquals(1, singleton.getGrouperToValueSetMap().get("grouper1").size());
  }

  @Test
  public void testAddGrouperToValueSetMap_ContainsGrouper() {
    ValueSet vs = new ValueSet();
    vs.setId("val1");
    Set<ValueSet> set = new HashSet<ValueSet>();
    set.add(vs);

    Map<String, Set<ValueSet>> map = new HashMap<String, Set<ValueSet>>();
    map.put("grouper1", new HashSet<ValueSet>());
    singleton.setGrouperToValueSetMap(map);
    singleton.addGrouperToValueSetMap("grouper1", set);

    assertEquals(1, singleton.getGrouperToValueSetMap().size());
    assertTrue(singleton.getGrouperToValueSetMap().get("grouper1").isEmpty());
  }

  @Test
  public void testAddGrouperToValueSetMap_AddNewGrouper() {
    ValueSet vs = new ValueSet();
    vs.setId("val1");
    Set<ValueSet> set = new HashSet<ValueSet>();
    set.add(vs);

    Map<String, Set<ValueSet>> map = new HashMap<String, Set<ValueSet>>();
    map.put("someOtherGrouper", new HashSet<ValueSet>());
    singleton.setGrouperToValueSetMap(map);

    singleton.addGrouperToValueSetMap("grouper1", set);

    assertEquals(2, singleton.getGrouperToValueSetMap().size());
    assertTrue(singleton.getGrouperToValueSetMap().containsKey("grouper1"));
    assertEquals(1, singleton.getGrouperToValueSetMap().get("grouper1").size());
  }

  @Test
  public void testPrint() {
    ValueSet vs1 = new ValueSet();
    vs1.setId("val1");
    vs1.setUrl("http://example.com/ValueSet/val1");

    Set<ValueSet> set1 = new HashSet<>();
    set1.add(vs1);
    singleton.getGrouperToValueSetMap().put("grouper1", set1);

    ValueSet vs2 = new ValueSet();
    vs2.setId("emergent1");
    vs2.setUrl("http://example.com/ValueSet/emergent1");

    Set<ValueSet> set2 = new HashSet<>();
    set2.add(vs2);
    singleton.getGrouperToEmergentValueSetMap().put("grouperEmergent", set2);
    singleton.print();
  }

  // ============ NEW TEST CASES FOR UNCOVERED ELSE CONDITIONS ============

  @Test
  public void testPrint_WithNullValueSetInMap() {
    Map<String, Set<ValueSet>> map = new HashMap<>();
    map.put("grouper1", null); // Null value set
    singleton.setGrouperToValueSetMap(map);
    singleton.print();
  }

  @Test
  public void testPrint_WithEmptyGrouperToEmergentValueSetMap() {
    singleton.setGrouperToEmergentValueSetMap(new HashMap<>());
    singleton.setGrouperToValueSetMap(new HashMap<>());
    singleton.print();
  }

  @Test
  public void testGetValueSetsAsStringForGrouper_WithNullGrouperValueSet() {
    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.setGrouperToValueSetMap(new HashMap<>());

    Set<String> result = singleton.getValueSetsAsStringForGrouper("nonexistent");

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetValueSetsAsStringForGrouper_WithNullGrouperToValueSetMap() {
    ValueSet grouper = new ValueSet();
    grouper.setId("grouper1");
    Set<ValueSet> grouperSet = new HashSet<>();
    grouperSet.add(grouper);

    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.getTriggerPathToGrouperMap().put("test", grouperSet);
    singleton.setGrouperToValueSetMap(null);

    Set<String> result = singleton.getValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetValueSetsAsStringForGrouper_WithEmptyValueSet() {
    ValueSet grouper = new ValueSet();
    grouper.setId("grouper1");
    Set<ValueSet> grouperSet = new HashSet<>();
    grouperSet.add(grouper);

    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.getTriggerPathToGrouperMap().put("test", grouperSet);

    Map<String, Set<ValueSet>> grouperToValueSetMap = new HashMap<>();
    grouperToValueSetMap.put("grouper1", new HashSet<>()); // Empty set
    singleton.setGrouperToValueSetMap(grouperToValueSetMap);

    Set<String> result = singleton.getValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetValueSetsAsStringForGrouper_WithNullGrouperId() {
    ValueSet grouper = new ValueSet();
    grouper.setId((String) null); // Null ID
    Set<ValueSet> grouperSet = new HashSet<>();
    grouperSet.add(grouper);

    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.getTriggerPathToGrouperMap().put("test", grouperSet);
    singleton.setGrouperToValueSetMap(new HashMap<>());

    Set<String> result = singleton.getValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetEmergentValueSetsAsStringForGrouper_WithNullGrouperToEmergentMap() {
    ValueSet grouper = new ValueSet();
    grouper.setId("grouper1");
    Set<ValueSet> grouperSet = new HashSet<>();
    grouperSet.add(grouper);

    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.getTriggerPathToGrouperMap().put("test", grouperSet);
    singleton.setGrouperToEmergentValueSetMap(null);

    Set<String> result = singleton.getEmergentValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetEmergentValueSetsAsStringForGrouper_WithEmptyEmergentSet() {
    ValueSet grouper = new ValueSet();
    grouper.setId("grouper1");
    Set<ValueSet> grouperSet = new HashSet<>();
    grouperSet.add(grouper);

    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.getTriggerPathToGrouperMap().put("test", grouperSet);

    Map<String, Set<ValueSet>> emergentMap = new HashMap<>();
    emergentMap.put("grouper1", new HashSet<>()); // Empty set
    singleton.setGrouperToEmergentValueSetMap(emergentMap);

    Set<String> result = singleton.getEmergentValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetEmergentValueSetsAsStringForGrouper_WithNullEmergentValueSet() {
    ValueSet grouper = new ValueSet();
    grouper.setId("grouper1");
    Set<ValueSet> grouperSet = new HashSet<>();
    grouperSet.add(grouper);

    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.getTriggerPathToGrouperMap().put("test", grouperSet);

    Map<String, Set<ValueSet>> emergentMap = new HashMap<>();
    emergentMap.put("grouper1", null); // Null value set
    singleton.setGrouperToEmergentValueSetMap(emergentMap);

    Set<String> result = singleton.getEmergentValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetEmergentValueSetsAsStringForGrouper_WithNullGrouperId() {
    ValueSet grouper = new ValueSet();
    grouper.setId((String) null); // Null ID
    Set<ValueSet> grouperSet = new HashSet<>();
    grouperSet.add(grouper);

    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.getTriggerPathToGrouperMap().put("test", grouperSet);
    singleton.setGrouperToEmergentValueSetMap(new HashMap<>());

    Set<String> result = singleton.getEmergentValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetEmergentValueSetsAsStringForGrouper_WithMultipleGroupers() {
    PowerMockito.mockStatic(ApplicationUtils.class);
    Set<String> mockSet1 = new HashSet<>();
    mockSet1.add("emergent-url-1");
    Set<String> mockSet2 = new HashSet<>();
    mockSet2.add("emergent-url-2");

    when(ApplicationUtils.convertValueSetsToString(anySet()))
        .thenReturn(mockSet1)
        .thenReturn(mockSet2);

    ValueSet grouper1 = new ValueSet();
    grouper1.setId("grouper1");
    ValueSet grouper2 = new ValueSet();
    grouper2.setId("grouper2");

    Set<ValueSet> grouperSet = new HashSet<>();
    grouperSet.add(grouper1);
    grouperSet.add(grouper2);

    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.getTriggerPathToGrouperMap().put("test", grouperSet);

    Map<String, Set<ValueSet>> emergentMap = new HashMap<>();
    Set<ValueSet> emergentSet1 = new HashSet<>();
    ValueSet emergent1 = new ValueSet();
    emergent1.setId("emergent1");
    emergentSet1.add(emergent1);
    emergentMap.put("grouper1", emergentSet1);

    Set<ValueSet> emergentSet2 = new HashSet<>();
    ValueSet emergent2 = new ValueSet();
    emergent2.setId("emergent2");
    emergentSet2.add(emergent2);
    emergentMap.put("grouper2", emergentSet2);

    singleton.setGrouperToEmergentValueSetMap(emergentMap);

    Set<String> result = singleton.getEmergentValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertFalse(result.isEmpty());
  }

  @Test
  public void testAddGrouperToValueSetMap_AddToExistingMap() {
    ValueSet vs1 = new ValueSet();
    vs1.setId("val1");
    Set<ValueSet> set1 = new HashSet<>();
    set1.add(vs1);

    ValueSet vs2 = new ValueSet();
    vs2.setId("val2");
    Set<ValueSet> set2 = new HashSet<>();
    set2.add(vs2);

    Map<String, Set<ValueSet>> map = new HashMap<>();
    map.put("existingGrouper", set1);
    singleton.setGrouperToValueSetMap(map);

    singleton.addGrouperToValueSetMap("newGrouper", set2);

    assertEquals(2, singleton.getGrouperToValueSetMap().size());
    assertTrue(singleton.getGrouperToValueSetMap().containsKey("newGrouper"));
    assertTrue(singleton.getGrouperToValueSetMap().containsKey("existingGrouper"));
  }

  @Test
  public void testAddGrouperToEmergentValueSetMap_AddToExistingMap() {
    ValueSet vs1 = new ValueSet();
    vs1.setId("val1");
    Set<ValueSet> set1 = new HashSet<>();
    set1.add(vs1);

    ValueSet vs2 = new ValueSet();
    vs2.setId("val2");
    Set<ValueSet> set2 = new HashSet<>();
    set2.add(vs2);

    Map<String, Set<ValueSet>> map = new HashMap<>();
    map.put("existingGrouper", set1);
    singleton.setGrouperToEmergentValueSetMap(map);

    singleton.addGrouperToEmergentValueSetMap("newGrouper", set2);

    assertEquals(2, singleton.getGrouperToEmergentValueSetMap().size());
    assertTrue(singleton.getGrouperToEmergentValueSetMap().containsKey("newGrouper"));
    assertTrue(singleton.getGrouperToEmergentValueSetMap().containsKey("existingGrouper"));
  }

  @Test
  public void testGetEmergentValueSets_InitializesIfNull() {
    singleton.setEmergentValueSets(null);

    Set<ValueSet> result = singleton.getEmergentValueSets();

    assertNotNull(result);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetValueSets_InitializesIfNull() {
    singleton.setValueSets(null);

    Set<ValueSet> result = singleton.getValueSets();

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetTriggerPathToValueSetsMap_InitializesIfNull() {
    singleton.setTriggerPathToValueSetsMap(null);

    Map<String, Set<ValueSet>> result = singleton.getTriggerPathToValueSetsMap();

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetTriggerPathToGrouperMap_InitializesIfNull() {
    singleton.setTriggerPathToGrouperMap(null);

    Map<String, Set<ValueSet>> result = singleton.getTriggerPathToGrouperMap();

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetValueSetsAsStringForGrouper_WithMultipleValueSets() {
    PowerMockito.mockStatic(ApplicationUtils.class);
    Set<String> mockSet = new HashSet<>();
    mockSet.add("value-set-1");
    mockSet.add("value-set-2");

    when(ApplicationUtils.convertValueSetsToString(anySet())).thenReturn(mockSet);

    ValueSet grouper = new ValueSet();
    grouper.setId("grouper1");
    Set<ValueSet> grouperSet = new HashSet<>();
    grouperSet.add(grouper);

    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.getTriggerPathToGrouperMap().put("test", grouperSet);

    ValueSet vs1 = new ValueSet();
    vs1.setId("vs1");
    ValueSet vs2 = new ValueSet();
    vs2.setId("vs2");

    Set<ValueSet> valueSetSet = new HashSet<>();
    valueSetSet.add(vs1);
    valueSetSet.add(vs2);

    Map<String, Set<ValueSet>> grouperToValueSetMap = new HashMap<>();
    grouperToValueSetMap.put("grouper1", valueSetSet);
    singleton.setGrouperToValueSetMap(grouperToValueSetMap);

    Set<String> result = singleton.getValueSetsAsStringForGrouper("test");

    assertNotNull(result);
    assertEquals(2, result.size());
    assertTrue(result.contains("value-set-1"));
    assertTrue(result.contains("value-set-2"));
  }

  @Test
  public void testSetEmergentValueSets_UpdatesEmergentValueSetsAsString() {
    PowerMockito.mockStatic(ApplicationUtils.class);

    Set<String> mockSet = new HashSet<>();
    mockSet.add("mock-string-1");
    when(ApplicationUtils.convertValueSetsToString(anySet())).thenReturn(mockSet);

    ValueSet vs = new ValueSet();
    vs.setId("val1");
    Set<ValueSet> valueSetSet = new HashSet<>();
    valueSetSet.add(vs);

    singleton.setEmergentValueSets(valueSetSet);

    Set<String> emergentStrings = singleton.getEmergentValueSetsAsString();
    assertNotNull(emergentStrings);
  }

  @Test
  public void testGetEmergentValueSetsAsStringForGrouper_NullPathParameter() {
    singleton.setTriggerPathToGrouperMap(new HashMap<>());
    singleton.setGrouperToEmergentValueSetMap(new HashMap<>());

    Set<String> result = singleton.getEmergentValueSetsAsStringForGrouper(null);

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }
}
