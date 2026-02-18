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
}
