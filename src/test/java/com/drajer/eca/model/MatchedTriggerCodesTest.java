package com.drajer.eca.model;

import static org.junit.Assert.*;

import java.util.HashSet;
import java.util.Set;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.Before;
import org.junit.Test;

public class MatchedTriggerCodesTest {

  private MatchedTriggerCodes mtc;

  @Before
  public void setUp() {
    mtc = new MatchedTriggerCodes();
    mtc.setMatchedPath("Observation.value");
    mtc.setValueSet("TestValueSet");
    mtc.setValueSetVersion("1.0");
  }

  @Test
  public void testAddAndGetCodes() {
    mtc.addCode("C001");
    mtc.addCode("C002");
    assertTrue(mtc.getMatchedCodes().contains("C001"));
    assertTrue(mtc.getMatchedCodes().contains("C002"));

    Set<String> codes = new HashSet<>();
    codes.add("C003");
    mtc.addCodes(codes);
    assertTrue(mtc.getMatchedCodes().contains("C003"));
  }

  @Test
  public void testAddAndGetValues() {
    mtc.addValue("V001");
    mtc.addValue("V002");
    assertTrue(mtc.getMatchedValues().contains("V001"));
    assertTrue(mtc.getMatchedValues().contains("V002"));

    Set<String> values = new HashSet<>();
    values.add("V003");
    mtc.addValues(values);
    assertTrue(mtc.getMatchedValues().contains("V003"));
  }

  @Test
  public void testHasMatchedTriggerCodes() {
    mtc.addCode("C001");
    assertTrue(mtc.hasMatchedTriggerCodes("Observation"));
    assertFalse(mtc.hasMatchedTriggerCodes("Patient"));
  }

  @Test
  public void testHasMatchedTriggerValue() {
    mtc.addValue("V001");
    assertTrue(mtc.hasMatchedTriggerValue("Observation"));
    assertFalse(mtc.hasMatchedTriggerValue("Patient"));
  }

  @Test
  public void testContainsMatch() {
    mtc.addCode("C001");
    assertTrue(mtc.containsMatch(ResourceType.Observation));
    assertFalse(mtc.containsMatch(ResourceType.Patient));
  }

  @Test
  public void testIsCodePresent() {
    mtc.addCode("C001");
    mtc.addValue("V001");
    assertTrue(mtc.isCodePresent("C001", "Observation.value"));
    assertTrue(mtc.isCodePresent("V001", "Observation.value"));
    assertFalse(mtc.isCodePresent("C002", "Observation.value"));
    assertFalse(mtc.isCodePresent("C001", "Patient.value"));
  }

  @Test
  public void testSettersAndGetters() {
    Set<String> codes = new HashSet<>();
    codes.add("C001");
    mtc.setMatchedCodes(codes);
    assertEquals(codes, mtc.getMatchedCodes());

    mtc.setValueSet("TestVS");
    assertEquals("TestVS", mtc.getValueSet());
    mtc.setValueSetVersion("1.0");
    assertEquals("1.0", mtc.getValueSetVersion());
    mtc.setMatchedPath("Observation.value");
    assertEquals("Observation.value", mtc.getMatchedPath());
    Set<String> values = new HashSet<>();
    values.add("V001");
    mtc.setMatchedValues(values);
    assertEquals(values, mtc.getMatchedValues());

    mtc.setValueSetOid("1.2.3");
    assertEquals("1.2.3", mtc.getValueSetOid());
  }

  @Test
  public void testLogMethod() {
    mtc.setMatchedPath("Observation.value");
    mtc.setValueSet("TestValueSet");
    mtc.setValueSetVersion("v1");

    mtc.addCode("123");
    mtc.addCode("456");
    mtc.addValue("val1");
    mtc.addValue("val2");
    mtc.log();

    assertTrue(mtc.getMatchedCodes().contains("123"));
    assertTrue(mtc.getMatchedCodes().contains("456"));
    assertTrue(mtc.getMatchedValues().contains("val1"));
    assertTrue(mtc.getMatchedValues().contains("val2"));
    assertEquals("Observation.value", mtc.getMatchedPath());
    assertEquals("TestValueSet", mtc.getValueSet());
    assertEquals("v1", mtc.getValueSetVersion());
  }
}
