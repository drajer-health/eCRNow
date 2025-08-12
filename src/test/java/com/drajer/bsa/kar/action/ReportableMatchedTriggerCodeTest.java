package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;

import java.util.HashSet;
import java.util.Set;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
public class ReportableMatchedTriggerCodeTest {

  @InjectMocks ReportableMatchedTriggerCode triggerCode;

  @Test
  public void testGetAndSetValueSetVersion() {
    triggerCode.setValueSetVersion("1.0.0");
    assertEquals("1.0.0", triggerCode.getValueSetVersion());
  }

  @Test
  public void testGetAndSetValueSet() {
    triggerCode.setValueSet("TestValueSet");
    assertEquals("TestValueSet", triggerCode.getValueSet());
  }

  @Test
  public void testGetAndSetValueSetOid() {
    triggerCode.setValueSetOid("2.16.840.1.113883.3.464.1003.101.12.1001");
    assertEquals("2.16.840.1.113883.3.464.1003.101.12.1001", triggerCode.getValueSetOid());
  }

  @Test
  public void testGetAndSetCodeSystem() {
    triggerCode.setCodeSystem("ICD-10");
    assertEquals("ICD-10", triggerCode.getCodeSystem());
  }

  @Test
  public void testGetAndSetCode() {
    triggerCode.setCode("A00");
    assertEquals("A00", triggerCode.getCode());
  }

  @Test
  public void testGetAndSetAllMatches() {
    Set<String> matches = new HashSet<>();
    matches.add("match1");
    matches.add("match2");

    triggerCode.setAllMatches(matches);

    assertEquals(2, triggerCode.getAllMatches().size());
    assertTrue(triggerCode.getAllMatches().contains("match1"));
    assertTrue(triggerCode.getAllMatches().contains("match2"));
  }
}
