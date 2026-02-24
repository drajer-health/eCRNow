package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;

import java.lang.reflect.Field;
import java.util.HashMap;
import org.junit.Before;
import org.junit.Test;

public class ReportCreatorTest {

  private static final String HCS_PROFILE =
      "http://hl7.org/fhir/us/health-care-surveys-reporting/StructureDefinition/hcs-reporting-bundle";

  private ReportCreator creator;

  @Before
  public void setUp() {
    creator = ReportCreator.getReportCreator(HCS_PROFILE);
  }

  @Test
  public void testInitReportingClasses_ClassNotFound() throws Exception {
    Field field = ReportCreator.class.getDeclaredField("reportingClasses");
    field.setAccessible(true);
    HashMap<String, ReportCreator> map = (HashMap<String, ReportCreator>) field.get(null);
    map.clear();
    map.put(HCS_PROFILE, null);
    ReportCreator result = ReportCreator.getReportCreator(HCS_PROFILE);
    assertNull(result);
    assertEquals(1, map.size());
    assertTrue(map.containsKey(HCS_PROFILE));
  }
}
