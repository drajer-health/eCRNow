package com.drajer.sof.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;
import org.hl7.fhir.r4.model.Meta;
import org.hl7.fhir.r4.model.Patient;
import org.junit.Test;

public class ResourceUtilsTest {

  @Test
  public void testDeduplicate_PicksHighestVersion() {
    Patient p1 = new Patient();
    p1.setId("Patient/1");
    p1.setMeta(new Meta().setVersionId("1"));
    Patient p2 = new Patient();
    p2.setId("Patient/1");
    p2.setMeta(new Meta().setVersionId("3"));
    List<Patient> result = ResourceUtils.deduplicate(Arrays.asList(p1, p2));
    assertEquals(1, result.size());
    assertEquals("3", result.get(0).getMeta().getVersionId());
  }

  @Test
  public void testDeduplicate_MetaIsNull() {
    Patient p1 = new Patient();
    p1.setId("Patient/2");
    Patient p2 = new Patient();
    p2.setId("Patient/2");
    p2.setMeta(new Meta().setVersionId("1"));
    List<Patient> result = ResourceUtils.deduplicate(Arrays.asList(p1, p2));
    assertEquals(1, result.size());
    assertTrue(result.get(0).getMeta().getVersionId().equals("1"));
  }

  @Test
  public void testDeduplicate_VersionIdIsNull() {
    Patient p1 = new Patient();
    p1.setId("Patient/3");
    p1.setMeta(new Meta());
    Patient p2 = new Patient();
    p2.setId("Patient/3");
    p2.setMeta(new Meta().setVersionId("2"));
    List<Patient> result = ResourceUtils.deduplicate(Arrays.asList(p1, p2));
    assertEquals(1, result.size());
    assertEquals("2", result.get(0).getMeta().getVersionId());
  }

  @Test
  public void testDeduplicate_DifferentIds() {
    Patient p1 = new Patient();
    p1.setId("Patient/4");
    Patient p2 = new Patient();
    p2.setId("Patient/5");
    List<Patient> result = ResourceUtils.deduplicate(Arrays.asList(p1, p2));
    assertEquals(2, result.size());
  }
}
