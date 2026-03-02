package com.drajer.cda.parser;

import static org.junit.Assert.assertEquals;

import java.util.Collections;
import org.junit.Test;

public class CdaRrModelTest {

  @Test
  public void testGetPatId() {
    CdaRrModel cdaRrModel = new CdaRrModel();
    CdaIi setId = new CdaIi();
    setId.setExtValue("PAT123|ENC456");
    cdaRrModel.setSetId(Collections.singletonList(setId));
    String patId = cdaRrModel.getPatId();
    assertEquals("PAT123", patId);
  }

  @Test
  public void testGetPatId_WhenPatientIdDoesNotContainPatientPrefix() {
    CdaRrModel cdaRrModel = new CdaRrModel();
    CdaIi patientId = new CdaIi();
    patientId.setExtValue("ABCDEF");
    cdaRrModel.setPatientId(Collections.singletonList(patientId));
    String patId = cdaRrModel.getPatId();
    assertEquals("ABCDEF", patId);
  }

  @Test
  public void testGetPatId_WhenPatientIdContainsPatientPrefix() {
    CdaRrModel cdaRrModel = new CdaRrModel();
    CdaIi patientId = new CdaIi();
    patientId.setExtValue("Patient/12345");

    cdaRrModel.setPatientId(Collections.singletonList(patientId));
    String patId = cdaRrModel.getPatId();
    assertEquals("12345", patId);
  }

  @Test
  public void testGetEnctId_FromSetId() {
    CdaRrModel cdaRrModel = new CdaRrModel();

    CdaIi setId = new CdaIi();
    setId.setExtValue("PAT123|ENC456");

    cdaRrModel.setSetId(Collections.singletonList(setId));

    String enctId = cdaRrModel.getEnctId();

    assertEquals("ENC456", enctId);
  }

  @Test
  public void testGetEnctId_WhenEncounterIdDoesNotContainEncounterPrefix() {
    CdaRrModel cdaRrModel = new CdaRrModel();

    CdaIi encounterId = new CdaIi();
    encounterId.setExtValue("ENC789");

    cdaRrModel.setEncounterId(Collections.singletonList(encounterId));

    String enctId = cdaRrModel.getEnctId();

    assertEquals("ENC789", enctId);
  }

  @Test
  public void testGetEnctId_WhenEncounterIdContainsEncounterPrefix() {
    CdaRrModel cdaRrModel = new CdaRrModel();

    CdaIi encounterId = new CdaIi();
    encounterId.setExtValue("Encounter/ENC999");

    cdaRrModel.setEncounterId(Collections.singletonList(encounterId));

    String enctId = cdaRrModel.getEnctId();

    assertEquals("ENC999", enctId);
  }

  @Test
  public void testSetRrDocId_WhenListIsNotEmpty() {
    CdaRrModel cdaRrModel = new CdaRrModel();

    CdaIi rrDocId = new CdaIi();
    rrDocId.setExtValue("RR-123");

    cdaRrModel.setRrDocId(Collections.singletonList(rrDocId));

    assertEquals("RR-123", cdaRrModel.getRrDocId().getExtValue());
  }

  @Test
  public void testSetRrDocId_WhenListIsEmpty() {
    CdaRrModel cdaRrModel = new CdaRrModel();
    cdaRrModel.setRrDocId(Collections.emptyList());
    assertEquals(null, cdaRrModel.getRrDocId());
  }
}
