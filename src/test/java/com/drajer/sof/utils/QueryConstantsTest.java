package com.drajer.sof.utils;

import static org.junit.Assert.*;

import java.lang.reflect.Constructor;
import org.junit.Test;

public class QueryConstantsTest {

  @Test
  public void testLoincCodeSystem() {
    assertEquals("http://loinc.org", QueryConstants.LOINC_CODE_SYSTEM);
  }

  @Test
  public void testSnomedCodeSystem() {
    assertEquals("http://snomed.info/sct", QueryConstants.SNOMED_CODE_SYSTEM);
  }

  @Test
  public void testPregnancyCode() {
    assertEquals("90767-5", QueryConstants.PREGNANCY_CODE);
  }

  @Test
  public void testTravelCode() {
    assertEquals("29762-2", QueryConstants.TRAVEL_CODE);
  }

  @Test
  public void testGetTravelHistorySmtCodes() {
    String[] codes = QueryConstants.getTravelHistorySmtCodes();

    assertNotNull(codes);
    assertEquals(6, codes.length);

    assertArrayEquals(
        new String[] {
          "161085007", "443846001", "420008001", "46521000175102", "34831000175105", "161086008"
        },
        codes);
  }

  @Test
  public void testGetOccupationSmtCodes() {
    String[] codes = QueryConstants.getOccupationSmtCodes();

    assertNotNull(codes);
    assertEquals(2, codes.length);

    assertArrayEquals(new String[] {"224362002", "364703007"}, codes);
  }

  @Test
  public void testGetOccupationLoincCodes() {
    String[] codes = QueryConstants.getOccupationLoincCodes();

    assertNotNull(codes);
    assertEquals(2, codes.length);

    assertArrayEquals(new String[] {"11295-3", "11341-5"}, codes);
  }

  @Test
  public void testGetPregnancySmtCodes() {
    String[] codes = QueryConstants.getPregnancySmtCodes();

    assertNotNull(codes);
    assertEquals(1, codes.length);

    assertArrayEquals(new String[] {"77386006"}, codes);
  }

  @Test
  public void testPrivateConstructor() throws Exception {
    Constructor<QueryConstants> constructor = QueryConstants.class.getDeclaredConstructor();

    assertNotNull(constructor);
    constructor.setAccessible(true);

    QueryConstants instance = constructor.newInstance();
    assertNotNull(instance);
  }
}
