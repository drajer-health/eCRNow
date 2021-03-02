package com.drajer.ecrapp.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import org.hl7.fhir.r4.model.Duration;
import org.junit.Test;

public class ApplicationUtilsTest {

  @Test
  public void testConvertDurationToInstant_sec() {

    Duration d = new Duration();
    d.setUnit("s");
    d.setValue(10);

    Instant expectedInstant = new Date().toInstant().plusSeconds(d.getValue().longValue());
    Instant actualInstant = ApplicationUtils.convertDurationToInstant(d);

    assertNotNull(actualInstant);
    assertEquals(
        expectedInstant.truncatedTo(ChronoUnit.SECONDS),
        actualInstant.truncatedTo(ChronoUnit.SECONDS));
  }

  @Test
  public void testConvertDurationToInstant_sec_upperCase() {

    Duration d = new Duration();
    d.setUnit("S");
    d.setValue(10);

    Instant expectedInstant = new Date().toInstant().plusSeconds(d.getValue().longValue());
    Instant actualInstant = ApplicationUtils.convertDurationToInstant(d);

    assertNotNull(actualInstant);
    assertEquals(
        expectedInstant.truncatedTo(ChronoUnit.SECONDS),
        actualInstant.truncatedTo(ChronoUnit.SECONDS));
  }

  @Test
  public void testConvertDurationToInstant_min() {

    Duration d = new Duration();
    d.setUnit("min");
    d.setValue(10);

    Instant expectedInstant = new Date().toInstant().plusSeconds(d.getValue().longValue() * 60);
    Instant actualInstant = ApplicationUtils.convertDurationToInstant(d);

    assertNotNull(actualInstant);
    assertEquals(
        expectedInstant.truncatedTo(ChronoUnit.MINUTES),
        actualInstant.truncatedTo(ChronoUnit.MINUTES));
  }

  @Test
  public void testConvertDurationToInstant_min_upperCase() {

    Duration d = new Duration();
    d.setUnit("MIN");
    d.setValue(10);

    Instant expectedInstant = new Date().toInstant().plusSeconds(d.getValue().longValue() * 60);
    Instant actualInstant = ApplicationUtils.convertDurationToInstant(d);

    assertNotNull(actualInstant);
    assertEquals(
        expectedInstant.truncatedTo(ChronoUnit.MINUTES),
        actualInstant.truncatedTo(ChronoUnit.MINUTES));
  }

  @Test
  public void testConvertDurationToInstant_hour() {

    Duration d = new Duration();
    d.setUnit("h");
    d.setValue(1);

    Instant expectedInstant =
        new Date().toInstant().plusSeconds(d.getValue().longValue() * 60 * 60);
    Instant actualInstant = ApplicationUtils.convertDurationToInstant(d);

    assertNotNull(actualInstant);
    assertEquals(
        expectedInstant.truncatedTo(ChronoUnit.MINUTES),
        actualInstant.truncatedTo(ChronoUnit.MINUTES));
  }

  @Test
  public void testConvertDurationToInstant_hour_upperCase() {

    Duration d = new Duration();
    d.setUnit("H");
    d.setValue(1);

    Instant expectedInstant =
        new Date().toInstant().plusSeconds(d.getValue().longValue() * 60 * 60);
    Instant actualInstant = ApplicationUtils.convertDurationToInstant(d);

    assertNotNull(actualInstant);
    assertEquals(
        expectedInstant.truncatedTo(ChronoUnit.MINUTES),
        actualInstant.truncatedTo(ChronoUnit.MINUTES));
  }
}
