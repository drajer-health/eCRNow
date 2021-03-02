package com.drajer.ecrapp.util;

import static org.junit.Assert.assertNotNull;

import java.time.Instant;
import org.hl7.fhir.r4.model.Duration;
import org.junit.Test;

public class ApplicationUtilsTest {

  @Test
  public void testConvertDurationToInstant_sec() {

    Duration d = new Duration();
    d.setUnit("s");
    d.setValue(12345);

    Instant instant = ApplicationUtils.convertDurationToInstant(d);
    assertNotNull(instant);
  }

  @Test
  public void testConvertDurationToInstant_min() {

    Duration d = new Duration();
    d.setUnit("min");
    d.setValue(12345);

    Instant instant = ApplicationUtils.convertDurationToInstant(d);
    assertNotNull(instant);
  }

  @Test
  public void testConvertDurationToInstant_hour() {

    Duration d = new Duration();
    d.setUnit("h");
    d.setValue(12345);

    Instant instant = ApplicationUtils.convertDurationToInstant(d);
    assertNotNull(instant);
  }
}
