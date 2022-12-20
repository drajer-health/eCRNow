package com.drajer.ecrapp.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.time.Instant;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
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

  @Test
  public void testCalculateNewTimeForTimer_Timer_In_OffHours() {

    Instant ct = Calendar.getInstance(TimeZone.getTimeZone("UTC")).getTime().toInstant();

    int lowHours = ct.atZone(ZoneOffset.UTC).getHour() - 2;
    int lowMin = ct.atZone(ZoneOffset.UTC).getMinute();

    int highHours = ct.atZone(ZoneOffset.UTC).getHour() + 3;
    int highMin = 0;

    String tz = "UTC";
    Duration d = null;

    Instant t =
        ApplicationUtils.calculateNewTimeForTimer(lowHours, lowMin, highHours, highMin, tz, d, ct);

    assertNotNull(t);

    assertTrue(t.isAfter(ct));

    assertTrue(
        t.atZone(ZoneOffset.UTC).getDayOfMonth()
            >= (ct.atZone(ZoneOffset.UTC).getDayOfMonth() + 1));
    assertTrue(t.getEpochSecond() > (ct.getEpochSecond() + 86400));
  }

  @Test
  public void testCalculateNewTimeForTimer_Timer_Before_OffHours() {

    Instant ct = Calendar.getInstance(TimeZone.getTimeZone("UTC")).getTime().toInstant();

    int lowHours = ct.atZone(ZoneOffset.UTC).getHour() + 2;
    int lowMin = ct.atZone(ZoneOffset.UTC).getMinute();

    int highHours = ct.atZone(ZoneOffset.UTC).getHour() + 6;
    int highMin = 0;

    String tz = "UTC";
    Duration d = null;

    Instant t =
        ApplicationUtils.calculateNewTimeForTimer(lowHours, lowMin, highHours, highMin, tz, d, ct);

    assertNotNull(t);

    assertTrue(t.isAfter(ct));
    assertTrue(t.getEpochSecond() > ct.getEpochSecond());

    Calendar st = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
    st.set(Calendar.HOUR_OF_DAY, lowHours);
    st.set(Calendar.MINUTE, lowMin);

    Instant stTime = st.getTime().toInstant();
    assertTrue(t.getEpochSecond() >= stTime.getEpochSecond());

    Calendar ht = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
    ht.set(Calendar.HOUR_OF_DAY, highHours);
    ht.set(Calendar.MINUTE, highMin);

    Instant htTime = ht.getTime().toInstant();
    assertTrue(t.getEpochSecond() <= htTime.getEpochSecond());
  }

  @Test
  public void testCalculateNewTimeForTimer_Timer_After_OffHours() {

    Instant ct = Calendar.getInstance(TimeZone.getTimeZone("UTC")).getTime().toInstant();

    int lowHours = ct.atZone(ZoneOffset.UTC).getHour() - 5;
    int lowMin = ct.atZone(ZoneOffset.UTC).getMinute();

    int highHours = ct.atZone(ZoneOffset.UTC).getHour() - 3;
    int highMin = 0;

    String tz = "UTC";
    Duration d = null;

    Instant t =
        ApplicationUtils.calculateNewTimeForTimer(lowHours, lowMin, highHours, highMin, tz, d, ct);

    assertNotNull(t);

    assertTrue(t.isAfter(ct));
    assertTrue(t.getEpochSecond() > ct.getEpochSecond());

    Calendar st = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
    st.add(Calendar.DATE, 1);
    st.set(Calendar.HOUR_OF_DAY, lowHours);
    st.set(Calendar.MINUTE, lowMin);

    Instant stTime = st.getTime().toInstant();
    assertTrue(t.getEpochSecond() >= stTime.getEpochSecond());

    Calendar ht = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
    ht.add(Calendar.DATE, 1);
    ht.set(Calendar.HOUR_OF_DAY, highHours);
    ht.set(Calendar.MINUTE, highMin);

    Instant htTime = ht.getTime().toInstant();
    assertTrue(t.getEpochSecond() <= htTime.getEpochSecond());
  }
}
