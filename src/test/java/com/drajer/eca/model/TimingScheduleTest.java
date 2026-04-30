package com.drajer.eca.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.math.BigDecimal;
import org.hl7.fhir.r4.model.Timing.UnitsOfTime;
import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.junit.Test;

public class TimingScheduleTest {

  @Test
  public void testSettersAndGetters() {

    TimingSchedule ts = new TimingSchedule();

    ts.setTriggerType(TriggerType.PERIODIC);
    ts.setNumOfRepeat(5);
    ts.setMaxRepeat(10);
    ts.setFrequency(2);
    ts.setFrequencyMax(4);
    ts.setFrequencyPeriod(new BigDecimal("3"));
    ts.setFrequencyPeriodUnit(UnitsOfTime.D);
    ts.setDuration(new BigDecimal("7"));
    ts.setDurationUnit(UnitsOfTime.H);

    assertEquals(TriggerType.PERIODIC, ts.getTriggerType());
    assertEquals(5, ts.getNumOfRepeat());
    assertEquals(10, ts.getMaxRepeat());
    assertEquals(2, ts.getFrequency());
    assertEquals(4, ts.getFrequencyMax());
    assertEquals(new BigDecimal("3"), ts.getFrequencyPeriod());
    assertEquals(UnitsOfTime.D, ts.getFrequencyPeriodUnit());
    assertEquals(new BigDecimal("7"), ts.getDuration());
    assertEquals(UnitsOfTime.H, ts.getDurationUnit());
  }

  @Test
  public void testPrintMethodCoverage() {

    TimingSchedule ts = new TimingSchedule();
    ts.setTriggerType(TriggerType.PERIODIC);
    ts.setNumOfRepeat(1);
    ts.print();
    assertNotNull(ts);
  }

  @Test
  public void testEnumValues() {
    TimingSchedule.TimingUnits[] values = TimingSchedule.TimingUnits.values();
    assertNotNull(values);
    assertEquals(7, values.length);
  }

  @Test
  public void testValueOf() {

    TimingSchedule.TimingUnits unit = TimingSchedule.TimingUnits.valueOf("DAYS");
    assertEquals(TimingSchedule.TimingUnits.DAYS, unit);
  }

  @Test
  public void testEnumConstantAccess() {
    TimingSchedule.TimingUnits unit = TimingSchedule.TimingUnits.HOURS;
    assertNotNull(unit);
    assertEquals("HOURS", unit.name());
  }
}
