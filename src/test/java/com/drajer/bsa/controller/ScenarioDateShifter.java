package com.drajer.bsa.controller;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.Set;
import org.hl7.fhir.r4.model.Base;
import org.hl7.fhir.r4.model.BaseDateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Property;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Shifts every date/dateTime/instant value inside a FHIR resource by a configurable number of
 * days. Used by integration tests to keep scenarios from going stale relative to {@code now()}.
 *
 * <p>Test scenarios author dates with hard-coded ISO timestamps (e.g.,
 * {@code "effectiveDateTime": "2026-04-13T..."}). When real time moves on, those dates fall
 * outside the PlanDefinition's reporting window and clauses like {@code %encounterStartDate +
 * %normalReportingDuration >= now()} flip to false, masking the actual filter logic under test.
 *
 * <p>By computing a shift that anchors each scenario's Encounter to a stable offset from
 * {@link LocalDate#now()} and applying it uniformly to every date in the resource tree, the
 * relative offsets between dates within a scenario are preserved (a lab effective 30 days before
 * encounter remains 30 days before encounter) while the absolute calendar stays current.
 *
 * <p>Identity-bearing date fields ({@code Patient.birthDate}) are excluded — shifting a patient's
 * DOB on every test run would be both wrong and surprising.
 */
public final class ScenarioDateShifter {

  private static final Logger logger = LoggerFactory.getLogger(ScenarioDateShifter.class);

  /** Property names whose date values must not be shifted. */
  private static final Set<String> SKIP_PROPERTIES = Set.of("birthDate");

  private ScenarioDateShifter() {}

  /**
   * Compute the day-shift needed to anchor the given encounter to {@code today - 1 day}.
   *
   * @return number of days to add to every date in the scenario, or 0 if the encounter has no
   *     resolvable start date (in which case no shift should be applied).
   */
  public static long computeShiftDays(Encounter anchor) {
    if (anchor == null || !anchor.hasPeriod() || anchor.getPeriod().getStart() == null) {
      return 0L;
    }
    LocalDate anchorDate =
        anchor.getPeriod().getStart().toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    LocalDate target = LocalDate.now().minusDays(1);
    return ChronoUnit.DAYS.between(anchorDate, target);
  }

  /**
   * Shift every date/dateTime/instant inside the resource tree by {@code days} days. No-op when
   * {@code days == 0}.
   */
  public static void shift(Base element, long days) {
    if (element == null || days == 0L) {
      return;
    }
    shiftInternal(element, days);
  }

  private static void shiftInternal(Base element, long days) {
    if (element instanceof BaseDateTimeType dt) {
      if (dt.getValue() != null) {
        Date shifted = Date.from(dt.getValue().toInstant().plus(days, ChronoUnit.DAYS));
        dt.setValue(shifted);
      }
      return;
    }
    for (Property prop : element.children()) {
      if (SKIP_PROPERTIES.contains(prop.getName())) {
        continue;
      }
      if (prop.getValues() == null) {
        continue;
      }
      for (Base child : prop.getValues()) {
        if (child != null) {
          shiftInternal(child, days);
        }
      }
    }
  }

  static void logShift(String scenarioName, long days) {
    if (days != 0L) {
      logger.info("Scenario '{}' date shift: {} days", scenarioName, days);
    }
  }
}
