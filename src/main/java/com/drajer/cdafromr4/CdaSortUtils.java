package com.drajer.cdafromr4;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import org.hl7.fhir.r4.model.BaseDateTimeType;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Procedure;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.Type;

/**
 * Provides a consistent display order for entries within eICR sections (GitHub issue #1152).
 *
 * <p>All sorts are most-recent-first. Entries without a usable date are placed at the end. The
 * sorts are stable, so entries with the same date keep their original relative order. The input
 * collections are never modified; a new sorted collection is returned.
 */
public final class CdaSortUtils {

  private static final int ACTIVE_RANK = 0;
  private static final int UNKNOWN_RANK = 1;
  private static final int INACTIVE_RANK = 2;

  private CdaSortUtils() {}

  /** Immunizations by occurrence date, falling back to recorded date. */
  public static List<Immunization> sortImmunizations(List<Immunization> imms) {
    return sortByDateDesc(
        imms,
        imm ->
            firstNonNull(
                toDate(imm.getOccurrence()), imm.hasRecorded() ? imm.getRecorded() : null));
  }

  /**
   * Problems with active ones first and resolved/inactive ones last. Within each group, most recent
   * recorded date first, falling back to the onset date.
   */
  public static List<Condition> sortConditions(List<Condition> conds) {
    if (conds == null || conds.size() < 2) {
      return conds;
    }
    Comparator<Condition> byStatus = Comparator.comparingInt(CdaSortUtils::getClinicalStatusRank);
    Comparator<Condition> byDate =
        Comparator.comparing(
            CdaSortUtils::getConditionDate, Comparator.nullsLast(Comparator.reverseOrder()));
    List<Condition> sorted = new ArrayList<>(conds);
    sorted.sort(byStatus.thenComparing(byDate));
    return sorted;
  }

  /** Procedures by performed date. */
  public static List<Procedure> sortProcedures(List<Procedure> procs) {
    return sortByDateDesc(procs, proc -> toDate(proc.getPerformed()));
  }

  /** Medication statements by effective date, falling back to date asserted. */
  public static List<MedicationStatement> sortMedicationStatements(List<MedicationStatement> meds) {
    return sortByDateDesc(
        meds,
        med ->
            firstNonNull(
                toDate(med.getEffective()), med.hasDateAsserted() ? med.getDateAsserted() : null));
  }

  /** Medication administrations by effective date. */
  public static List<MedicationAdministration> sortMedicationAdministrations(
      List<MedicationAdministration> medAdms) {
    return sortByDateDesc(medAdms, medAdm -> toDate(medAdm.getEffective()));
  }

  /** Medication requests by authored date. */
  public static List<MedicationRequest> sortMedicationRequests(List<MedicationRequest> medReqs) {
    return sortByDateDesc(
        medReqs, medReq -> medReq.hasAuthoredOn() ? medReq.getAuthoredOn() : null);
  }

  /** Observations (results, vital signs) by effective date, falling back to issued date. */
  public static List<Observation> sortObservations(List<Observation> obs) {
    return sortByDateDesc(obs, CdaSortUtils::getObservationDate);
  }

  /** Service requests (planned orders) by occurrence date, falling back to authored date. */
  public static List<ServiceRequest> sortServiceRequests(List<ServiceRequest> srs) {
    return sortByDateDesc(
        srs,
        sr ->
            firstNonNull(
                toDate(sr.getOccurrence()), sr.hasAuthoredOn() ? sr.getAuthoredOn() : null));
  }

  /** Diagnostic reports (lab orders in Plan of Treatment) by effective date, then issued date. */
  public static List<DiagnosticReport> sortDiagnosticReportList(List<DiagnosticReport> reports) {
    return sortByDateDesc(reports, CdaSortUtils::getDiagnosticReportDate);
  }

  /** Observations keyed by id, ordered by effective date. Returns an insertion-ordered map. */
  public static Map<String, Observation> sortObservationMap(Map<String, Observation> obsMap) {
    if (obsMap == null || obsMap.size() < 2) {
      return obsMap;
    }
    List<Map.Entry<String, Observation>> entries = new ArrayList<>(obsMap.entrySet());
    entries.sort(
        Comparator.comparing(
            (Map.Entry<String, Observation> e) -> getObservationDate(e.getValue()),
            Comparator.nullsLast(Comparator.reverseOrder())));
    Map<String, Observation> sorted = new LinkedHashMap<>();
    for (Map.Entry<String, Observation> e : entries) {
      sorted.put(e.getKey(), e.getValue());
    }
    return sorted;
  }

  /**
   * Diagnostic reports by effective date (falling back to issued date), with the observations of
   * each report also sorted. Returns an insertion-ordered map.
   */
  public static Map<DiagnosticReport, List<Observation>> sortDiagnosticReports(
      Map<DiagnosticReport, List<Observation>> reports) {
    if (reports == null || reports.isEmpty()) {
      return reports;
    }
    List<DiagnosticReport> keys =
        sortByDateDesc(new ArrayList<>(reports.keySet()), CdaSortUtils::getDiagnosticReportDate);
    Map<DiagnosticReport, List<Observation>> sorted = new LinkedHashMap<>();
    for (DiagnosticReport dr : keys) {
      sorted.put(dr, sortObservations(reports.get(dr)));
    }
    return sorted;
  }

  /**
   * Generic stable sort, most recent first, entries with no date last. Returns a new list; the
   * input list is not modified.
   */
  public static <T> List<T> sortByDateDesc(List<T> items, Function<T, Date> dateExtractor) {
    if (items == null || items.size() < 2) {
      return items;
    }
    List<T> sorted = new ArrayList<>(items);
    sorted.sort(
        Comparator.comparing(dateExtractor, Comparator.nullsLast(Comparator.reverseOrder())));
    return sorted;
  }

  /** Converts dateTime, date, instant and Period values to a Date. Other types return null. */
  public static Date toDate(Type value) {
    if (value == null || value.isEmpty()) {
      return null;
    }
    if (value instanceof BaseDateTimeType) {
      return ((BaseDateTimeType) value).getValue();
    }
    if (value instanceof Period) {
      Period p = (Period) value;
      return p.hasStart() ? p.getStart() : (p.hasEnd() ? p.getEnd() : null);
    }
    return null;
  }

  static int getClinicalStatusRank(Condition cond) {
    if (cond == null || !cond.hasClinicalStatus() || !cond.getClinicalStatus().hasCoding()) {
      return UNKNOWN_RANK;
    }
    String code = cond.getClinicalStatus().getCodingFirstRep().getCode();
    if (code == null) {
      return UNKNOWN_RANK;
    }
    switch (code) {
      case "active":
      case "recurrence":
      case "relapse":
        return ACTIVE_RANK;
      case "inactive":
      case "remission":
      case "resolved":
        return INACTIVE_RANK;
      default:
        return UNKNOWN_RANK;
    }
  }

  static Date getConditionDate(Condition cond) {
    return firstNonNull(
        cond.hasRecordedDate() ? cond.getRecordedDate() : null, toDate(cond.getOnset()));
  }

  static Date getObservationDate(Observation obs) {
    return firstNonNull(toDate(obs.getEffective()), obs.hasIssued() ? obs.getIssued() : null);
  }

  static Date getDiagnosticReportDate(DiagnosticReport dr) {
    return firstNonNull(toDate(dr.getEffective()), dr.hasIssued() ? dr.getIssued() : null);
  }

  private static Date firstNonNull(Date first, Date second) {
    return first != null ? first : second;
  }
}
