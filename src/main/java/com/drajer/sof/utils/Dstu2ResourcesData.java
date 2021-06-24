package com.drajer.sof.utils;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Condition;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticOrder;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticOrder.Event;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticReport;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.model.dstu2.resource.Immunization;
import ca.uhn.fhir.model.dstu2.resource.Medication;
import ca.uhn.fhir.model.dstu2.resource.MedicationAdministration;
import ca.uhn.fhir.model.dstu2.resource.MedicationStatement;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class Dstu2ResourcesData {

  @Autowired FhirContextInitializer resourceData;

  private final Logger logger = LoggerFactory.getLogger(Dstu2ResourcesData.class);

  private List<CodeableConceptDt> findEncounterCodes(Encounter encounter) {
    List<CodeableConceptDt> encounterCodes = new ArrayList<>();
    if (encounter.getType() != null) {
      encounterCodes = encounter.getType();
    }
    return encounterCodes;
  }

  private List<CodeableConceptDt> findConditionCodes(Condition condition) {
    List<CodeableConceptDt> conditionCodes = new ArrayList<>();
    if (!condition.getCode().isEmpty() && condition.getCode() != null) {
      conditionCodes.add(condition.getCode());
    }
    return conditionCodes;
  }

  private List<CodeableConceptDt> findLaboratoryCodes(Observation observation) {
    List<CodeableConceptDt> observationCodes = new ArrayList<>();

    if (!observation.getCode().isEmpty() && observation.getCode() != null) {
      observationCodes.add(observation.getCode());
    }
    return observationCodes;
  }

  private List<CodeableConceptDt> findMedicationCodes(MedicationAdministration medAdministration) {
    List<CodeableConceptDt> medicationCodes = new ArrayList<>();

    if (!medAdministration.getMedication().isEmpty() && medAdministration.getMedication() != null) {
      if (medAdministration.getMedication() instanceof CodeableConceptDt) {
        // Handle Codeable Concept
        CodeableConceptDt medicationCode = (CodeableConceptDt) medAdministration.getMedication();
        medicationCodes.add(medicationCode);
      } else {
        // Handle Reference data types
      }
    }
    return medicationCodes;
  }

  private List<CodeableConceptDt> findMedicationStatementCodes(MedicationStatement medStatement) {
    List<CodeableConceptDt> medicationCodes = new ArrayList<>();

    if (!medStatement.getMedication().isEmpty() && medStatement.getMedication() != null) {
      if (medStatement.getMedication() instanceof CodeableConceptDt) {
        // Handle Codeable Concept
        CodeableConceptDt medicationCode = (CodeableConceptDt) medStatement.getMedication();
        medicationCodes.add(medicationCode);
      } else {
        // Handle Reference data types
      }
    }
    return medicationCodes;
  }

  private List<CodeableConceptDt> findDiagnosticOrderCodes(DiagnosticOrder diagnosticOrder) {
    List<CodeableConceptDt> diagnosticOrderCodes = new ArrayList<>();
    if (!diagnosticOrder.getReason().isEmpty() && diagnosticOrder.getReason() != null) {
      diagnosticOrderCodes.addAll(diagnosticOrder.getReason());
    }
    return diagnosticOrderCodes;
  }

  private List<CodeableConceptDt> findDiagnosticReportCodes(DiagnosticReport diagnosticReport) {
    List<CodeableConceptDt> diagnosticReportCodes = new ArrayList<>();

    if (!diagnosticReport.getCode().isEmpty() && diagnosticReport.getCode() != null) {
      diagnosticReportCodes.add(diagnosticReport.getCode());
    }
    return diagnosticReportCodes;
  }

  private List<CodeableConceptDt> findImmunizationCodes(Immunization immunization) {

    List<CodeableConceptDt> immunizationCodes = new ArrayList<>();
    if (!immunization.getVaccineCode().isEmpty() && immunization.getVaccineCode() != null) {
      immunizationCodes.add(immunization.getVaccineCode());
    }

    return immunizationCodes;
  }

  public Encounter getEncounterData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      Date start,
      Date end) {
    Encounter encounter;
    // If Encounter Id is present in Launch Details
    if (launchDetails.getEncounterId() != null) {
      encounter =
          (Encounter)
              resourceData.getResouceById(
                  launchDetails, client, context, "Encounter", launchDetails.getEncounterId());
      dstu2FhirData.setEncounterCodes(findEncounterCodes(encounter));
    } else {
      // If Encounter Id is not Present in Launch Details Get Encounters by Patient Id
      // and Find the latest Encounter
      Bundle bundle =
          (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context, "Encounter");
      Map<Encounter, Date> encounterMap = new HashMap<>();
      for (Entry entry : bundle.getEntry()) {
        Encounter encounterEntry = (Encounter) entry.getResource();
        // Checking if Period element exists in Encounter. If Exists compare period is
        // falling in between Start and End Date
        if (!encounterEntry.getPeriod().isEmpty()) {
          PeriodDt period = encounterEntry.getPeriod();
          if (period.getStart().after(start) || period.getEnd().before(end)) {
            encounterMap.put(encounterEntry, encounterEntry.getMeta().getLastUpdated());
          }
          // If period is not present using LastUpdatedDate in meta information to filter
          // the Encounter
        } else {
          Date lastUpdatedDateTime = encounterEntry.getMeta().getLastUpdated();
          if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
            encounterMap.put(encounterEntry, encounterEntry.getMeta().getLastUpdated());
          }
        }
      }
      encounter = Collections.max(encounterMap.entrySet(), Map.Entry.comparingByValue()).getKey();
      dstu2FhirData.setEncounterCodes(findEncounterCodes(encounter));
    }
    return encounter;
  }

  public List<Condition> getConditionData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context, "Condition");
    List<Condition> conditions = new ArrayList<>();
    List<CodeableConceptDt> conditionCodes = new ArrayList<>();
    // Filter Conditions based on Encounter Reference
    if (encounter != null && !encounter.getId().getValue().isEmpty()) {
      for (Entry entry : bundle.getEntry()) {
        Condition condition = (Condition) entry.getResource();
        if (!condition.getEncounter().isEmpty()) {
          if (condition
              .getEncounter()
              .getReference()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            conditions.add(condition);
            conditionCodes.addAll(findConditionCodes(condition));
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter conditions
    } else {
      for (Entry entry : bundle.getEntry()) {
        Condition condition = (Condition) entry.getResource();
        // Checking If Date Recorded present in Condition resource
        if (condition.getDateRecorded() != null) {
          if (condition.getDateRecorded().after(start) && condition.getDateRecorded().before(end)) {
            conditions.add(condition);
            conditionCodes.addAll(findConditionCodes(condition));
          }
          // If Date Recorded is not present using LastUpdatedDate
        } else {
          Date lastUpdatedDateTime = condition.getMeta().getLastUpdated();
          if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
            conditions.add(condition);
            conditionCodes.addAll(findConditionCodes(condition));
          }
        }
      }
    }
    dstu2FhirData.setConditionCodes(conditionCodes);
    return conditions;
  }

  public List<Observation> getObservationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getObservationByPatientId(
                launchDetails, client, context, "Observation", "laboratory");
    List<Observation> observations = new ArrayList<>();
    List<CodeableConceptDt> observationCodes = new ArrayList<>();
    // Filter Observations based on Encounter Reference
    if (encounter != null && !encounter.getId().getValue().isEmpty()) {
      for (Entry entry : bundle.getEntry()) {
        Observation observation = (Observation) entry.getResource();
        if (!observation.getEncounter().isEmpty()) {
          if (observation
              .getEncounter()
              .getReference()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            observations.add(observation);
            observationCodes.addAll(findLaboratoryCodes(observation));
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // Observations
    } else {
      for (Entry entry : bundle.getEntry()) {
        Observation observation = (Observation) entry.getResource();
        // Checking If Issued Date is present in Observation resource
        if (observation.getIssued() != null) {
          if (observation.getIssued().after(start) && observation.getIssued().before(end)) {
            observations.add(observation);
            observationCodes.addAll(findLaboratoryCodes(observation));
          }
          // If Issued date is not present, Checking for Effective Date
        } else if (!observation.getEffective().isEmpty()) {
          Date effectiveDate = (Date) observation.getEffective();
          if (effectiveDate.after(start) && effectiveDate.before(end)) {
            observations.add(observation);
            observationCodes.addAll(findLaboratoryCodes(observation));
          }
          // If Issued and Effective Date are not present looking for LastUpdatedDate
        } else {
          Date lastUpdatedDateTime = observation.getMeta().getLastUpdated();
          if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
            observations.add(observation);
            observationCodes.addAll(findLaboratoryCodes(observation));
          }
        }
      }
    }
    dstu2FhirData.setLabResultCodes(observationCodes);
    return observations;
  }

  public List<Observation> getPregnancyObservationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientIdAndCode(
                launchDetails,
                client,
                context,
                "Observation",
                QueryConstants.PREGNANCY_CODE,
                QueryConstants.LOINC_CODE_SYSTEM);
    List<Observation> observations = filterObservation(bundle, encounter, start, end);

    return observations;
  }

  public List<Observation> getTravelObservationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientIdAndCode(
                launchDetails,
                client,
                context,
                "Observation",
                QueryConstants.TRAVEL_CODE,
                QueryConstants.LOINC_CODE_SYSTEM);
    List<Observation> observations = filterObservation(bundle, encounter, start, end);

    return observations;
  }

  public Medication getMedicationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      String medicationId) {
    Medication medication =
        (Medication)
            resourceData.getResouceById(launchDetails, client, context, "Medication", medicationId);
    return medication;
  }

  public List<MedicationAdministration> getMedicationAdministrationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(
                launchDetails, client, context, "MedicationAdministration");
    List<MedicationAdministration> medAdministrations = new ArrayList<>();
    List<CodeableConceptDt> medicationCodes = new ArrayList<>();
    // Filter MedicationAdministrations based on Encounter Reference
    if (bundle != null && encounter != null && !encounter.getId().getValue().isEmpty()) {
      for (Entry entry : bundle.getEntry()) {
        MedicationAdministration medAdministration = (MedicationAdministration) entry.getResource();
        if (!medAdministration.getEncounter().isEmpty()) {
          if (medAdministration
              .getEncounter()
              .getReference()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            medAdministrations.add(medAdministration);
            medicationCodes.addAll(findMedicationCodes(medAdministration));
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // MedicationAdministrations
    } else if (bundle != null) {
      for (Entry entry : bundle.getEntry()) {
        MedicationAdministration medAdministration = (MedicationAdministration) entry.getResource();
        // Checking If Effective Date is present in MedicationAdministration resource
        if (medAdministration.getEffectiveTime() != null) {
          Date effectiveDateTime = (Date) medAdministration.getEffectiveTime();
          if (effectiveDateTime.after(start) && effectiveDateTime.before(end)) {
            medAdministrations.add(medAdministration);
            medicationCodes.addAll(findMedicationCodes(medAdministration));
          }
        }
        // If Effective Date is not present looking for LastUpdatedDate
        else {
          Date lastUpdatedDateTime = medAdministration.getMeta().getLastUpdated();
          if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
            medAdministrations.add(medAdministration);
            medicationCodes.addAll(findMedicationCodes(medAdministration));
          }
        }
      }
    }
    dstu2FhirData.setMedicationCodes(medicationCodes);
    return medAdministrations;
  }

  public List<MedicationStatement> getMedicationStatementData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(
                launchDetails, client, context, "MedicationStatement");
    List<MedicationStatement> medStatements = new ArrayList<>();
    List<CodeableConceptDt> medicationCodes = new ArrayList<>();
    if (bundle != null) {
      for (Entry entry : bundle.getEntry()) {
        MedicationStatement medStatement = (MedicationStatement) entry.getResource();
        // Checking If Effective Date is present in MedicationAdministration resource
        if (medStatement.getEffective() != null) {
          Date effectiveDateTime = (Date) medStatement.getEffective();
          if (effectiveDateTime.after(start) && effectiveDateTime.before(end)) {
            medStatements.add(medStatement);
            medicationCodes.addAll(findMedicationStatementCodes(medStatement));
          }
        }
        // If Effective Date is not present looking for LastUpdatedDate
        else {
          Date lastUpdatedDateTime = medStatement.getMeta().getLastUpdated();
          if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
            medStatements.add(medStatement);
            medicationCodes.addAll(findMedicationStatementCodes(medStatement));
          }
        }
      }
    }
    dstu2FhirData.setMedicationCodes(medicationCodes);
    return medStatements;
  }

  public List<DiagnosticOrder> getDiagnosticOrderData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(launchDetails, client, context, "DiagnosticOrder");
    List<DiagnosticOrder> diagnosticOrders = new ArrayList<>();
    List<CodeableConceptDt> diagnosticOrderCodes = new ArrayList<>();
    // Filter DiagnosticOrders based on Encounter Reference
    if (encounter != null && !encounter.getId().getValue().isEmpty()) {
      for (Entry entry : bundle.getEntry()) {
        DiagnosticOrder diagnosticOrder = (DiagnosticOrder) entry.getResource();
        if (!diagnosticOrder.getEncounter().isEmpty()) {
          if (diagnosticOrder
              .getEncounter()
              .getReference()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            diagnosticOrders.add(diagnosticOrder);
            diagnosticOrderCodes.addAll(findDiagnosticOrderCodes(diagnosticOrder));
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // DiagnosticOrders
    } else {
      for (Entry entry : bundle.getEntry()) {
        DiagnosticOrder diagnosticOrder = (DiagnosticOrder) entry.getResource();
        // Checking If DiagnosticOrder.Event Date is present in DiagnosticOrder resource
        if (diagnosticOrder.getEvent() != null) {
          List<Event> diagnosticOrderEvents = diagnosticOrder.getEvent();
          for (Event diagnosticOrderEvent : diagnosticOrderEvents) {
            if (diagnosticOrderEvent.getDateTime().after(start)
                && diagnosticOrderEvent.getDateTime().before(end)) {
              diagnosticOrders.add(diagnosticOrder);
              diagnosticOrderCodes.addAll(findDiagnosticOrderCodes(diagnosticOrder));
            }
          }
        }
        // If DiagnosticOrder.Event Date is not present looking for LastUpdatedDate
        else {
          Date lastUpdatedDateTime = diagnosticOrder.getMeta().getLastUpdated();
          if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
            diagnosticOrders.add(diagnosticOrder);
            diagnosticOrderCodes.addAll(findDiagnosticOrderCodes(diagnosticOrder));
          }
        }
      }
    }
    dstu2FhirData.setDiagnosticOrderCodes(diagnosticOrderCodes);
    return diagnosticOrders;
  }

  public List<Immunization> getImmunizationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(launchDetails, client, context, "Immunization");
    List<Immunization> immunizations = new ArrayList<>();
    List<CodeableConceptDt> immunizationCodes = new ArrayList<>();
    // Filter Immunizations based on Encounter Reference
    if (encounter != null && !encounter.getId().getValue().isEmpty()) {
      for (Entry entry : bundle.getEntry()) {
        Immunization immunization = (Immunization) entry.getResource();
        if (!immunization.getEncounter().isEmpty()) {
          if (immunization
              .getEncounter()
              .getReference()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            immunizations.add(immunization);
            immunizationCodes.addAll(findImmunizationCodes(immunization));
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // Immunizations
    } else {
      for (Entry entry : bundle.getEntry()) {
        Immunization immunization = (Immunization) entry.getResource();
        // Checking If Immunization DateTime is present in Immunization
        // resource
        if (immunization.getDate() != null) {
          if (immunization.getDate().after(start) && immunization.getDate().before(end)) {
            immunizations.add(immunization);
            immunizationCodes.addAll(findImmunizationCodes(immunization));
          }
        }
        // If Immunization Date is not present looking for LastUpdatedDate
        else {
          Date lastUpdatedDateTime = immunization.getMeta().getLastUpdated();
          if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
            immunizations.add(immunization);
            immunizationCodes.addAll(findImmunizationCodes(immunization));
          }
        }
      }
    }
    dstu2FhirData.setImmuniationCodes(immunizationCodes);
    return immunizations;
  }

  public List<DiagnosticReport> getDiagnosticReportData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      Dstu2FhirData dstu2FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(launchDetails, client, context, "DiagnosticReport");
    List<DiagnosticReport> diagnosticReports = new ArrayList<>();
    List<CodeableConceptDt> diagnosticReportCodes = new ArrayList<>();
    // Filter DiagnosticReports based on Encounter Reference
    if (bundle != null && bundle.getEntry() != null) {
      if (encounter != null && !encounter.getId().getValue().isEmpty()) {
        for (Entry entry : bundle.getEntry()) {
          DiagnosticReport diagnosticReport = (DiagnosticReport) entry.getResource();
          if (!diagnosticReport.getEncounter().isEmpty()) {
            if (diagnosticReport
                .getEncounter()
                .getReference()
                .getIdPart()
                .equals(encounter.getIdElement().getIdPart())) {
              diagnosticReports.add(diagnosticReport);
              diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
            }
          }
        }
        // If Encounter Id is not present using start and end dates to filter
        // DiagnosticReports
      } else {
        for (Entry entry : bundle.getEntry()) {
          DiagnosticReport diagnosticReport = (DiagnosticReport) entry.getResource();
          // Checking If Issued Date is present in DiagnosticReport resource
          if (diagnosticReport.getIssued() != null) {
            if (diagnosticReport.getIssued().after(start)
                && diagnosticReport.getIssued().before(end)) {
              diagnosticReports.add(diagnosticReport);
              diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
            }
            // If Issued date is not present, Checking for Effective Date
          } else if (!diagnosticReport.getEffective().isEmpty()) {
            Date effectiveDate = (Date) diagnosticReport.getEffective();
            if (effectiveDate.after(start) && effectiveDate.before(end)) {
              diagnosticReports.add(diagnosticReport);
              diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
            }
            // If Issued and Effective Date are not present looking for LastUpdatedDate
          } else {
            Date lastUpdatedDateTime = diagnosticReport.getMeta().getLastUpdated();
            if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
              diagnosticReports.add(diagnosticReport);
              diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
            }
          }
        }
      }
    }
    dstu2FhirData.setDiagnosticReportCodes(diagnosticReportCodes);
    return diagnosticReports;
  }

  public static List<Observation> filterObservation(
      Bundle bundle, Encounter encounter, Date start, Date end) {

    List<Observation> observations = new ArrayList<>();
    // Filter Observations based on Encounter Reference
    if (encounter != null && !encounter.getId().getValue().isEmpty()) {
      for (Entry entry : bundle.getEntry()) {
        Observation observation = (Observation) entry.getResource();
        if (!observation.getEncounter().isEmpty()) {
          if (observation
              .getEncounter()
              .getReference()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            observations.add(observation);
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // Observations
    } else {
      for (Entry entry : bundle.getEntry()) {
        Observation observation = (Observation) entry.getResource();
        // Checking If Issued Date is present in Observation resource
        if (observation.getIssued() != null) {
          if (observation.getIssued().after(start) && observation.getIssued().before(end)) {
            observations.add(observation);
          }
          // If Issued date is not present, Checking for Effective Date
        } else if (!observation.getEffective().isEmpty()) {
          Date effectiveDate = (Date) observation.getEffective();
          if (effectiveDate.after(start) && effectiveDate.before(end)) {
            observations.add(observation);
          }
          // If Issued and Effective Date are not present looking for LastUpdatedDate
        } else {
          Date lastUpdatedDateTime = observation.getMeta().getLastUpdated();
          if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
            observations.add(observation);
          }
        }
      }
    }
    return observations;
  }
}
