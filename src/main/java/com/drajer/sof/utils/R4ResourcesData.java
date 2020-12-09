package com.drajer.sof.utils;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Encounter.EncounterLocationComponent;
import org.hl7.fhir.r4.model.Encounter.EncounterParticipantComponent;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class R4ResourcesData {

  @Autowired FhirContextInitializer resourceData;

  @Autowired FhirContextInitializer fhirContextInitializer;

  private final Logger logger = LoggerFactory.getLogger(R4ResourcesData.class);

  private static final String OBSERVATION = "Observation";
  private static final String CONDITION = "Condition";

  private List<CodeableConcept> findEncounterCodes(Encounter encounter) {
    List<CodeableConcept> encounterCodes = new ArrayList<>();
    if (encounter.getType() != null) {
      encounterCodes = encounter.getType();
    }
    return encounterCodes;
  }

  public Encounter getEncounterData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Date start,
      Date end) {
    Encounter encounter = new Encounter();
    // If Encounter Id is present in Launch Details
    if (launchDetails.getEncounterId() != null) {
      encounter =
          (Encounter)
              resourceData.getResouceById(
                  launchDetails, client, context, "Encounter", launchDetails.getEncounterId());
      r4FhirData.setR4EncounterCodes(findEncounterCodes(encounter));
    } else {
      // If Encounter Id is not Present in Launch Details Get Encounters by Patient Id
      // and Find the latest Encounter
      Bundle bundle =
          (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context, "Encounter");
      Map<Encounter, Date> encounterMap = new HashMap<>();
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Encounter encounterEntry = (Encounter) entry.getResource();
        // Checking if Period element exists in Encounter. If Exists compare period is
        // falling in between Start and End Date
        if (!encounterEntry.getPeriod().isEmpty()) {
          Period period = encounterEntry.getPeriod();
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
      r4FhirData.setR4EncounterCodes(findEncounterCodes(encounter));
    }
    return encounter;
  }

  private List<CodeableConcept> findConditionCodes(Condition condition) {
    List<CodeableConcept> conditionCodes = new ArrayList<>();
    if (!condition.getCode().isEmpty() && condition.getCode() != null) {
      conditionCodes.add(condition.getCode());
    }
    return conditionCodes;
  }

  public List<Condition> getConditionData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context, "Condition");
    List<Condition> conditions = new ArrayList<>();
    List<CodeableConcept> conditionCodes = new ArrayList<>();
    // Filter Conditions based on Encounter Reference
    if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Condition condition = (Condition) entry.getResource();
        if (!condition.getEncounter().isEmpty()) {
          if (condition
              .getEncounter()
              .getReferenceElement()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            conditions.add(condition);
            conditionCodes.addAll(findConditionCodes(condition));
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter conditions
    } else {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Condition condition = (Condition) entry.getResource();
        // Checking If Date Recorded present in Condition resource
        if (condition.getRecordedDate() != null) {
          if (condition.getRecordedDate().after(start) && condition.getRecordedDate().before(end)) {
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
    r4FhirData.setR4ConditionCodes(conditionCodes);
    return conditions;
  }

  private List<CodeableConcept> findLaboratoryCodes(Observation observation) {
    List<CodeableConcept> observationCodes = new ArrayList<>();

    if (!observation.getCode().isEmpty() && observation.getCode() != null) {
      observationCodes.add(observation.getCode());
    }
    return observationCodes;
  }

  public List<Observation> getObservationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getObservationByPatientId(
                launchDetails, client, context, OBSERVATION, "laboratory");
    List<Observation> observations = new ArrayList<>();
    List<CodeableConcept> observationCodes = new ArrayList<>();
    // Filter Observations based on Encounter Reference
    if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Observation observation = (Observation) entry.getResource();
        if (!observation.getEncounter().isEmpty()) {
          if (observation
              .getEncounter()
              .getReferenceElement()
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
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Observation observation = (Observation) entry.getResource();
        // Checking If Issued Date is present in Observation resource
        if (observation.getIssued() != null) {
          if (observation.getIssued().after(start) && observation.getIssued().before(end)) {
            observations.add(observation);
            observationCodes.addAll(findLaboratoryCodes(observation));
          }
          // If Issued date is not present, Checking for Effective Date
        } else if (observation.getEffective() != null && !observation.getEffective().isEmpty()) {
          Type effectiveDate = (Type) observation.getEffectiveDateTimeType();
          Date effDate = effectiveDate.dateTimeValue().getValue();
          if (effDate.after(start) && effDate.before(end)) {
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
    r4FhirData.setR4LabResultCodes(observationCodes);
    return observations;
  }

  public List<Observation> getPregnancyObservationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientIdAndCode(
                launchDetails,
                client,
                context,
                OBSERVATION,
                QueryConstants.PREGNANCY_CODE,
                QueryConstants.LOINC_CODE_SYSTEM);
    List<Observation> observations = new ArrayList<>();
    observations = filterObservation(bundle, encounter, start, end);

    return observations;
  }

  public List<Observation> getTravelObservationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientIdAndCode(
                launchDetails,
                client,
                context,
                OBSERVATION,
                QueryConstants.TRAVEL_CODE,
                QueryConstants.LOINC_CODE_SYSTEM);
    List<Observation> observations = new ArrayList<>();
    observations = filterObservation(bundle, encounter, start, end);

    for (String travelSnomedCode : QueryConstants.TRAVEL_HISTORY_SNOMED_CODES) {
      Bundle travelHisWithSNOMEDCodesbundle =
          (Bundle)
              resourceData.getResourceByPatientIdAndCode(
                  launchDetails,
                  client,
                  context,
                  OBSERVATION,
                  travelSnomedCode,
                  QueryConstants.SNOMED_CODE_SYSTEM);
      List<Observation> travelobs = new ArrayList<>();
      travelobs = filterObservation(travelHisWithSNOMEDCodesbundle, encounter, start, end);
      observations.addAll(travelobs);
    }

    return observations;
  }

  public List<Observation> getSocialHistoryObservationDataOccupation(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    List<Observation> observations = new ArrayList<>();
    for (String occupationCode : QueryConstants.OCCUPATION_SNOMED_CODES) {
      Bundle occupationCodesbundle =
          (Bundle)
              resourceData.getResourceByPatientIdAndCode(
                  launchDetails,
                  client,
                  context,
                  OBSERVATION,
                  occupationCode,
                  QueryConstants.SNOMED_CODE_SYSTEM);
      for (BundleEntryComponent entryComp : occupationCodesbundle.getEntry()) {
        observations.add((Observation) entryComp.getResource());
      }
    }
    return observations;
  }

  public List<Condition> getPregnancyConditions(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    List<Condition> conditions = new ArrayList<>();
    for (String pregnancySnomedCode : QueryConstants.PREGNANCY_SNOMED_CODES) {
      Bundle occupationCodesbundle =
          (Bundle)
              resourceData.getResourceByPatientIdAndCode(
                  launchDetails,
                  client,
                  context,
                  CONDITION,
                  pregnancySnomedCode,
                  QueryConstants.SNOMED_CODE_SYSTEM);
      for (BundleEntryComponent entryComp : occupationCodesbundle.getEntry()) {
        conditions.add((Condition) entryComp.getResource());
      }
    }
    return conditions;
  }

  private List<CodeableConcept> findMedicationCodes(MedicationAdministration medAdministration) {
    List<CodeableConcept> medicationCodes = new ArrayList<>();

    if (!medAdministration.getMedication().isEmpty() && medAdministration.getMedication() != null) {
      if (medAdministration.getMedication() instanceof CodeableConcept) {
        // Handle Codeable Concept
        CodeableConcept medicationCode = (CodeableConcept) medAdministration.getMedication();
        medicationCodes.add(medicationCode);
      } else {
        // Handle Reference data types
      }
    }
    return medicationCodes;
  }

  public Medication getMedicationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      String medicationId) {
    return (Medication)
        resourceData.getResouceById(launchDetails, client, context, "Medication", medicationId);
  }

  public List<MedicationAdministration> getMedicationAdministrationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(
                launchDetails, client, context, "MedicationAdministration");
    List<MedicationAdministration> medAdministrations = new ArrayList<>();
    List<CodeableConcept> medicationCodes = new ArrayList<>();
    // Filter MedicationAdministrations based on Encounter Reference
    if (bundle != null && encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        MedicationAdministration medAdministration = (MedicationAdministration) entry.getResource();
        if (!medAdministration.getContext().isEmpty()) {
          if (medAdministration
              .getContext()
              .getReferenceElement()
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
      for (BundleEntryComponent entry : bundle.getEntry()) {
        MedicationAdministration medAdministration = (MedicationAdministration) entry.getResource();
        // Checking If Effective Date is present in MedicationAdministration resource
        if (medAdministration.getEffectiveDateTimeType() != null) {
          Type effectiveDateTime = (Type) medAdministration.getEffectiveDateTimeType();
          Date effDate = effectiveDateTime.dateTimeValue().getValue();
          if (effDate.after(start) && effDate.before(end)) {
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
    r4FhirData.setR4MedicationCodes(medicationCodes);
    return medAdministrations;
  }

  private List<CodeableConcept> findMedicationRequestCodes(MedicationRequest medRequest) {
    List<CodeableConcept> medicationCodes = new ArrayList<CodeableConcept>();

    if (!medRequest.getMedication().isEmpty() && medRequest.getMedication() != null) {
      if (medRequest.getMedication() instanceof CodeableConcept) {
        // Handle Codeable Concept
        CodeableConcept medicationCode = (CodeableConcept) medRequest.getMedication();
        medicationCodes.add(medicationCode);
      } else {
        // Handle Reference data types
      }
    }
    return medicationCodes;
  }

  public List<MedicationRequest> getMedicationRequestData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {

    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(
                launchDetails, client, context, "MedicationRequest");
    List<MedicationRequest> medRequests = new ArrayList<>();
    List<CodeableConcept> medicationCodes = new ArrayList<CodeableConcept>();
    // Filter MedicationAdministrations based on Encounter Reference
    if (bundle != null && !encounter.getIdElement().getValue().isEmpty() && encounter != null) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        MedicationRequest medRequest = (MedicationRequest) entry.getResource();
        if (!medRequest.getEncounter().isEmpty()) {
          if (medRequest
              .getEncounter()
              .getReferenceElement()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            medRequests.add(medRequest);
            medicationCodes.addAll(findMedicationRequestCodes(medRequest));
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // MedicationAdministrations
    } else if (bundle != null) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        MedicationRequest medRequest = (MedicationRequest) entry.getResource();
        // Checking If Effective Date is present in MedicationAdministration resource
        if (medRequest.getAuthoredOn() != null) {
          Date effDate = medRequest.getAuthoredOn();
          if (effDate.after(start) && effDate.before(end)) {
            medRequests.add(medRequest);
            medicationCodes.addAll(findMedicationRequestCodes(medRequest));
          }
        }
        // If Effective Date is not present looking for LastUpdatedDate
        else {
          Date lastUpdatedDateTime = medRequest.getMeta().getLastUpdated();
          if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
            medRequests.add(medRequest);
            medicationCodes.addAll(findMedicationRequestCodes(medRequest));
          }
        }
      }
    }
    r4FhirData.setR4MedicationCodes(medicationCodes);
    return medRequests;
  }

  private List<CodeableConcept> findMedicationStatementCodes(MedicationStatement medStatement) {
    List<CodeableConcept> medicationCodes = new ArrayList<>();

    if (!medStatement.getMedication().isEmpty() && medStatement.getMedication() != null) {
      if (medStatement.getMedication() instanceof CodeableConcept) {
        // Handle Codeable Concept
        CodeableConcept medicationCode = (CodeableConcept) medStatement.getMedication();
        medicationCodes.add(medicationCode);
      } else {
        // Handle Reference data types
      }
    }
    return medicationCodes;
  }

  public List<MedicationStatement> getMedicationStatementData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(
                launchDetails, client, context, "MedicationStatement");
    List<MedicationStatement> medStatements = new ArrayList<>();
    List<CodeableConcept> medicationCodes = new ArrayList<>();
    // Filter MedicationAdministrations based on Encounter Reference
    if (bundle != null && encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        MedicationStatement medStatement = (MedicationStatement) entry.getResource();
        if (!medStatement.getContext().isEmpty()) {
          if (medStatement
              .getContext()
              .getReferenceElement()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            medStatements.add(medStatement);
            medicationCodes.addAll(findMedicationStatementCodes(medStatement));
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // MedicationAdministrations
    } else if (bundle != null) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        MedicationStatement medStatement = (MedicationStatement) entry.getResource();
        // Checking If Effective Date is present in MedicationAdministration resource
        if (medStatement.getEffectiveDateTimeType() != null) {
          Type effectiveDateTime = (Type) medStatement.getEffectiveDateTimeType();
          Date effDate = effectiveDateTime.dateTimeValue().getValue();
          if (effDate.after(start) && effDate.before(end)) {
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
    r4FhirData.setR4MedicationCodes(medicationCodes);
    return medStatements;
  }

  private List<CodeableConcept> findDiagnosticReportCodes(DiagnosticReport diagnosticReport) {
    List<CodeableConcept> diagnosticReportCodes = new ArrayList<>();

    if (!diagnosticReport.getCode().isEmpty() && diagnosticReport.getCode() != null) {
      diagnosticReportCodes.add(diagnosticReport.getCode());
    }
    return diagnosticReportCodes;
  }

  public List<DiagnosticReport> getDiagnosticReportData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(launchDetails, client, context, "DiagnosticReport");
    List<DiagnosticReport> diagnosticReports = new ArrayList<>();
    List<CodeableConcept> diagnosticReportCodes = new ArrayList<>();
    // Filter DiagnosticReports based on Encounter Reference
    if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        DiagnosticReport diagnosticReport = (DiagnosticReport) entry.getResource();
        if (!diagnosticReport.getEncounter().isEmpty()) {
          if (diagnosticReport
              .getEncounter()
              .getReferenceElement()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            diagnosticReports.add(diagnosticReport);
            diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // DiagnosticOrders
    } else {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        DiagnosticReport diagnosticReport = (DiagnosticReport) entry.getResource();
        // Checking If Issued Date is present in Observation resource
        if (diagnosticReport.getIssued() != null) {
          if (diagnosticReport.getIssued().after(start)
              && diagnosticReport.getIssued().before(end)) {
            diagnosticReports.add(diagnosticReport);
            diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
          }
          // If Issued date is not present, Checking for Effective Date
        } else if (!diagnosticReport.getEffective().isEmpty()) {
          Type effectiveDate = (Type) diagnosticReport.getEffective();
          Date effDate = effectiveDate.dateTimeValue().getValue();
          if (effDate.after(start) && effDate.before(end)) {
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
    r4FhirData.setR4DiagnosticReportCodes(diagnosticReportCodes);
    return diagnosticReports;
  }

  private List<CodeableConcept> findImmunizationCodes(Immunization immunization) {

    List<CodeableConcept> immunizationCodes = new ArrayList<>();
    if (!immunization.getVaccineCode().isEmpty() && immunization.getVaccineCode() != null) {
      immunizationCodes.add(immunization.getVaccineCode());
    }

    return immunizationCodes;
  }

  public List<Immunization> getImmunizationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(launchDetails, client, context, "Immunization");
    List<Immunization> immunizations = new ArrayList<>();
    List<CodeableConcept> immunizationCodes = new ArrayList<>();
    // Filter Immunizations based on Encounter Reference
    if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Immunization immunization = (Immunization) entry.getResource();
        if (!immunization.getEncounter().isEmpty()) {
          if (immunization
              .getEncounter()
              .getReferenceElement()
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
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Immunization immunization = (Immunization) entry.getResource();
        // Checking If Immunization DateTime is present in Immunization
        // resource
        if (immunization.getOccurrence().isDateTime()) {
          if (immunization.getOccurrenceDateTimeType() != null) {
            if (immunization.getOccurrenceDateTimeType().dateTimeValue().getValue().after(start)
                && immunization
                    .getOccurrenceDateTimeType()
                    .dateTimeValue()
                    .getValue()
                    .before(end)) {
              immunizations.add(immunization);
              immunizationCodes.addAll(findImmunizationCodes(immunization));
            }
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
    r4FhirData.setR4ImmunizationCodes(immunizationCodes);
    return immunizations;
  }

  private List<CodeableConcept> findServiceRequestCodes(ServiceRequest serviceRequest) {
    List<CodeableConcept> serviceRequestCodes = new ArrayList<>();
    if (!serviceRequest.getCode().isEmpty() && serviceRequest.getCode() != null) {
      serviceRequestCodes.add(serviceRequest.getCode());
    }
    return serviceRequestCodes;
  }

  public List<ServiceRequest> getServiceRequestData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(launchDetails, client, context, "ServiceRequest");
    List<ServiceRequest> serviceRequests = new ArrayList<>();
    List<CodeableConcept> serviceRequestCodes = new ArrayList<>();
    // Filter ServiceRequests based on Encounter Reference
    if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();
        if (!serviceRequest.getEncounter().isEmpty()) {
          if (serviceRequest
              .getEncounter()
              .getReferenceElement()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            serviceRequests.add(serviceRequest);
            serviceRequestCodes.addAll(findServiceRequestCodes(serviceRequest));
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // ServiceRequests
    } else {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();
        // Checking If ServiceRequest DateTime is present in ServiceRequest
        // resource
        if (serviceRequest.getOccurrence().isDateTime()) {
          if (serviceRequest.getOccurrenceDateTimeType() != null) {
            if (serviceRequest.getOccurrenceDateTimeType().dateTimeValue().getValue().after(start)
                && serviceRequest
                    .getOccurrenceDateTimeType()
                    .dateTimeValue()
                    .getValue()
                    .before(end)) {
              serviceRequests.add(serviceRequest);
              serviceRequestCodes.addAll(findServiceRequestCodes(serviceRequest));
            }
          }
        }
        // If ServiceRequest Date is not present looking for LastUpdatedDate
        else {
          Date lastUpdatedDateTime = serviceRequest.getMeta().getLastUpdated();
          if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
            serviceRequests.add(serviceRequest);
            serviceRequestCodes.addAll(findServiceRequestCodes(serviceRequest));
          }
        }
      }
    }
    r4FhirData.setR4ServiceRequestCodes(serviceRequestCodes);
    return serviceRequests;
  }

  public static List<Observation> filterObservation(
      Bundle bundle, Encounter encounter, Date start, Date end) {

    List<Observation> observations = new ArrayList<>();
    // Filter Observations based on Encounter Reference
    if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Observation observation = (Observation) entry.getResource();
        if (!observation.getEncounter().isEmpty()) {
          if (observation
              .getEncounter()
              .getReferenceElement()
              .getIdPart()
              .equals(encounter.getIdElement().getIdPart())) {
            observations.add(observation);
          }
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // Observations
    } else {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Observation observation = (Observation) entry.getResource();
        // Checking If Issued Date is present in Observation resource
        if (observation.getIssued() != null) {
          if (observation.getIssued().after(start) && observation.getIssued().before(end)) {
            observations.add(observation);
          }
          // If Issued date is not present, Checking for Effective Date
        } else if (observation.getEffective() != null && !observation.getEffective().isEmpty()) {
          Type effectiveDate = (Type) observation.getEffectiveDateTimeType();
          Date effDate = effectiveDate.dateTimeValue().getValue();
          if (effDate.after(start) && effDate.before(end)) {
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

  public Bundle getCommonResources(
      R4FhirData r4FhirData,
      Date start,
      Date end,
      LaunchDetails launchDetails,
      IGenericClient client,
      FhirContext context) {

    Bundle bundle = new Bundle();
    // GET Patient Details and Add to Bundle
    try {
      logger.info("Get Patient Data");
      Patient patient =
          (Patient)
              fhirContextInitializer.getResouceById(
                  launchDetails, client, context, "Patient", launchDetails.getLaunchPatientId());
      r4FhirData.setPatient(patient);
      BundleEntryComponent patientEntry = new BundleEntryComponent();
      patientEntry.setResource(patient);
      bundle.addEntry(patientEntry);
    } catch (Exception e) {
      logger.error("Error in getting Patient Data");
    }
    // Step 1: Get Encounters for Patient based on encId. (Create a method to get
    // encounters)
    // If encId is null, find encounters for patient within the start and end time
    // provided.
    // Add to the bundle.
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of encounterCodes.
    Encounter encounter = null;
    try {
      logger.info("Get Encounter Data");
      encounter = getEncounterData(context, client, launchDetails, r4FhirData, start, end);
      r4FhirData.setEncounter(encounter);
      if (encounter.getParticipant() != null) {
        List<Practitioner> practitionerList = new ArrayList<Practitioner>();
        List<EncounterParticipantComponent> participants = encounter.getParticipant();
        for (EncounterParticipantComponent participant : participants) {
          if (participant.getIndividual() != null) {
            Reference practitionerReference = participant.getIndividual();
            Practitioner practitioner =
                (Practitioner)
                    fhirContextInitializer.getResouceById(
                        launchDetails,
                        client,
                        context,
                        "Practitioner",
                        practitionerReference.getReferenceElement().getIdPart());
            practitionerList.add(practitioner);
            if (practitioner != null) {
              BundleEntryComponent practitionerEntry =
                  new BundleEntryComponent().setResource(practitioner);
              bundle.addEntry(practitionerEntry);
            }
          }
        }
        r4FhirData.setPractitionersList(practitionerList);
      }
      if (encounter.getServiceProvider() != null) {
        Reference organizationReference = encounter.getServiceProvider();
        Organization organization =
            (Organization)
                fhirContextInitializer.getResouceById(
                    launchDetails,
                    client,
                    context,
                    "Organization",
                    organizationReference.getReferenceElement().getIdPart());
        BundleEntryComponent organizationEntry =
            new BundleEntryComponent().setResource(organization);
        bundle.addEntry(organizationEntry);
        r4FhirData.setOrganization(organization);
      }
      if (encounter.getLocation() != null) {
        List<Location> locationList = new ArrayList<Location>();
        List<EncounterLocationComponent> enocunterLocations = encounter.getLocation();
        for (EncounterLocationComponent location : enocunterLocations) {
          if (location.getLocation() != null) {
            Reference locationReference = location.getLocation();
            Location locationResource =
                (Location)
                    fhirContextInitializer.getResouceById(
                        launchDetails,
                        client,
                        context,
                        "Location",
                        locationReference.getReferenceElement().getIdPart());
            locationList.add(locationResource);
            BundleEntryComponent locationEntry =
                new BundleEntryComponent().setResource(locationResource);
            bundle.addEntry(locationEntry);
          }
        }
        r4FhirData.setLocationList(locationList);
      }
      BundleEntryComponent encounterEntry = new BundleEntryComponent().setResource(encounter);
      bundle.addEntry(encounterEntry);
    } catch (Exception e) {
      logger.error("Error in getting Encounter Data");
    }

    // Step 2: Get Conditions for Patient (Write a method)
    // Filter the conditions based on encounter Reference if Encounter Reference is
    // present.
    // If encounter is not present, then filter based on times (Start and end, if
    // Condition time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of ConditionCodes.
    try {
      logger.info("Get Condition Data");
      List<Condition> conditionsList =
          getConditionData(context, client, launchDetails, r4FhirData, encounter, start, end);
      if (logger.isInfoEnabled()) {
        logger.info("Filtered ConditionsList----> {}", conditionsList.size());
      }
      r4FhirData.setConditions(conditionsList);
      for (Condition condition : conditionsList) {
        BundleEntryComponent conditionsEntry = new BundleEntryComponent().setResource(condition);
        bundle.addEntry(conditionsEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Condition Data");
    }

    // Get Observations for Patients and laboratory category (Write a method).
    // Filter the observations based on encounter Reference if encounter is present.
    // If encounter is not present, then filter based on times (Start and end, if
    // observation time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of labResultCodes.
    try {
      logger.info("Get Observation Data");
      List<Observation> observationList =
          getObservationData(context, client, launchDetails, r4FhirData, encounter, start, end);
      if (logger.isInfoEnabled()) {
        logger.info("Filtered Observations----> {}", observationList.size());
      }
      r4FhirData.setLabResults(observationList);
      for (Observation observation : observationList) {
        BundleEntryComponent observationsEntry =
            new BundleEntryComponent().setResource(observation);
        bundle.addEntry(observationsEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Observation Data");
    }

    // Get MedicationAdministration for Patients and laboratory category (Write a
    // method).
    // Filter the MedicationAdministrations based on encounter Reference if
    // encounter is present.
    // If encounter is not present, then filter based on times (Start and end, if
    // medicationadministration time is between start and end times) -- Do this
    // later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of medicationCodes.
    try {
      logger.info("Get MedicationAdministration Data");
      List<MedicationAdministration> medAdministrationsList =
          getMedicationAdministrationData(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      if (logger.isInfoEnabled()) {
        logger.info(
            "Filtered MedicationAdministration-----------> {}", medAdministrationsList.size());
      }
      r4FhirData.setMedicationAdministrations(medAdministrationsList);
      for (MedicationAdministration medAdministration : medAdministrationsList) {
        if (!medAdministration.getMedication().isEmpty()
            && medAdministration.getMedication() != null) {
          if (medAdministration.getMedication() instanceof Reference) {
            Reference medRef = (Reference) medAdministration.getMedication();
            String medReference = medRef.getReferenceElement().getValue();
            if (medReference.startsWith("#")) {
              List<Resource> medAdministrationContained = medAdministration.getContained();
              // List<Medication> containedResources = medAdministrationContained;
              if (medAdministrationContained
                  .stream()
                  .anyMatch(resource -> resource.getIdElement().getValue().equals(medReference))) {
                logger.info(
                    "Medication Resource exists in MedicationAdministration.contained. So no need to add again in Bundle.");
              }
            } else {
              logger.info("Medication Reference Found=============>");
              Medication medication =
                  getMedicationData(context, client, launchDetails, r4FhirData, medReference);
              BundleEntryComponent medicationEntry =
                  new BundleEntryComponent().setResource(medication);
              bundle.addEntry(medicationEntry);
              if (medication != null) {
                List<Medication> medicationList = new ArrayList<>();
                medicationList.add(medication);
                r4FhirData.setMedicationList(medicationList);
              }
            }
          }
        }
        BundleEntryComponent medAdministrationEntry =
            new BundleEntryComponent().setResource(medAdministration);
        bundle.addEntry(medAdministrationEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting the MedicationAdministration Data", e);
    }

    try {
      logger.info("Get MedicationRequest Data");
      List<MedicationRequest> medRequestsList =
          getMedicationRequestData(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      logger.info("Filtered MedicationRequests----------->" + medRequestsList.size());
      r4FhirData.setMedicationRequests(medRequestsList);
      for (MedicationRequest medRequest : medRequestsList) {
        if (!medRequest.getMedication().isEmpty() && medRequest.getMedication() != null) {
          if (medRequest.getMedication() instanceof Reference) {
            Reference medRef = (Reference) medRequest.getMedication();
            String medReference = medRef.getReferenceElement().getValue();
            if (medReference.startsWith("#")) {
              List<Resource> medRequestContained = medRequest.getContained();
              // List<Medication> containedResources = medAdministrationContained;
              if (medRequestContained
                  .stream()
                  .anyMatch(resource -> resource.getIdElement().getValue().equals(medReference))) {
                logger.info(
                    "Medication Resource exists in MedicationRequest.contained. So no need to add again in Bundle.");
              }
            } else {
              logger.info("Medication Reference Found=============>");
              Medication medication =
                  getMedicationData(context, client, launchDetails, r4FhirData, medReference);
              BundleEntryComponent medicationEntry =
                  new BundleEntryComponent().setResource(medication);
              bundle.addEntry(medicationEntry);
              if (medication != null) {
                List<Medication> medicationList = new ArrayList<Medication>();
                medicationList.add(medication);
                r4FhirData.setMedicationList(medicationList);
              }
            }
          }
        }
        BundleEntryComponent medRequestEntry = new BundleEntryComponent().setResource(medRequest);
        bundle.addEntry(medRequestEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting the MedicationRequest Data", e);
    }

    // Get ServiceRequest for Patients (Write a method).
    // Filter the ServiceRequest based on encounter Reference if encounter is
    // present.
    // If encounter is not present, then filter based on times (Start and end, if
    // ServiceRequest time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of ServiceRequestCodes.

    try {
      logger.info("Get ServiceRequest Data");
      List<ServiceRequest> serviceRequestsList =
          getServiceRequestData(context, client, launchDetails, r4FhirData, encounter, start, end);
      if (logger.isInfoEnabled()) {
        logger.info("Filtered ServiceRequests-----------> {}", serviceRequestsList.size());
      }
      r4FhirData.setServiceRequests(serviceRequestsList);
      for (ServiceRequest serviceRequest : serviceRequestsList) {
        BundleEntryComponent serviceRequestEntry =
            new BundleEntryComponent().setResource(serviceRequest);
        bundle.addEntry(serviceRequestEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting the ServiceRequest Data");
    }
    return bundle;
  }

  public Resource getResourceFromBundle(Bundle bundle, Class<?> resource) {
    try {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        if (entry.getResource() != null) {
          if (entry.getResource().getClass() == resource) {
            return entry.getResource();
          }
        }
      }
    } catch (Exception e) {
      logger.error("Error in getting the Resource from Bundle");
    }
    return null;
  }
}
