package com.drajer.sof.utils;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.ResourceNotFoundException;
import com.drajer.cda.parser.CdaParserConstants;
import com.drajer.cdafromr4.CdaFhirUtilities;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Encounter.EncounterLocationComponent;
import org.hl7.fhir.r4.model.Encounter.EncounterParticipantComponent;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.logging.LogLevel;
import org.springframework.stereotype.Component;

@Component
public class R4ResourcesData {

  private final FhirContextInitializer fhirContextInitializer;
  private final Logger logger = LoggerFactory.getLogger(R4ResourcesData.class);

  @Autowired
  public R4ResourcesData(FhirContextInitializer fhirContextInitializer) {
    this.fhirContextInitializer = fhirContextInitializer;
  }

  /** Default constructor for backward compatibility with direct instantiation. */
  public R4ResourcesData() {
    this(null);
  }

  private static final String OBSERVATION = "Observation";
  private static final String CONDITION = "Condition";
  private static final String ENCOUNTER = "Encounter";
  private static final String OBSERVATION_SOCIAL_HISTORY = "social-history";
  private static final String ENTERED_IN_ERROR = "entered-in-error";

  private static final String ENCOUNTER_DIAGNOSIS_CONDITION = "encounter-diagnosis";
  private static final String PROBLEM_LIST_CONDITION = "problem-list-item";

  private static final String ATTACHMENT_CONTENT_TYPE = "text/xml";

  // ==================== REUSABLE HELPERS (Reduces complexity across 10 methods)
  // ====================

  /**
   * Filters resources by encounter reference. Used in: getEncounterData, getConditionData,
   * getObservationData, getMedicationAdministrationData, getMedicationRequestData,
   * getMedicationStatementData, getDiagnosticReportData
   */
  private <T extends Resource> List<T> filterByEncounterId(
      Bundle bundle, String encounterId, Class<T> resourceClass) {
    List<T> filtered = new ArrayList<>();
    if (bundle == null || bundle.getEntry() == null || encounterId == null) {
      return filtered;
    }

    for (BundleEntryComponent entry : bundle.getEntry()) {
      T resource = resourceClass.cast(entry.getResource());
      if (hasMatchingEncounterId(resource, encounterId)) {
        filtered.add(resource);
      }
    }
    return filtered;
  }

  /** Checks if resource has matching encounter reference. */
  private boolean hasMatchingEncounterId(Resource resource, String encounterId) {
    if (resource instanceof Observation) {
      Observation obs = (Observation) resource;
      return !obs.getEncounter().isEmpty()
          && obs.getEncounter().getReferenceElement().getIdPart().equals(encounterId);
    } else if (resource instanceof MedicationAdministration) {
      MedicationAdministration med = (MedicationAdministration) resource;
      return !med.getContext().isEmpty()
          && med.getContext().getReferenceElement().getIdPart().equals(encounterId);
    } else if (resource instanceof MedicationRequest) {
      MedicationRequest med = (MedicationRequest) resource;
      return !med.getEncounter().isEmpty()
          && med.getEncounter().getReferenceElement().getIdPart().equals(encounterId);
    } else if (resource instanceof MedicationStatement) {
      MedicationStatement med = (MedicationStatement) resource;
      return !med.getContext().isEmpty()
          && med.getContext().getReferenceElement().getIdPart().equals(encounterId);
    } else if (resource instanceof DiagnosticReport) {
      DiagnosticReport diag = (DiagnosticReport) resource;
      return !diag.getEncounter().isEmpty()
          && diag.getEncounter().getReferenceElement().getIdPart().equals(encounterId);
    } else if (resource instanceof ServiceRequest) {
      ServiceRequest sr = (ServiceRequest) resource;
      return !sr.getEncounter().isEmpty()
          && sr.getEncounter().getReferenceElement().getIdPart().equals(encounterId);
    }
    return false;
  }

  /**
   * Extracts date from resource with 3-way fallback: specific field → Effective → LastUpdated. Used
   * in: getObservationData, getConditionData, getDiagnosticReportData, etc.
   */
  private Date extractResourceDate(Resource resource) {
    if (resource instanceof Encounter) {
      return extractEncounterDate((Encounter) resource);
    } else if (resource instanceof Observation) {
      return extractObservationDate((Observation) resource);
    } else if (resource instanceof MedicationAdministration) {
      return extractMedicationAdministrationDate((MedicationAdministration) resource);
    } else if (resource instanceof MedicationRequest) {
      return extractMedicationRequestDate((MedicationRequest) resource);
    } else if (resource instanceof MedicationStatement) {
      return extractMedicationStatementDate((MedicationStatement) resource);
    } else if (resource instanceof DiagnosticReport) {
      return extractDiagnosticReportDate((DiagnosticReport) resource);
    } else if (resource instanceof ServiceRequest) {
      return extractServiceRequestDate((ServiceRequest) resource);
    }
    return resource.getMeta().getLastUpdated();
  }

  private Date extractEncounterDate(Encounter enc) {
    if (enc.hasPeriod() && enc.getPeriod().hasStart()) {
      return enc.getPeriod().getStart();
    }
    return enc.getMeta().getLastUpdated();
  }

  private Date extractObservationDate(Observation obs) {
    if (obs.getIssued() != null) {
      return obs.getIssued();
    }
    if (obs.getEffective() != null && !obs.getEffective().isEmpty()) {
      try {
        return obs.getEffectiveDateTimeType().dateTimeValue().getValue();
      } catch (Exception e) {
        logger.debug("Could not extract effective date from Observation");
      }
    }
    return obs.getMeta().getLastUpdated();
  }

  private Date extractMedicationAdministrationDate(MedicationAdministration med) {
    if (med.getEffective() != null) {
      Pair<Date, TimeZone> effDate = CdaFhirUtilities.getActualDate(med.getEffective());
      if (effDate != null) {
        return effDate.getValue0();
      }
    }
    return med.getMeta().getLastUpdated();
  }

  private Date extractMedicationRequestDate(MedicationRequest med) {
    if (med.getAuthoredOn() != null) {
      return med.getAuthoredOn();
    }
    return med.getMeta().getLastUpdated();
  }

  private Date extractMedicationStatementDate(MedicationStatement med) {
    if (med.hasEffectiveDateTimeType()) {
      try {
        return med.getEffectiveDateTimeType().dateTimeValue().getValue();
      } catch (Exception e) {
        logger.debug("Could not extract effective date from MedicationStatement");
      }
    }
    return med.getMeta().getLastUpdated();
  }

  private Date extractDiagnosticReportDate(DiagnosticReport diag) {
    if (diag.getIssued() != null) {
      return diag.getIssued();
    }
    if (!diag.getEffective().isEmpty()) {
      try {
        return diag.getEffective().dateTimeValue().getValue();
      } catch (Exception e) {
        logger.debug("Could not extract effective date from DiagnosticReport");
      }
    }
    return diag.getMeta().getLastUpdated();
  }

  private Date extractServiceRequestDate(ServiceRequest sr) {
    if (sr.getOccurrence() != null && sr.getOccurrence().isDateTime()) {
      try {
        return sr.getOccurrenceDateTimeType().dateTimeValue().getValue();
      } catch (Exception e) {
        logger.debug("Could not extract occurrence date from ServiceRequest");
      }
    }
    return sr.getMeta().getLastUpdated();
  }

  /** Filters resources by date range using extracted date. */
  private <T extends Resource> List<T> filterByDateRange(
      Bundle bundle, Date start, Date end, Class<T> resourceClass) {
    List<T> filtered = new ArrayList<>();
    if (bundle == null || bundle.getEntry() == null) {
      return filtered;
    }

    for (BundleEntryComponent entry : bundle.getEntry()) {
      T resource = resourceClass.cast(entry.getResource());
      Date resourceDate = extractResourceDate(resource);
      if (isResourceWithinDateTime(start, end, resourceDate)) {
        filtered.add(resource);
      }
    }
    return filtered;
  }

  // ==================== END REUSABLE HELPERS ====================

  private List<CodeableConcept> findEncounterCodes(Encounter encounter) {
    List<CodeableConcept> encounterCodes = new ArrayList<>();
    if (encounter != null && encounter.getType() != null) {
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
    Encounter encounter = null;
    String encounterID = launchDetails.getEncounterId();

    if (fhirContextInitializer.checkSkipResource(ENCOUNTER, (FhirClient) client)) {
      return encounter;
    }
    // If Encounter Id is present in Launch Details
    if (encounterID != null) {
      try {
        logger.info("Getting Encounter data by ID {}", StringEscapeUtils.escapeJava(encounterID));
        encounter = (Encounter) client.read().resource(ENCOUNTER).withId(encounterID).execute();
      } catch (ResourceNotFoundException resourceNotFoundException) {
        logger.error(
            "Error in getting Encounter resource by Id: {}",
            StringEscapeUtils.escapeJava(encounterID),
            resourceNotFoundException);
        WorkflowService.cancelAllScheduledTasksForLaunch(launchDetails, true);
      } catch (Exception e) {
        logger.error(
            "Error in getting Encounter resource by Id: {}",
            StringEscapeUtils.escapeJava(encounterID),
            e);
      }
      if (encounter != null) {
        r4FhirData.setR4EncounterCodes(findEncounterCodes(encounter));
      }

    } else {
      Bundle bundle =
          (Bundle)
              fhirContextInitializer.getResourceByPatientId(
                  launchDetails, client, context, ENCOUNTER);
      List<Encounter> encounters = filterByDateRange(bundle, start, end, Encounter.class);
      if (!encounters.isEmpty()) {
        encounter =
            encounters.stream()
                .max(
                    (e1, e2) ->
                        e1.getMeta().getLastUpdated().compareTo(e2.getMeta().getLastUpdated()))
                .orElse(null);
        if (encounter != null) {
          r4FhirData.setR4EncounterCodes(findEncounterCodes(encounter));
        }
      }
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

  public Boolean isConditionActive(Condition condition) {

    Boolean retVal = true;
    if (condition.hasAbatement() && condition.getAbatement() instanceof DateTimeType) {

      DateTimeType d = (DateTimeType) condition.getAbatement();

      DateTimeType current = new DateTimeType();
      current.setValue(new Date(System.currentTimeMillis()));

      if (d.before(current)) {
        retVal = false;
      }
    }

    return retVal;
  }

  public List<Condition> getConditionData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    logger.info(
        "Encounter :{} StartDate :{} and EndDate :{} in getConditionData ", encounter, start, end);

    logger.trace("Getting Conditions Data");
    Bundle bundle =
        (Bundle)
            fhirContextInitializer.getResourceByPatientId(
                launchDetails, client, context, CONDITION);
    List<Condition> allConditions = new ArrayList<>();
    List<Condition> problemConditions = new ArrayList<>();
    List<Condition> encounterDiagnosisConditions = new ArrayList<>();
    List<CodeableConcept> conditionCodes = new ArrayList<>();
    int[] counters = {0, 0}; // [inError, missingAbatement]

    if (bundle != null && bundle.getEntry() != null) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Condition condition = (Condition) entry.getResource();
        processConditionEntry(
            condition,
            launchDetails,
            problemConditions,
            encounterDiagnosisConditions,
            conditionCodes,
            counters);
      }

      allConditions.addAll(problemConditions);
      allConditions.addAll(encounterDiagnosisConditions);
      r4FhirData.setConditions(problemConditions);
      r4FhirData.setEncounterDiagnosisConditions(encounterDiagnosisConditions);
      r4FhirData.setR4ConditionCodes(conditionCodes);
      logger.info(
          "Total Conditions:{} Filtered Problem:{} Filtered Encounter Diagnosis:{} Entered InError:{} Missing Abatement:{}",
          bundle.getEntry().size(),
          problemConditions.size(),
          encounterDiagnosisConditions.size(),
          counters[0],
          counters[1]);
    }
    logger.info("Filtered ConditionsList ----> {}", allConditions.size());
    return allConditions;
  }

  private void processConditionEntry(
      Condition condition,
      LaunchDetails launchDetails,
      List<Condition> problemConditions,
      List<Condition> encounterDiagnosisConditions,
      List<CodeableConcept> conditionCodes,
      int[] counters) {
    if (isVerificationStatusInError(condition)) {
      counters[0]++;
      return;
    }

    if (!isConditionActive(condition) || !condition.hasCategory()) {
      counters[1]++;
      return;
    }

    boolean foundPregnancyCondition =
        condition.getCode().getCoding().stream()
            .anyMatch(
                coding ->
                    Arrays.stream(QueryConstants.getPregnancySmtCodes())
                        .anyMatch(coding.getCode()::equals));

    for (CodeableConcept categoryCodeableConcept : condition.getCategory()) {
      for (Coding categoryCoding : categoryCodeableConcept.getCoding()) {
        processCategoryCode(
            categoryCoding,
            condition,
            launchDetails,
            foundPregnancyCondition,
            problemConditions,
            encounterDiagnosisConditions,
            conditionCodes);
      }
    }
  }

  private boolean isVerificationStatusInError(Condition condition) {
    return isVerificationStatusPresent(condition)
        && condition.getVerificationStatus().getCodingFirstRep().getCode().equals(ENTERED_IN_ERROR);
  }

  private void processCategoryCode(
      Coding categoryCoding,
      Condition condition,
      LaunchDetails launchDetails,
      boolean foundPregnancyCondition,
      List<Condition> problemConditions,
      List<Condition> encounterDiagnosisConditions,
      List<CodeableConcept> conditionCodes) {
    if (categoryCoding.getCode().equals(PROBLEM_LIST_CONDITION) && !foundPregnancyCondition) {
      logger.debug("Added condition to problem list {}", condition.getId());
      problemConditions.add(condition);
      conditionCodes.addAll(findConditionCodes(condition));
    } else if (categoryCoding.getCode().equals(ENCOUNTER_DIAGNOSIS_CONDITION)
        && !foundPregnancyCondition) {
      if (condition.hasEncounter()
          && condition
              .getEncounter()
              .getReference()
              .equals("Encounter/" + launchDetails.getEncounterId())) {
        logger.info(ATTACHMENT_CONTENT_TYPE);
        logger.debug("Added condition to Encounter Diagnosis list {}", condition.getId());
        encounterDiagnosisConditions.add(condition);
        conditionCodes.addAll(findConditionCodes(condition));
      }
    }
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
      Date start,
      Date end) {
    logger.trace("Get Observation Data");
    Bundle bundle =
        (Bundle)
            fhirContextInitializer.getObservationByPatientId(
                launchDetails, client, context, OBSERVATION, "laboratory");
    List<Observation> observations = new ArrayList<>();
    List<Observation> valueObservations = new ArrayList<>();
    List<CodeableConcept> observationCodes = new ArrayList<>();
    List<CodeableConcept> valueObservationCodes = new ArrayList<>();

    if (bundle != null && bundle.getEntry() != null) {
      bundle = filterObservationByStatus(bundle, ENTERED_IN_ERROR);
      String encounterId = launchDetails.getEncounterId();

      if (StringUtils.isNotEmpty(encounterId)) {
        observations = filterByEncounterId(bundle, encounterId, Observation.class);
      } else {
        observations = filterByDateRange(bundle, start, end, Observation.class);
      }

      for (Observation obs : observations) {
        observationCodes.addAll(findLaboratoryCodes(obs));
        findAllValueCodes(obs, valueObservations, valueObservationCodes);
      }
    }

    r4FhirData.setR4LabResultCodes(observationCodes);
    r4FhirData.setR4LabResultValues(valueObservationCodes);
    r4FhirData.setLabResultValueObservations(valueObservations);

    logger.info("Filtered Observations ----> {}", observations.size());
    logger.info("Filtered Observation Coded Values ----> {}", valueObservations.size());
    return observations;
  }

  public boolean observationHasSameEncounter(Encounter enc, Observation obs) {

    if (enc != null
        && obs.getEncounter() != null
        && obs.getEncounter().getReferenceElement() != null
        && obs.getEncounter().getReferenceElement().getIdPart() != null
        && enc.getIdElement()
            .getIdPart()
            .contentEquals(obs.getEncounter().getReferenceElement().getIdPart())) {

      logger.debug(" Filtering based on Encounter Reference {}", enc.getId());
      return true;

    } else return false;
  }

  public boolean isObservationWithinTimeRange(Date start, Date end, Observation obs) {

    if (obs.getIssued() != null && isResourceWithinDateTime(start, end, obs.getIssued())) {

      logger.debug(" Adding observation based on time thresholds compared to Issued Time ");
      return true;
    }

    if (obs.getEffective() != null && !obs.getEffective().isEmpty()) {
      Type effectiveDate = obs.getEffectiveDateTimeType();
      Date effDate = effectiveDate.dateTimeValue().getValue();
      if (isResourceWithinDateTime(start, end, effDate)) {

        logger.debug(" Adding observation based on time thresholds compared to Effective Time ");
        return true;
      }
    }

    Date lastUpdatedDateTime = obs.getMeta().getLastUpdated();
    if (isResourceWithinDateTime(start, end, lastUpdatedDateTime)) {
      logger.debug(" Adding observation based on time thresholds compared to Last Updated Time ");
      return true;
    }

    logger.debug(
        " Observation {} not being added as it is not within the time range ", obs.getId());
    return false;
  }

  public void findAllValueCodes(
      Observation obs,
      List<Observation> valueObservations,
      List<CodeableConcept> valueObservationCodes) {

    if (obs.getValue() instanceof CodeableConcept) {
      CodeableConcept cd = obs.getValueCodeableConcept();
      valueObservationCodes.add(cd);
      valueObservations.add(obs);
    }
  }

  public List<Observation> getPregnancyObservationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    logger.info(
        "R4FhirData :{} Encounter :{} StartDate :{} and EndDate :{} in getPregnancyObservationData ",
        r4FhirData,
        encounter,
        start,
        end);
    logger.trace("Get Pregnancy Observation Data");
    Bundle bundle =
        (Bundle)
            fhirContextInitializer.getResourceByPatientIdAndCode(
                launchDetails,
                client,
                context,
                OBSERVATION,
                QueryConstants.PREGNANCY_CODE,
                QueryConstants.LOINC_CODE_SYSTEM);
    List<Observation> observations = new ArrayList<>();
    if (bundle != null) {
      bundle = filterObservationByStatus(bundle, ENTERED_IN_ERROR);
      observations = filterObservation(bundle, encounter, start, end);
      logger.info("Filtered Pregnancy Observations ----> {}", observations.size());
    }
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
    logger.info(
        "R4FhirData :{} Encounter :{} StartDate :{} and EndDate :{} in getTravelObservationData. ",
        r4FhirData,
        encounter,
        start,
        end);
    logger.trace("Get Travel Observation Data");

    StringBuilder codeBuilder = new StringBuilder(2000);
    for (String travelSnomedCode : QueryConstants.getTravelHistorySmtCodes()) {
      codeBuilder.append(QueryConstants.SNOMED_CODE_SYSTEM + "|" + travelSnomedCode + ",");
    }
    codeBuilder.append(QueryConstants.LOINC_CODE_SYSTEM + "|" + QueryConstants.TRAVEL_CODE);
    codeBuilder.trimToSize();
    String codes = codeBuilder.toString();

    String url =
        launchDetails.getEhrServerURL()
            + "/"
            + OBSERVATION
            + "?patient="
            + launchDetails.getLaunchPatientId()
            + "&code="
            + codes;

    Bundle travelCodeBundle =
        (Bundle)
            FhirContextInitializer.getResourceBundleByUrl(
                launchDetails, client, context, OBSERVATION, url);

    List<Observation> observations = new ArrayList<>();
    if (travelCodeBundle != null) {
      travelCodeBundle = filterObservationByStatus(travelCodeBundle, ENTERED_IN_ERROR);
      travelCodeBundle =
          filterObservationsBundleByCategory(travelCodeBundle, OBSERVATION_SOCIAL_HISTORY);
      observations = filterObservation(travelCodeBundle, encounter, start, end);
    }

    logger.info("Filtered Travel Observations ----> {}", observations.size());
    return observations;
  }

  private Bundle filterObservationByStatus(Bundle bundle, String observationStatus) {
    Bundle filteredBundle = new Bundle();
    int observationInError = 0;
    List<BundleEntryComponent> filteredEntryComponents = new ArrayList<>();
    for (BundleEntryComponent entryComp : bundle.getEntry()) {
      Observation observation = (Observation) entryComp.getResource();
      if (observation.hasStatus()) {
        if (!observation.getStatus().toCode().equals(observationStatus)) {
          filteredEntryComponents.add(new BundleEntryComponent().setResource(observation));
        } else {
          observationInError++;
        }
      }
    }
    logger.info("Skipped {} Observation with status entered-in-error", observationInError);
    filteredBundle.setEntry(filteredEntryComponents);
    return filteredBundle;
  }

  private Bundle filterObservationsBundleByCategory(
      Bundle bundle, String observationSocialHistory) {
    Bundle filteredBundle = new Bundle();
    List<BundleEntryComponent> filteredEntryComponents = new ArrayList<>();
    for (BundleEntryComponent entryComp : bundle.getEntry()) {
      Observation observation = (Observation) entryComp.getResource();
      List<CodeableConcept> observationCategories = observation.getCategory();
      boolean isSocialHistory =
          observationCategories.stream()
              .anyMatch(
                  category ->
                      category.getCoding().stream()
                          .anyMatch(coding -> coding.getCode().equals(observationSocialHistory)));
      if (isSocialHistory) {
        filteredEntryComponents.add(new BundleEntryComponent().setResource(observation));
      }
    }
    filteredBundle.setEntry(filteredEntryComponents);
    return filteredBundle;
  }

  public List<Observation> getSocialHistoryObservationDataOccupation(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    logger.info(
        "R4FhirData :{} Encounter :{} StartDate :{} and EndDate :{} in getSocialHistoryObservationDataOccupation. ",
        r4FhirData,
        encounter,
        start,
        end);
    logger.trace("Get Social History Observation Data (Occupation)");
    List<Observation> observations = new ArrayList<>();

    StringBuilder codeBuilder = new StringBuilder(2000);
    for (String occupationCode : QueryConstants.getOccupationSmtCodes()) {
      codeBuilder.append(QueryConstants.SNOMED_CODE_SYSTEM + "|" + occupationCode + ",");
    }

    for (String occupationCode : QueryConstants.getOccupationLoincCodes()) {
      codeBuilder.append(QueryConstants.LOINC_CODE_SYSTEM + "|" + occupationCode + ",");
    }
    codeBuilder.trimToSize();
    String codes =
        codeBuilder.substring(0, codeBuilder.length() - 1); // Remove extra "," at the end.

    String url =
        launchDetails.getEhrServerURL()
            + "/"
            + OBSERVATION
            + "?patient="
            + launchDetails.getLaunchPatientId()
            + "&code="
            + codes;

    Bundle occupationCodesbundle =
        (Bundle)
            FhirContextInitializer.getResourceBundleByUrl(
                launchDetails, client, context, OBSERVATION, url);

    if (occupationCodesbundle != null) {
      occupationCodesbundle = filterObservationByStatus(occupationCodesbundle, ENTERED_IN_ERROR);
      for (BundleEntryComponent entryComp : occupationCodesbundle.getEntry()) {
        observations.add((Observation) entryComp.getResource());
      }
    }

    logger.info("Filtered Social History Occupation Observations ----> {}", observations.size());
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
    logger.info(
        "R4FhirData :{} Encounter :{} StartDate :{} and EndDate :{} in getPregnancyConditions. ",
        r4FhirData,
        encounter,
        start,
        end);
    logger.trace("Get Pregnancy Conditions");
    List<Condition> conditions = new ArrayList<>();
    for (String pregnancySnomedCode : QueryConstants.getPregnancySmtCodes()) {
      Bundle pregnancyCodesbundle =
          (Bundle)
              fhirContextInitializer.getResourceByPatientIdAndCode(
                  launchDetails,
                  client,
                  context,
                  CONDITION,
                  pregnancySnomedCode,
                  QueryConstants.SNOMED_CODE_SYSTEM);
      if (pregnancyCodesbundle != null) {
        for (BundleEntryComponent entryComp : pregnancyCodesbundle.getEntry()) {
          Condition condition = (Condition) entryComp.getResource();
          List<Coding> conditionCodes = condition.getCode().getCoding();
          for (Coding conditionCoding : conditionCodes) {
            if (conditionCoding.getCode().equalsIgnoreCase(pregnancySnomedCode)
                && (isConditionActive(condition))) {
              conditions.add(condition);
            }
          }
        }
      }
    }
    logger.info("Filtered Pregnancy Conditions ----> {}", conditions.size());
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
    logger.info("R4FhirData :{} in getMedicationData. ", r4FhirData);
    return (Medication)
        fhirContextInitializer.getResouceById(
            launchDetails, client, context, "Medication", medicationId);
  }

  public List<MedicationAdministration> getMedicationAdministrationData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Date start,
      Date end) {
    logger.trace("Get MedicationAdministration Data");
    Bundle bundle =
        (Bundle)
            fhirContextInitializer.getResourceByPatientId(
                launchDetails, client, context, "MedicationAdministration");
    List<MedicationAdministration> medAdministrations = new ArrayList<>();
    List<CodeableConcept> medicationCodes = new ArrayList<>();

    if (bundle != null && bundle.getEntry() != null) {
      if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
        medAdministrations =
            filterByEncounterId(
                bundle, encounter.getIdElement().getIdPart(), MedicationAdministration.class);
      } else {
        medAdministrations = filterByDateRange(bundle, start, end, MedicationAdministration.class);
      }

      for (MedicationAdministration med : medAdministrations) {
        medicationCodes.addAll(findMedicationCodes(med));
      }
      r4FhirData.setR4MedicationCodes(medicationCodes);
    }
    logger.info("Filtered MedicationAdministration -----------> {}", medAdministrations.size());
    return medAdministrations;
  }

  private List<CodeableConcept> findMedicationRequestCodes(MedicationRequest medRequest) {
    List<CodeableConcept> medicationCodes = new ArrayList<>();

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
    logger.trace("Get MedicationRequest Data");
    Bundle bundle =
        (Bundle)
            fhirContextInitializer.getResourceByPatientId(
                launchDetails, client, context, "MedicationRequest");
    List<MedicationRequest> medRequests = new ArrayList<>();
    List<CodeableConcept> medicationCodes = new ArrayList<>();

    if (bundle != null && bundle.getEntry() != null) {
      if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
        medRequests =
            filterByEncounterId(
                bundle, encounter.getIdElement().getIdPart(), MedicationRequest.class);
      } else {
        medRequests = filterByDateRange(bundle, start, end, MedicationRequest.class);
      }

      for (MedicationRequest med : medRequests) {
        medicationCodes.addAll(findMedicationRequestCodes(med));
      }
      r4FhirData.setR4MedicationCodes(medicationCodes);
    }
    logger.info("Filtered MedicationRequests -----------> {} ", medRequests.size());
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
    logger.trace("Get MedicationStatement Data");
    Bundle bundle =
        (Bundle)
            fhirContextInitializer.getResourceByPatientId(
                launchDetails, client, context, "MedicationStatement");
    List<MedicationStatement> medStatements = new ArrayList<>();
    List<CodeableConcept> medicationCodes = new ArrayList<>();

    if (bundle != null && bundle.getEntry() != null) {
      if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
        medStatements =
            filterByEncounterId(
                bundle, encounter.getIdElement().getIdPart(), MedicationStatement.class);
      } else {
        medStatements = filterByDateRange(bundle, start, end, MedicationStatement.class);
      }

      for (MedicationStatement med : medStatements) {
        medicationCodes.addAll(findMedicationStatementCodes(med));
      }
      r4FhirData.setR4MedicationCodes(medicationCodes);
    }
    logger.info("Filtered MedicationStatement -----------> {}", medStatements.size());
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
    logger.trace("Get DiagnosticReport Data");
    Bundle bundle =
        (Bundle)
            fhirContextInitializer.getResourceByPatientId(
                launchDetails, client, context, "DiagnosticReport");
    List<DiagnosticReport> diagnosticReports = new ArrayList<>();
    List<CodeableConcept> diagnosticReportCodes = new ArrayList<>();

    if (bundle != null && bundle.getEntry() != null) {
      if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
        diagnosticReports =
            filterByEncounterId(
                bundle, encounter.getIdElement().getIdPart(), DiagnosticReport.class);
      } else {
        diagnosticReports = filterByDateRange(bundle, start, end, DiagnosticReport.class);
      }

      for (DiagnosticReport report : diagnosticReports) {
        diagnosticReportCodes.addAll(findDiagnosticReportCodes(report));
      }
      r4FhirData.setR4DiagnosticReportCodes(diagnosticReportCodes);
    }

    logger.info("Filtered DiagnosticReports -----------> {}", diagnosticReports.size());
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
    logger.trace("Get Immunization Data");
    Bundle bundle =
        (Bundle)
            fhirContextInitializer.getResourceByPatientId(
                launchDetails, client, context, "Immunization");
    List<Immunization> immunizations = new ArrayList<>();
    List<CodeableConcept> immunizationCodes = new ArrayList<>();
    if (bundle != null && bundle.getEntry() != null) {
      // Filter Immunizations based on Encounter Reference
      if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          Immunization immunization = (Immunization) entry.getResource();
          if (!immunization.getEncounter().isEmpty()
              && immunization
                  .getEncounter()
                  .getReferenceElement()
                  .getIdPart()
                  .equals(encounter.getIdElement().getIdPart())) {
            immunizations.add(immunization);
            immunizationCodes.addAll(findImmunizationCodes(immunization));
          } else {
            populateImmunizationsWithoutEncounters(
                bundle, immunizations, immunizationCodes, start, end);
          }
        }
        // If Encounter Id is not present using start and end dates to filter
        // Immunizations
      } else if (bundle != null) {

        populateImmunizationsWithoutEncounters(
            bundle, immunizations, immunizationCodes, start, end);
      }
      r4FhirData.setR4ImmunizationCodes(immunizationCodes);
    }
    logger.info("Filtered Immunizations -----------> {}", immunizations.size());
    return immunizations;
  }

  private void populateImmunizationsWithoutEncounters(
      Bundle b,
      List<Immunization> immunizations,
      List<CodeableConcept> immunizationCodes,
      Date start,
      Date end) {

    for (BundleEntryComponent entry : b.getEntry()) {
      Immunization immunization = (Immunization) entry.getResource();
      // Checking If Immunization DateTime is present in Immunization
      // resource
      if (immunization.getOccurrence().isDateTime()
          && immunization.getOccurrenceDateTimeType() != null) {
        if (isResourceWithinDateTime(
            start, end, immunization.getOccurrenceDateTimeType().dateTimeValue().getValue())) {
          immunizations.add(immunization);
          immunizationCodes.addAll(findImmunizationCodes(immunization));
        }
      }
      // If Immunization Date is not present looking for LastUpdatedDate
      else {
        Date lastUpdatedDateTime = immunization.getMeta().getLastUpdated();
        if (isResourceWithinDateTime(start, end, lastUpdatedDateTime)) {
          immunizations.add(immunization);
          immunizationCodes.addAll(findImmunizationCodes(immunization));
        }
      }
    }
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
      Date start,
      Date end) {
    logger.trace("Get ServiceRequest Data");
    Bundle bundle =
        (Bundle)
            fhirContextInitializer.getResourceByPatientId(
                launchDetails, client, context, "ServiceRequest");
    List<ServiceRequest> serviceRequests = new ArrayList<>();
    List<CodeableConcept> serviceRequestCodes = new ArrayList<>();

    if (bundle != null && bundle.getEntry() != null) {
      String encounterId = launchDetails.getEncounterId();
      if (StringUtils.isNotEmpty(encounterId)) {
        serviceRequests = filterByEncounterId(bundle, encounterId, ServiceRequest.class);
      } else {
        serviceRequests = filterByDateRange(bundle, start, end, ServiceRequest.class);
      }

      for (ServiceRequest req : serviceRequests) {
        serviceRequestCodes.addAll(findServiceRequestCodes(req));
      }
      r4FhirData.setR4ServiceRequestCodes(serviceRequestCodes);
    }

    logger.info("Filtered ServiceRequests -----------> {}", serviceRequests.size());
    return serviceRequests;
  }

  public List<Observation> filterObservation(
      Bundle bundle, Encounter encounter, Date start, Date end) {

    List<Observation> observations = new ArrayList<>();
    // Filter Observations based on Encounter Reference
    if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Observation observation = (Observation) entry.getResource();
        if (!observation.getEncounter().isEmpty()
            && observation
                .getEncounter()
                .getReferenceElement()
                .getIdPart()
                .equals(encounter.getIdElement().getIdPart())) {
          observations.add(observation);
        }
      }
      // If Encounter Id is not present using start and end dates to filter
      // Observations
    } else {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Observation observation = (Observation) entry.getResource();
        // Checking If Issued Date is present in Observation resource
        if (observation.getIssued() != null) {
          if (isResourceWithinDateTime(start, end, observation.getIssued())) {
            observations.add(observation);
          }
          // If Issued date is not present, Checking for Effective Date
        } else if (observation.getEffective() != null && !observation.getEffective().isEmpty()) {
          Type effectiveDate = observation.getEffectiveDateTimeType();
          Date effDate = effectiveDate.dateTimeValue().getValue();
          if (isResourceWithinDateTime(start, end, effDate)) {
            observations.add(observation);
          }
          // If Issued and Effective Date are not present looking for LastUpdatedDate
        } else {
          Date lastUpdatedDateTime = observation.getMeta().getLastUpdated();
          if (isResourceWithinDateTime(start, end, lastUpdatedDateTime)) {
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
      Patient patient =
          (Patient)
              fhirContextInitializer.getResouceById(
                  launchDetails, client, context, "Patient", launchDetails.getLaunchPatientId());
      if (patient != null) {
        r4FhirData.setPatient(patient);
        BundleEntryComponent patientEntry = new BundleEntryComponent();
        patientEntry.setResource(patient);
        bundle.addEntry(patientEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Patient Data", e);
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
      encounter = getEncounterData(context, client, launchDetails, r4FhirData, start, end);

      if (encounter != null) {
        r4FhirData.setEncounter(encounter);
        BundleEntryComponent encounterEntry = new BundleEntryComponent().setResource(encounter);
        bundle.addEntry(encounterEntry);
      }
    } catch (Exception e) {
      ApplicationUtils.handleException(e, "Error in getting Encounter Data", LogLevel.ERROR);
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
      List<Condition> conditionsList =
          getConditionData(context, client, launchDetails, r4FhirData, encounter, start, end);
      if (conditionsList != null && !conditionsList.isEmpty()) {
        // Already sorted and set in the getConditionData method
        for (Condition condition : conditionsList) {
          BundleEntryComponent conditionsEntry = new BundleEntryComponent().setResource(condition);
          bundle.addEntry(conditionsEntry);
        }
      }
    } catch (Exception e) {
      logger.error("Error in getting Condition Data", e);
    }

    // Get Observations for Patients and laboratory category (Write a method).
    // Filter the observations based on encounter Reference if encounter is present.
    // If encounter is not present, then filter based on times (Start and end, if
    // observation time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of labResultCodes.
    try {
      List<Observation> observationList =
          getObservationData(context, client, launchDetails, r4FhirData, start, end);
      if (observationList != null && !observationList.isEmpty()) {
        r4FhirData.setLabResults(observationList);
        for (Observation observation : observationList) {
          BundleEntryComponent observationsEntry =
              new BundleEntryComponent().setResource(observation);
          bundle.addEntry(observationsEntry);
        }
      }

      if (r4FhirData.getLabResultValueObservations() != null
          && !r4FhirData.getLabResultValueObservations().isEmpty()) {

        for (Observation observation : r4FhirData.getLabResultValueObservations()) {
          BundleEntryComponent observationsEntry =
              new BundleEntryComponent().setResource(observation);
          bundle.addEntry(observationsEntry);
        }
      }

    } catch (Exception e) {
      logger.error("Error in getting Observation Data", e);
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
      List<ServiceRequest> serviceRequestsList =
          getServiceRequestData(context, client, launchDetails, r4FhirData, start, end);
      if (serviceRequestsList != null && !serviceRequestsList.isEmpty()) {
        r4FhirData.setServiceRequests(serviceRequestsList);
        for (ServiceRequest serviceRequest : serviceRequestsList) {
          BundleEntryComponent serviceRequestEntry =
              new BundleEntryComponent().setResource(serviceRequest);
          bundle.addEntry(serviceRequestEntry);
        }
      }
    } catch (Exception e) {
      logger.error("Error in getting the ServiceRequest Data", e);
    }
    return bundle;
  }

  public Resource getResourceFromBundle(Bundle bundle, Class<?> resource) {
    try {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        if (entry.getResource() != null && entry.getResource().getClass() == resource) {
          return entry.getResource();
        }
      }
    } catch (Exception e) {
      logger.error("Error in getting the Resource from Bundle", e);
    }
    return null;
  }

  public void loadMedicationsData(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Bundle bundle,
      Date start,
      Date end) {

    try {
      List<MedicationAdministration> medAdministrationsList =
          getMedicationAdministrationData(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      if (medAdministrationsList != null && !medAdministrationsList.isEmpty()) {
        r4FhirData.setMedicationAdministrations(medAdministrationsList);
        for (MedicationAdministration medAdministration : medAdministrationsList) {
          processMedicationReferences(
              medAdministration.getMedication(),
              medAdministration.getContained(),
              context,
              client,
              launchDetails,
              r4FhirData,
              bundle);
          bundle.addEntry(new BundleEntryComponent().setResource(medAdministration));
        }
      }
    } catch (Exception e) {
      logger.error("Error in getting the MedicationAdministration Data", e);
    }

    try {
      List<MedicationRequest> medRequestsList =
          getMedicationRequestData(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      if (medRequestsList != null && !medRequestsList.isEmpty()) {
        r4FhirData.setMedicationRequests(medRequestsList);
        for (MedicationRequest medRequest : medRequestsList) {
          processMedicationReferences(
              medRequest.getMedication(),
              medRequest.getContained(),
              context,
              client,
              launchDetails,
              r4FhirData,
              bundle);
          bundle.addEntry(new BundleEntryComponent().setResource(medRequest));
        }
      }
    } catch (Exception e) {
      logger.error("Error in getting the MedicationRequest Data", e);
    }
  }

  private void processMedicationReferences(
      Type medication,
      List<Resource> containedResources,
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Bundle bundle) {
    if (medication == null || medication.isEmpty() || !(medication instanceof Reference)) {
      return;
    }

    Reference medRef = (Reference) medication;
    String medReference = medRef.getReferenceElement().getValue();

    if (medReference.startsWith("#")) {
      if (containedResources.stream()
          .anyMatch(resource -> resource.getIdElement().getValue().equals(medReference))) {
        logger.debug(
            "Medication Resource {} exists in contained resources, So no need to add again in Bundle.",
            medReference);
      }
      return;
    }

    logger.debug("Medication Reference Found=============> {}", medReference);
    Medication medicationData =
        getMedicationData(context, client, launchDetails, r4FhirData, medReference);
    if (medicationData != null) {
      bundle.addEntry(new BundleEntryComponent().setResource(medicationData));
      r4FhirData.getMedicationList().add(medicationData);
    }
  }

  public void loadPractitionersLocationAndOrganization(
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Encounter encounter,
      Bundle bundle,
      Date start,
      Date end) {
    logger.info(
        "StartDate :{} EndDate :{} in loadPractitionersLocationAndOrganization", start, end);

    if (encounter == null) {
      logger.debug("Encounter is null, cannot fetch Practitioners");
      return;
    }

    processPractitioners(encounter, context, client, launchDetails, r4FhirData, bundle);
    processOrganization(encounter, context, client, launchDetails, r4FhirData, bundle);
    processLocations(encounter, context, client, launchDetails, r4FhirData, bundle);
  }

  private void processPractitioners(
      Encounter encounter,
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Bundle bundle) {
    if (encounter.getParticipant() == null) {
      return;
    }

    List<Practitioner> practitionerList = new ArrayList<>();
    Map<String, String> practitionerMap = new HashMap<>();

    for (EncounterParticipantComponent participant : encounter.getParticipant()) {
      if (participant.getIndividual() != null) {
        String practitionerID = participant.getIndividual().getReferenceElement().getIdPart();
        if (!practitionerMap.containsKey(practitionerID)) {
          Practitioner practitioner =
              (Practitioner)
                  fhirContextInitializer.getResouceById(
                      launchDetails, client, context, "Practitioner", practitionerID);
          if (practitioner != null) {
            practitionerList.add(practitioner);
            practitionerMap.put(practitionerID, practitioner.getResourceType().name());
            bundle.addEntry(new BundleEntryComponent().setResource(practitioner));
          }
        }
      }
    }

    if (!practitionerList.isEmpty()) {
      r4FhirData.setPractitionersList(practitionerList);
    }
  }

  private void processOrganization(
      Encounter encounter,
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Bundle bundle) {
    if (!encounter.hasServiceProvider()) {
      return;
    }

    Reference organizationReference = encounter.getServiceProvider();
    if (!organizationReference.hasReferenceElement()) {
      return;
    }

    Organization organization =
        (Organization)
            fhirContextInitializer.getResouceById(
                launchDetails,
                client,
                context,
                "Organization",
                organizationReference.getReferenceElement().getIdPart());
    if (organization != null) {
      bundle.addEntry(new BundleEntryComponent().setResource(organization));
      r4FhirData.setOrganization(organization);
    }
  }

  private void processLocations(
      Encounter encounter,
      FhirContext context,
      IGenericClient client,
      LaunchDetails launchDetails,
      R4FhirData r4FhirData,
      Bundle bundle) {
    if (!encounter.hasLocation()) {
      return;
    }

    List<Location> locationList = new ArrayList<>();
    for (EncounterLocationComponent location : encounter.getLocation()) {
      if (location.getLocation() != null) {
        Location locationResource =
            (Location)
                fhirContextInitializer.getResouceById(
                    launchDetails,
                    client,
                    context,
                    "Location",
                    location.getLocation().getReferenceElement().getIdPart());
        if (locationResource != null && locationResource.hasAddress()) {
          locationList.add(locationResource);
          bundle.addEntry(new BundleEntryComponent().setResource(locationResource));
          r4FhirData.setLocation(locationResource);
        }
      }
    }

    r4FhirData.setLocationList(locationList);
    if (!locationList.isEmpty()) {
      r4FhirData.setLocation(locationList.get(0));
    }
  }

  public DocumentReference constructR4DocumentReference(
      String rrXml,
      String patientId,
      String encounterID,
      String providerUUID,
      String rrDocRefMimeType) {
    DocumentReference documentReference = new DocumentReference();

    // Set Doc Ref Status
    documentReference.setStatus(Enumerations.DocumentReferenceStatus.CURRENT);
    documentReference.setDocStatus(DocumentReference.ReferredDocumentStatus.FINAL);

    // Set Doc Ref Type
    CodeableConcept typeCode = new CodeableConcept();
    List<Coding> codingList = new ArrayList<>();
    Coding typeCoding = new Coding();
    typeCoding.setSystem(CdaParserConstants.RR_DOC_CODE_SYSTEM);
    typeCoding.setCode(CdaParserConstants.RR_DOC_CODE);
    typeCoding.setDisplay(CdaParserConstants.RR_DOC_DISPLAY_NAME);
    codingList.add(typeCoding);
    typeCode.setCoding(codingList);
    typeCode.setText(CdaParserConstants.RR_DOC_DISPLAY_NAME);
    documentReference.setType(typeCode);

    // Set Subject
    Reference patientReference = new Reference();
    patientReference.setReference("Patient/" + patientId);
    documentReference.setSubject(patientReference);

    // Set Author
    if (providerUUID != null) {
      List<Reference> authorRefList = new ArrayList<>();
      Reference providerReference = new Reference();
      providerReference.setReference("Practitioner/" + providerUUID);
      authorRefList.add(providerReference);
      documentReference.setAuthor(authorRefList);
    }

    // Set Doc Ref Content
    List<DocumentReference.DocumentReferenceContentComponent> contentList = new ArrayList<>();
    DocumentReference.DocumentReferenceContentComponent contentComp =
        new DocumentReference.DocumentReferenceContentComponent();
    Attachment attachment = new Attachment();
    attachment.setTitle("EICR Reportability Response");
    attachment.setContentType(rrDocRefMimeType);

    if (rrXml != null && !rrXml.isEmpty()) {
      attachment.setData(rrXml.getBytes());
    }
    contentComp.setAttachment(attachment);
    contentList.add(contentComp);
    documentReference.setContent(contentList);

    // Set Doc Ref Context
    DocumentReference.DocumentReferenceContextComponent docContextComp =
        new DocumentReference.DocumentReferenceContextComponent();
    List<Reference> encounterRefList = new ArrayList<>();
    Reference encounterReference = new Reference();
    encounterReference.setReference("Encounter/" + encounterID);
    encounterRefList.add(encounterReference);
    docContextComp.setEncounter(encounterRefList);

    Period period = new Period();
    period.setStart(new Date());
    period.setEnd(new Date());
    docContextComp.setPeriod(period);
    documentReference.setContext(docContextComp);

    String docReference =
        FhirContext.forR4().newJsonParser().encodeResourceToString(documentReference);
    logger.debug("DocumentReference Object===========> {}", docReference);

    return documentReference;
  }

  public boolean isResourceWithinDateTime(Date start, Date end, Date resourceDate) {
    boolean withinDateTime = false;
    if (start != null
        && end != null
        && resourceDate != null
        && resourceDate.after(start)
        && resourceDate.before(end)) {
      withinDateTime = true;
    }
    return withinDateTime;
  }

  private boolean isVerificationStatusPresent(Condition condition) {
    boolean present = false;

    if (condition.getVerificationStatus() != null
        && condition.getVerificationStatus().getCodingFirstRep() != null
        && condition.getVerificationStatus().getCodingFirstRep().getCode() != null) {
      present = true;
    }
    return present;
  }
}
