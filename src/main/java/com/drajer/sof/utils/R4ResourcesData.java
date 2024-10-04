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
import java.util.Collections;
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

  @Autowired FhirContextInitializer resourceData;

  @Autowired FhirContextInitializer fhirContextInitializer;

  private final Logger logger = LoggerFactory.getLogger(R4ResourcesData.class);

  private static final String OBSERVATION = "Observation";
  private static final String CONDITION = "Condition";
  private static final String ENCOUNTER = "Encounter";
  private static final String OBSERVATION_SOCIAL_HISTORY = "social-history";
  private static final String ENTERED_IN_ERROR = "entered-in-error";

  private static final String ENCOUNTER_DIAGNOSIS_CONDITION = "encounter-diagnosis";
  private static final String PROBLEM_LIST_CONDITION = "problem-list-item";

  private static final String ATTACHMENT_CONTENT_TYPE = "text/xml";

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

    if (resourceData.checkSkipResource(ENCOUNTER, (FhirClient) client)) {
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
      // If Encounter Id is not Present in Launch Details Get Encounters by Patient Id
      // and Find the latest Encounter
      Bundle bundle =
          (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context, ENCOUNTER);
      Map<Encounter, Date> encounterMap = new HashMap<>();
      if (bundle != null && bundle.getEntry() != null) {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          Encounter encounterEntry = (Encounter) entry.getResource();
          // Checking if Period element exists in Encounter. If Exists compare period is
          // falling in between Start and End Date
          if (!encounterEntry.getPeriod().isEmpty()) {
            Period period = encounterEntry.getPeriod();
            if (period.getStart().after(start)
                || (period.getEnd() != null && period.getEnd().before(end))) {
              encounterMap.put(encounterEntry, encounterEntry.getMeta().getLastUpdated());
            }
            // If period is not present using LastUpdatedDate in meta information to filter
            // the Encounter
          } else {
            Date lastUpdatedDateTime = encounterEntry.getMeta().getLastUpdated();
            if (isResourceWithinDateTime(start, end, lastUpdatedDateTime)) {
              encounterMap.put(encounterEntry, encounterEntry.getMeta().getLastUpdated());
            }
          }
        }
        encounter = Collections.max(encounterMap.entrySet(), Map.Entry.comparingByValue()).getKey();
        r4FhirData.setR4EncounterCodes(findEncounterCodes(encounter));
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
        (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context, CONDITION);
    List<Condition> allConditions = new ArrayList<>();
    List<Condition> problemConditions = new ArrayList<>();
    List<CodeableConcept> conditionCodes = new ArrayList<>();
    int conditionInError = 0;
    int conditionMissingAbatement = 0;
    List<Condition> encounterDiagnosisConditions = new ArrayList<>();

    if (bundle != null && bundle.getEntry() != null) {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        Condition condition = (Condition) entry.getResource();

        if ((!isVerificationStatusPresent(condition))
            || (!condition
                .getVerificationStatus()
                .getCodingFirstRep()
                .getCode()
                .equals(ENTERED_IN_ERROR))) {

          if (isConditionActive(condition) && condition.hasCategory()) {
            List<CodeableConcept> conditionCategory = condition.getCategory();
            for (CodeableConcept categoryCodeableConcept : conditionCategory) {
              List<Coding> categoryCodingList = categoryCodeableConcept.getCoding();
              for (Coding categoryCoding : categoryCodingList) {
                boolean foundPregnancyCondition =
                    condition
                        .getCode()
                        .getCoding()
                        .stream()
                        .anyMatch(
                            coding ->
                                Arrays.stream(QueryConstants.getPregnancySmtCodes())
                                    .anyMatch(coding.getCode()::equals));
                if (categoryCoding.getCode().equals(PROBLEM_LIST_CONDITION)
                    && !foundPregnancyCondition) {
                  logger.debug("Added condition to problem list {}", condition.getId());
                  problemConditions.add(condition);
                  conditionCodes.addAll(findConditionCodes(condition));
                } else if (categoryCoding.getCode().equals(ENCOUNTER_DIAGNOSIS_CONDITION)
                    && condition.hasEncounter()
                    && !foundPregnancyCondition) {
                  logger.info(ATTACHMENT_CONTENT_TYPE);
                  if (condition
                      .getEncounter()
                      .getReference()
                      .equals("Encounter/" + launchDetails.getEncounterId())) {
                    logger.debug(
                        "Added condition to Encounter Diagnosis list {}", condition.getId());
                    encounterDiagnosisConditions.add(condition);
                    conditionCodes.addAll(findConditionCodes(condition));
                  }
                }
              }
            }
          } else {
            conditionMissingAbatement++;
          }
        } else {
          conditionInError++;
        }
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
          conditionInError,
          conditionMissingAbatement);
    }
    logger.info("Filtered ConditionsList ----> {}", allConditions.size());
    return allConditions;
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
            resourceData.getObservationByPatientId(
                launchDetails, client, context, OBSERVATION, "laboratory");
    List<Observation> observations = new ArrayList<>();
    List<Observation> valueObservations = new ArrayList<>();
    List<CodeableConcept> observationCodes = new ArrayList<>();
    List<CodeableConcept> valueObservationCodes = new ArrayList<>();
    if (bundle != null && bundle.getEntry() != null) {
      // Filter Observations based on Encounter Reference
      String encounterId = launchDetails.getEncounterId();
      if (StringUtils.isNotEmpty(encounterId)) {
        bundle = filterObservationByStatus(bundle, ENTERED_IN_ERROR);
        for (BundleEntryComponent entry : bundle.getEntry()) {
          Observation observation = (Observation) entry.getResource();
          if (!observation.getEncounter().isEmpty()
              && observation.getEncounter().getReferenceElement().getIdPart().equals(encounterId)) {
            observations.add(observation);
            observationCodes.addAll(findLaboratoryCodes(observation));
            findAllValueCodes(observation, valueObservations, valueObservationCodes);
          }
        }
        // If Encounter Id is not present using start and end dates to filter
        // Observations
      } else {
        bundle = filterObservationByStatus(bundle, ENTERED_IN_ERROR);
        for (BundleEntryComponent entry : bundle.getEntry()) {
          Observation observation = (Observation) entry.getResource();
          // Checking If Issued Date is present in Observation resource
          if (observation.getIssued() != null) {
            if (isResourceWithinDateTime(start, end, observation.getIssued())) {
              observations.add(observation);
              observationCodes.addAll(findLaboratoryCodes(observation));
              findAllValueCodes(observation, valueObservations, valueObservationCodes);
            }
            // If Issued date is not present, Checking for Effective Date
          } else if (observation.getEffective() != null && !observation.getEffective().isEmpty()) {
            Type effectiveDate = observation.getEffectiveDateTimeType();
            Date effDate = effectiveDate.dateTimeValue().getValue();
            if (isResourceWithinDateTime(start, end, effDate)) {
              observations.add(observation);
              observationCodes.addAll(findLaboratoryCodes(observation));
              findAllValueCodes(observation, valueObservations, valueObservationCodes);
            }
            // If Issued and Effective Date are not present looking for LastUpdatedDate
          } else {
            Date lastUpdatedDateTime = observation.getMeta().getLastUpdated();
            if (isResourceWithinDateTime(start, end, lastUpdatedDateTime)) {
              observations.add(observation);
              observationCodes.addAll(findLaboratoryCodes(observation));
              findAllValueCodes(observation, valueObservations, valueObservationCodes);
            }
          }
        }
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
            resourceData.getResourceByPatientIdAndCode(
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
          observationCategories
              .stream()
              .anyMatch(
                  category ->
                      category
                          .getCoding()
                          .stream()
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
              resourceData.getResourceByPatientIdAndCode(
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
    logger.trace("Get MedicationAdministration Data");
    Bundle bundle =
        (Bundle)
            resourceData.getResourceByPatientId(
                launchDetails, client, context, "MedicationAdministration");
    List<MedicationAdministration> medAdministrations = new ArrayList<>();
    List<CodeableConcept> medicationCodes = new ArrayList<>();
    if (bundle != null && bundle.getEntry() != null) {
      // Filter MedicationAdministrations based on Encounter Reference
      if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          MedicationAdministration medAdministration =
              (MedicationAdministration) entry.getResource();
          if (!medAdministration.getContext().isEmpty()
              && medAdministration
                  .getContext()
                  .getReferenceElement()
                  .getIdPart()
                  .equals(encounter.getIdElement().getIdPart())) {
            medAdministrations.add(medAdministration);
            medicationCodes.addAll(findMedicationCodes(medAdministration));
          }
        }
        // If Encounter Id is not present using start and end dates to filter
        // MedicationAdministrations
      } else if (bundle != null) {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          MedicationAdministration medAdministration =
              (MedicationAdministration) entry.getResource();
          // Checking If Effective Date is present in MedicationAdministration resource
          if (medAdministration.getEffective() != null) {
            Pair<Date, TimeZone> effDate =
                CdaFhirUtilities.getActualDate(medAdministration.getEffective());
            if (isResourceWithinDateTime(start, end, effDate.getValue0())) {
              medAdministrations.add(medAdministration);
              medicationCodes.addAll(findMedicationCodes(medAdministration));
            }
          }
          // If Effective Date is not present looking for LastUpdatedDate
          else {
            Date lastUpdatedDateTime = medAdministration.getMeta().getLastUpdated();
            if (isResourceWithinDateTime(start, end, lastUpdatedDateTime)) {
              medAdministrations.add(medAdministration);
              medicationCodes.addAll(findMedicationCodes(medAdministration));
            }
          }
        }
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
            resourceData.getResourceByPatientId(
                launchDetails, client, context, "MedicationRequest");
    List<MedicationRequest> medRequests = new ArrayList<>();
    List<CodeableConcept> medicationCodes = new ArrayList<>();
    if (bundle != null && bundle.getEntry() != null) {
      // Filter MedicationAdministrations based on Encounter Reference
      if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          MedicationRequest medRequest = (MedicationRequest) entry.getResource();
          if (!medRequest.getEncounter().isEmpty()
              && medRequest
                  .getEncounter()
                  .getReferenceElement()
                  .getIdPart()
                  .equals(encounter.getIdElement().getIdPart())) {
            medRequests.add(medRequest);
            medicationCodes.addAll(findMedicationRequestCodes(medRequest));
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
            if (isResourceWithinDateTime(start, end, effDate)) {
              medRequests.add(medRequest);
              medicationCodes.addAll(findMedicationRequestCodes(medRequest));
            }
          }
          // If Effective Date is not present looking for LastUpdatedDate
          else {
            Date lastUpdatedDateTime = medRequest.getMeta().getLastUpdated();
            if (isResourceWithinDateTime(start, end, lastUpdatedDateTime)) {
              medRequests.add(medRequest);
              medicationCodes.addAll(findMedicationRequestCodes(medRequest));
            }
          }
        }
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
            resourceData.getResourceByPatientId(
                launchDetails, client, context, "MedicationStatement");
    List<MedicationStatement> medStatements = new ArrayList<>();
    List<CodeableConcept> medicationCodes = new ArrayList<>();
    if (bundle != null && bundle.getEntry() != null) {
      // Filter MedicationStatement based on Encounter Reference
      if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          MedicationStatement medStatement = (MedicationStatement) entry.getResource();
          if (!medStatement.getContext().isEmpty()
              && medStatement
                  .getContext()
                  .getReferenceElement()
                  .getIdPart()
                  .equals(encounter.getIdElement().getIdPart())) {
            medStatements.add(medStatement);
            medicationCodes.addAll(findMedicationStatementCodes(medStatement));
          }
        }
        // If Encounter Id is not present using start and end dates to filter
        // MedicationStatement
      } else if (bundle != null) {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          MedicationStatement medStatement = (MedicationStatement) entry.getResource();
          // Checking If Effective Date is present in MedicationStatement resource
          if (medStatement.hasEffectiveDateTimeType()) {
            Type effectiveDateTime = medStatement.getEffectiveDateTimeType();
            Date effDate = effectiveDateTime.dateTimeValue().getValue();
            if (isResourceWithinDateTime(start, end, effDate)) {
              medStatements.add(medStatement);
              medicationCodes.addAll(findMedicationStatementCodes(medStatement));
            }
          }
          // If Effective Date is not present looking for LastUpdatedDate
          else {
            Date lastUpdatedDateTime = medStatement.getMeta().getLastUpdated();
            if (isResourceWithinDateTime(start, end, lastUpdatedDateTime)) {
              medStatements.add(medStatement);
              medicationCodes.addAll(findMedicationStatementCodes(medStatement));
            }
          }
        }
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
            resourceData.getResourceByPatientId(launchDetails, client, context, "DiagnosticReport");
    List<DiagnosticReport> diagnosticReports = new ArrayList<>();
    List<CodeableConcept> diagnosticReportCodes = new ArrayList<>();
    if (bundle != null && bundle.getEntry() != null) {
      // Filter DiagnosticReports based on Encounter Reference
      if (encounter != null && !encounter.getIdElement().getValue().isEmpty()) {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          DiagnosticReport diagnosticReport = (DiagnosticReport) entry.getResource();
          if (!diagnosticReport.getEncounter().isEmpty()
              && diagnosticReport
                  .getEncounter()
                  .getReferenceElement()
                  .getIdPart()
                  .equals(encounter.getIdElement().getIdPart())) {
            diagnosticReports.add(diagnosticReport);
            diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
          }
        }
        // If Encounter Id is not present using start and end dates to filter
        // DiagnosticOrders
      } else if (bundle != null) {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          DiagnosticReport diagnosticReport = (DiagnosticReport) entry.getResource();
          // Checking If Issued Date is present in Observation resource
          if (diagnosticReport.getIssued() != null) {
            if (isResourceWithinDateTime(start, end, diagnosticReport.getIssued())) {
              diagnosticReports.add(diagnosticReport);
              diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
            }
            // If Issued date is not present, Checking for Effective Date
          } else if (!diagnosticReport.getEffective().isEmpty()) {
            Type effectiveDate = diagnosticReport.getEffective();
            Date effDate = effectiveDate.dateTimeValue().getValue();
            if (isResourceWithinDateTime(start, end, effDate)) {
              diagnosticReports.add(diagnosticReport);
              diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
            }
            // If Issued and Effective Date are not present looking for LastUpdatedDate
          } else {
            Date lastUpdatedDateTime = diagnosticReport.getMeta().getLastUpdated();
            if (isResourceWithinDateTime(start, end, lastUpdatedDateTime)) {
              diagnosticReports.add(diagnosticReport);
              diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
            }
          }
        }
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
            resourceData.getResourceByPatientId(launchDetails, client, context, "Immunization");
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
            resourceData.getResourceByPatientId(launchDetails, client, context, "ServiceRequest");
    List<ServiceRequest> serviceRequests = new ArrayList<>();
    List<CodeableConcept> serviceRequestCodes = new ArrayList<>();
    if (bundle != null && bundle.getEntry() != null) {
      // Filter ServiceRequests based on Encounter Reference
      String encounterId = launchDetails.getEncounterId();
      if (StringUtils.isNotEmpty(encounterId)) {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();

          if (!serviceRequest.getEncounter().isEmpty()
              && serviceRequest
                  .getEncounter()
                  .getReferenceElement()
                  .getIdPart()
                  .equals(encounterId)) {
            serviceRequests.add(serviceRequest);
            serviceRequestCodes.addAll(findServiceRequestCodes(serviceRequest));
          }
        }
        // If Encounter Id is not present using start and end dates to filter
        // ServiceRequests
      } else {
        for (BundleEntryComponent entry : bundle.getEntry()) {
          ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();
          // Checking If ServiceRequest DateTime is present in ServiceRequest
          // resource
          if (serviceRequest.getOccurrence() != null
              && serviceRequest.getOccurrence().isDateTime()) {
            if (serviceRequest.getOccurrenceDateTimeType() != null
                && isResourceWithinDateTime(
                    start,
                    end,
                    serviceRequest.getOccurrenceDateTimeType().dateTimeValue().getValue())) {
              serviceRequests.add(serviceRequest);
              serviceRequestCodes.addAll(findServiceRequestCodes(serviceRequest));
            }
          }
          // If ServiceRequest Date is not present looking for LastUpdatedDate
          else {
            Date lastUpdatedDateTime = serviceRequest.getMeta().getLastUpdated();
            if (isResourceWithinDateTime(start, end, lastUpdatedDateTime)) {
              serviceRequests.add(serviceRequest);
              serviceRequestCodes.addAll(findServiceRequestCodes(serviceRequest));
            }
          }
        }
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
      List<MedicationAdministration> medAdministrationsList =
          getMedicationAdministrationData(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      if (medAdministrationsList != null && !medAdministrationsList.isEmpty()) {
        List<Medication> medicationList = r4FhirData.getMedicationList();
        r4FhirData.setMedicationAdministrations(medAdministrationsList);
        for (MedicationAdministration medAdministration : medAdministrationsList) {
          if (medAdministration.getMedication() != null
              && !medAdministration.getMedication().isEmpty()
              && medAdministration.getMedication() instanceof Reference) {
            Reference medRef = (Reference) medAdministration.getMedication();
            String medReference = medRef.getReferenceElement().getValue();
            if (medReference.startsWith("#")) {
              List<Resource> medAdministrationContained = medAdministration.getContained();
              if (medAdministrationContained
                  .stream()
                  .anyMatch(resource -> resource.getIdElement().getValue().equals(medReference))) {
                logger.debug(
                    "Medication Resource {} exists in MedicationAdministration.contained, So no need to add again in Bundle.",
                    medReference);
              }
            } else {
              logger.debug("Medication Reference Found=============> {}", medReference);
              Medication medication =
                  getMedicationData(context, client, launchDetails, r4FhirData, medReference);
              if (medication != null) {
                BundleEntryComponent medicationEntry =
                    new BundleEntryComponent().setResource(medication);
                bundle.addEntry(medicationEntry);
                medicationList.add(medication);
              }
            }
          }
          BundleEntryComponent medAdministrationEntry =
              new BundleEntryComponent().setResource(medAdministration);
          bundle.addEntry(medAdministrationEntry);
        }
        r4FhirData.setMedicationList(medicationList);
      }
    } catch (Exception e) {
      logger.error("Error in getting the MedicationAdministration Data", e);
    }

    try {
      List<MedicationRequest> medRequestsList =
          getMedicationRequestData(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      if (medRequestsList != null && !medRequestsList.isEmpty()) {
        List<Medication> medicationList = r4FhirData.getMedicationList();
        r4FhirData.setMedicationRequests(medRequestsList);
        for (MedicationRequest medRequest : medRequestsList) {
          if (medRequest.getMedication() != null
              && !medRequest.getMedication().isEmpty()
              && medRequest.getMedication() instanceof Reference) {
            Reference medRef = (Reference) medRequest.getMedication();
            String medReference = medRef.getReferenceElement().getValue();
            if (medReference.startsWith("#")) {
              List<Resource> medRequestContained = medRequest.getContained();
              if (medRequestContained
                  .stream()
                  .anyMatch(resource -> resource.getIdElement().getValue().equals(medReference))) {
                logger.debug(
                    "Medication Resource {} exists in MedicationRequest.contained, So no need to add again in Bundle.",
                    medReference);
              }
            } else {
              logger.debug("Medication Reference Found=============> {}", medReference);
              Medication medication =
                  getMedicationData(context, client, launchDetails, r4FhirData, medReference);
              if (medication != null) {
                BundleEntryComponent medicationEntry =
                    new BundleEntryComponent().setResource(medication);
                bundle.addEntry(medicationEntry);
                medicationList.add(medication);
              }
            }
          }
          BundleEntryComponent medRequestEntry = new BundleEntryComponent().setResource(medRequest);
          bundle.addEntry(medRequestEntry);
        }
        r4FhirData.setMedicationList(medicationList);
      }
    } catch (Exception e) {
      logger.error("Error in getting the MedicationRequest Data", e);
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
    if (encounter != null) {

      // Load Practitioners
      if (encounter.getParticipant() != null) {

        List<Practitioner> practitionerList = new ArrayList<>();

        Map<String, String> practitionerMap = new HashMap<>();

        List<EncounterParticipantComponent> participants = encounter.getParticipant();

        for (EncounterParticipantComponent participant : participants) {
          if (participant.getIndividual() != null) {
            Reference practitionerReference = participant.getIndividual();
            String practitionerID = practitionerReference.getReferenceElement().getIdPart();
            if (!practitionerMap.containsKey(practitionerID)) {
              Practitioner practitioner =
                  (Practitioner)
                      fhirContextInitializer.getResouceById(
                          launchDetails, client, context, "Practitioner", practitionerID);
              if (practitioner != null) {
                practitionerList.add(practitioner);
                practitionerMap.put(practitionerID, practitioner.getResourceType().name());
                BundleEntryComponent practitionerEntry =
                    new BundleEntryComponent().setResource(practitioner);
                bundle.addEntry(practitionerEntry);
              }
            }
          }
        }
        if (!practitionerList.isEmpty()) {
          r4FhirData.setPractitionersList(practitionerList);
        }
      }

      // Add Organization
      if (Boolean.TRUE.equals(encounter.hasServiceProvider())) {
        Reference organizationReference = encounter.getServiceProvider();
        if (organizationReference.hasReferenceElement()) {
          Organization organization =
              (Organization)
                  fhirContextInitializer.getResouceById(
                      launchDetails,
                      client,
                      context,
                      "Organization",
                      organizationReference.getReferenceElement().getIdPart());
          if (organization != null) {
            BundleEntryComponent organizationEntry =
                new BundleEntryComponent().setResource(organization);
            bundle.addEntry(organizationEntry);
            r4FhirData.setOrganization(organization);
          }
        }
      }

      // Add Locations
      if (Boolean.TRUE.equals(encounter.hasLocation())) {
        List<Location> locationList = new ArrayList<>();
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
            if (locationResource != null && locationResource.hasAddress()) {
              locationList.add(locationResource);
              BundleEntryComponent locationEntry =
                  new BundleEntryComponent().setResource(locationResource);
              bundle.addEntry(locationEntry);
              r4FhirData.setLocation(locationResource);
            }
          }
        }
        r4FhirData.setLocationList(locationList);

        if (!locationList.isEmpty()) {
          r4FhirData.setLocation(locationList.get(0));
        }
      }
    } else {
      logger.debug("Encounter is null, cannot fetch Practitioners");
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
