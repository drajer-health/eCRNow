package com.drajer.bsa.utils;

import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.eca.model.EcaUtils;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.MatchTriggerStatus;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Encounter.EncounterLocationComponent;
import org.hl7.fhir.r4.model.codesystems.ObservationCategory;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The class is used to convert from Release3.0 data structures to Release2.0 data structures so
 * that common code such as CDA generation, validation and routing can be reused from Release2.0.
 *
 * @author nbashyam
 */
public class R3ToR2DataConverterUtils {

  private static final Logger logger = LoggerFactory.getLogger(R3ToR2DataConverterUtils.class);
  private static final String SNOMED_CODE_237364002 = "237364002";
  private static final String LOINC_URL = "http://loinc.org";
  private static final String SNOMED_INFO_SCT_URL = "http://snomed.info/sct";

  private R3ToR2DataConverterUtils() {
    super();
  }

  /**
   * The method creates the Release2.0 R4FhirData and LaunchDetails data structures from
   * KarProcessingData.
   *
   * @param kd
   * @return
   */
  public static Pair<R4FhirData, LaunchDetails> convertKarProcessingDataForCdaGeneration(
      KarProcessingData kd, BsaAction act) {

    R4FhirData r4FhirData = new R4FhirData();
    LaunchDetails details = new LaunchDetails();
    Map<String, List<String>> uniqueResourceIdsByType = new HashMap<>();
    Bundle data = new Bundle();

    if (kd != null) {

      logger.debug(" KarProcessingData is not null, to be converted ");
      setPatientStateInLaunchDetails(kd, details);
      logger.info(" Patient State created from KarProcessingData is : {}", details.getStatus());

      details.setEhrServerURL(kd.getNotificationContext().getFhirServerBaseUrl());
      details.setAssigningAuthorityId(kd.getHealthcareSetting().getAssigningAuthorityId());
      details.setLaunchPatientId(kd.getNotificationContext().getPatientId());
      details.setEncounterId(kd.getNotificationContext().getNotificationResourceId());
      details.setProviderUUID(kd.getHealthcareSetting().getDefaultProviderId());
      details.setSetId(details.getLaunchPatientId() + "|" + details.getEncounterId());

      if (kd.getKar() != null) {
        details.setRctcOid(kd.getKar().getRctcOid());
        details.setRctcVersion(kd.getKar().getRctcVersion());
      }

      if (kd.getPhm() != null) {
        details.setVersionNumber(kd.getPhm().getSubmittedVersionNumber() + 1);
      } else {
        details.setVersionNumber(1);
      }

      List<DataRequirement> reqs = act.getInputData();

      for (DataRequirement dr : reqs) {

        Set<Resource> resources = kd.getDataForId(dr.getId(), act.getRelatedDataId(dr.getId()));

        if (resources != null) {

          addResourcesToR4FhirData(
              dr.getId(),
              data,
              r4FhirData,
              details,
              resources,
              dr.getType(),
              uniqueResourceIdsByType);
        }
      }

      addAdministrativeResources(null, data, r4FhirData, details, kd, act, uniqueResourceIdsByType);
      addSecondaryResources(null, data, r4FhirData, details, kd, act, uniqueResourceIdsByType);
      setLocationAndServiceProviderOrganization(kd, r4FhirData);

    } else {

      logger.error(" Cannot convert from R3 to R2 as the KarProcessingData is null ");
    }

    r4FhirData.setData(data);
    return new Pair<>(r4FhirData, details);
  }

  public static void addSecondaryResources(
      String dataId,
      Bundle data,
      R4FhirData r4FhirData,
      LaunchDetails details,
      KarProcessingData kd,
      BsaAction act,
      Map<String, List<String>> r4DataResourceIds) {
    logger.info("BsaAction in addSecondaryResources:{}", act);

    Set<Resource> medications = kd.getResourcesByType(ResourceType.Medication.toString());
    addResourcesToR4FhirData(
        dataId,
        data,
        r4FhirData,
        details,
        medications,
        ResourceType.Medication.toString(),
        r4DataResourceIds);

    Set<Resource> observations = kd.getResourcesByType(ResourceType.Observation.toString());
    addResourcesToR4FhirData(
        dataId,
        data,
        r4FhirData,
        details,
        observations,
        ResourceType.Observation.toString(),
        r4DataResourceIds);

    Set<Resource> specimens = kd.getResourcesByType(ResourceType.Specimen.toString());
    addResourcesToR4FhirData(
        dataId,
        data,
        r4FhirData,
        details,
        specimens,
        ResourceType.Specimen.toString(),
        r4DataResourceIds);

    Set<Resource> medicationRequests =
        kd.getResourcesByType(ResourceType.MedicationRequest.toString());
    addResourcesToR4FhirData(
        dataId,
        data,
        r4FhirData,
        details,
        medicationRequests,
        ResourceType.MedicationRequest.toString(),
        r4DataResourceIds);
  }

  public static void addAdministrativeResources(
      String dataId,
      Bundle data,
      R4FhirData r4FhirData,
      LaunchDetails details,
      KarProcessingData kd,
      BsaAction act,
      Map<String, List<String>> uniqueResourceIdsByType) {
    logger.info("BsaAction in addAdministrativeResources:{}", act);

    Set<Resource> practitioners = kd.getResourcesByType(ResourceType.Practitioner.toString());
    addResourcesToR4FhirData(
        dataId,
        data,
        r4FhirData,
        details,
        practitioners,
        ResourceType.Practitioner.toString(),
        uniqueResourceIdsByType);

    Set<Resource> locations = kd.getResourcesByType(ResourceType.Location.toString());
    addResourcesToR4FhirData(
        dataId,
        data,
        r4FhirData,
        details,
        locations,
        ResourceType.Location.toString(),
        uniqueResourceIdsByType);

    Set<Resource> orgs = kd.getResourcesByType(ResourceType.Organization.toString());
    addResourcesToR4FhirData(
        dataId,
        data,
        r4FhirData,
        details,
        orgs,
        ResourceType.Organization.toString(),
        uniqueResourceIdsByType);
  }

  public static void addResourcesToR4FhirData(
      String dataId,
      Bundle data,
      R4FhirData r4FhirData,
      LaunchDetails details,
      Set<Resource> resources,
      String type,
      Map<String, List<String>> uniqueResourceIdsByType) {
    logger.info("Data id in addResourcesToR4FhirData:{}", dataId);

    removeDuplicatesAndUpdateData(resources, type, uniqueResourceIdsByType);

    if (resources == null || resources.isEmpty()) {
      logger.warn(" Cannot add null resources for type {}", type);
      return;
    }

    ResourceType resourceType = ResourceType.fromCode(type);
    if (resourceType == null) {
      logger.error(
          " Unknow Resource Type {} passed for report creation. Data from resource type : {} will not be used",
          type,
          type);
      return;
    }

    switch (resourceType) {
      case Patient:
        addPatient(resources, r4FhirData, details, data);
        break;
      case Encounter:
        addEncounter(resources, r4FhirData, details, data);
        break;
      case Location:
        addLocations(resources, r4FhirData, data);
        break;
      case Organization:
        addOrganizations(resources, r4FhirData, data);
        break;
      case Practitioner:
        addPractitioners(resources, r4FhirData, data);
        break;
      case Condition:
        addConditions(resources, r4FhirData, data);
        break;
      case Immunization:
        addImmunizations(resources, r4FhirData, data);
        break;
      case Procedure:
        addProcedures(resources, r4FhirData, data);
        break;
      case MedicationRequest:
        addMedicationRequests(resources, r4FhirData, data);
        break;
      case MedicationAdministration:
        addMedicationAdministrations(resources, r4FhirData, data);
        break;
      case MedicationStatement:
        addMedicationStatements(resources, r4FhirData, data);
        break;
      case Medication:
        addMedications(resources, r4FhirData, data);
        break;
      case ServiceRequest:
        addServiceRequests(resources, r4FhirData, data);
        break;
      case Observation:
        addObservations(resources, r4FhirData, data);
        break;
      case Specimen:
        addSpecimens(resources, r4FhirData, data);
        break;
      case DiagnosticReport:
        addDiagnosticReports(resources, r4FhirData, data);
        break;
      default:
        logger.error(
            " Unknow Resource Type {} passed for report creation. Data from resource type : {} will not be used",
            type,
            type);
    }
  }

  private static void addPatient(
      Set<Resource> resources, R4FhirData r4FhirData, LaunchDetails details, Bundle data) {
    logger.info(" Setting up the patient for R4FhirData ");
    Resource patient = resources.iterator().next();
    r4FhirData.setPatient((Patient) patient);
    details.setLaunchPatientId(patient.getIdElement().getIdPart());
    data.addEntry(new BundleEntryComponent().setResource(patient));
  }

  private static void addEncounter(
      Set<Resource> resources, R4FhirData r4FhirData, LaunchDetails details, Bundle data) {
    logger.info(" Setting up the encounter for R4FhirData ");
    Resource encounter = resources.iterator().next();
    r4FhirData.setEncounter((Encounter) encounter);
    details.setEncounterId(encounter.getIdElement().getIdPart());
    data.addEntry(new BundleEntryComponent().setResource(encounter));
  }

  private static void addLocations(Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the location for R4FhirData ");
    Resource location = resources.iterator().next();
    r4FhirData.setLocation((Location) location);
    data.addEntry(new BundleEntryComponent().setResource(location));

    ArrayList<Location> locList = new ArrayList<>();
    for (Resource r : resources) {
      locList.add((Location) r);
      data.addEntry(new BundleEntryComponent().setResource((Location) r));
    }
    r4FhirData.addLocations(locList);
  }

  private static void addOrganizations(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the organization for R4FhirData ");
    Resource organization = resources.iterator().next();
    r4FhirData.setOrganization((Organization) organization);

    ArrayList<Organization> orgList = new ArrayList<>();
    for (Resource r : resources) {
      orgList.add((Organization) r);
      data.addEntry(new BundleEntryComponent().setResource((Organization) r));
    }
    r4FhirData.addOrganization(orgList);
  }

  private static void addPractitioners(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the Practitioner for R4FhirData ");
    ArrayList<Practitioner> practitioners = new ArrayList<>();
    for (Resource r : resources) {
      practitioners.add((Practitioner) r);
      data.addEntry(new BundleEntryComponent().setResource(r));
    }
    r4FhirData.addPractitionersList(practitioners);
  }

  private static void addConditions(Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the Conditions for R4FhirData ");
    ArrayList<Condition> conditionList = new ArrayList<>();
    ArrayList<Condition> encDiagList = new ArrayList<>();
    List<Condition> pregnancyConditions = new ArrayList<>();

    for (Resource r : resources) {
      Condition c = (Condition) r;
      conditionList.add(c);

      if (isEncounterDiagnosis(c)) {
        logger.info(" Adding Encounter Diagnosis ");
        encDiagList.add(c);
      }

      data.addEntry(new BundleEntryComponent().setResource(r));

      if (c.hasCode() && isPregnancyCondition(c.getCode())) {
        logger.info(" Adding Pregnancy Condition ");
        pregnancyConditions.add(c);
      }
    }

    r4FhirData.addPregnancyConditions(pregnancyConditions);
    r4FhirData.addConditions(conditionList);
    r4FhirData.addEncounterDiagnosisConditions(encDiagList);
  }

  private static boolean isEncounterDiagnosis(Condition c) {
    return c.getCategoryFirstRep() != null
        && c.getCategoryFirstRep().getCodingFirstRep() != null
        && c.getCategoryFirstRep().getCodingFirstRep().getCode() != null
        && c.getCategoryFirstRep()
            .getCodingFirstRep()
            .getCode()
            .contentEquals("encounter-diagnosis");
  }

  private static void addImmunizations(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the Immunization for R4FhirData ");
    ArrayList<Immunization> immList = new ArrayList<>();
    for (Resource r : resources) {
      immList.add((Immunization) r);
      data.addEntry(new BundleEntryComponent().setResource(r));
    }
    r4FhirData.addImmunizations(immList);
  }

  private static void addProcedures(Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the Procedure for R4FhirData ");
    ArrayList<Procedure> procList = new ArrayList<>();
    for (Resource r : resources) {
      procList.add((Procedure) r);
      data.addEntry(new BundleEntryComponent().setResource(r));
    }
    r4FhirData.setProcedureList(procList);
  }

  private static void addMedicationRequests(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the MedicationRequest for R4FhirData ");
    ArrayList<MedicationRequest> medReqList = new ArrayList<>();
    for (Resource r : resources) {
      medReqList.add((MedicationRequest) r);
      data.addEntry(new BundleEntryComponent().setResource(r));
    }
    r4FhirData.addMedicationRequests(medReqList);
  }

  private static void addMedicationAdministrations(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the MedicationAdministration for R4FhirData ");
    ArrayList<MedicationAdministration> medAdmList = new ArrayList<>();
    for (Resource r : resources) {
      medAdmList.add((MedicationAdministration) r);
      data.addEntry(new BundleEntryComponent().setResource(r));
    }
    r4FhirData.addMedicationAdministrations(medAdmList);
  }

  private static void addMedicationStatements(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the MedicationStatement for R4FhirData ");
    ArrayList<MedicationStatement> medStatementList = new ArrayList<>();
    for (Resource r : resources) {
      medStatementList.add((MedicationStatement) r);
      data.addEntry(new BundleEntryComponent().setResource(r));
    }
    r4FhirData.addMedicationStatements(medStatementList);
  }

  private static void addMedications(Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the Medication for R4FhirData ");
    ArrayList<Medication> medList = new ArrayList<>();
    for (Resource r : resources) {
      medList.add((Medication) r);
      data.addEntry(new BundleEntryComponent().setResource(r));
    }
    r4FhirData.addMedicationList(medList);
  }

  private static void addServiceRequests(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the ServiceRequest for R4FhirData ");
    ArrayList<ServiceRequest> servReqList = new ArrayList<>();
    for (Resource r : resources) {
      servReqList.add((ServiceRequest) r);
      data.addEntry(new BundleEntryComponent().setResource(r));
    }
    r4FhirData.addServiceRequests(servReqList);
  }

  private static void addObservations(Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    addLabObservations(resources, r4FhirData, data);
    addVitalSignsObservations(resources, r4FhirData, data);
    addSocialHistoryObservations(resources, r4FhirData, data);
  }

  private static void addLabObservations(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the LabResults for R4FhirData ");
    Set<Resource> labObs =
        ReportGenerationUtils.filterObservationsByCategory(
            resources, ObservationCategory.LABORATORY.toCode());
    ArrayList<Observation> labObsList = new ArrayList<>();
    if (labObs != null && !labObs.isEmpty()) {
      for (Resource r : labObs) {
        labObsList.add((Observation) r);
        data.addEntry(new BundleEntryComponent().setResource(r));
      }
      r4FhirData.addLabResults(labObsList);
    }
  }

  private static void addVitalSignsObservations(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the Vital Signs for R4FhirData ");
    Set<Resource> vitalObs =
        ReportGenerationUtils.filterObservationsByCategory(
            resources, ObservationCategory.VITALSIGNS.toCode());
    ArrayList<Observation> vitalObsList = new ArrayList<>();
    if (vitalObs != null && !vitalObs.isEmpty()) {
      for (Resource r : vitalObs) {
        vitalObsList.add((Observation) r);
        data.addEntry(new BundleEntryComponent().setResource(r));
      }
      r4FhirData.setVitalObs(vitalObsList);
    }
  }

  private static void addSocialHistoryObservations(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the SocialHistory for R4FhirData ");
    Set<Resource> socObs = ReportGenerationUtils.filterSocialHistoryObservations(resources);

    if (socObs == null || socObs.isEmpty()) {
      return;
    }

    ArrayList<Observation> socObsList = new ArrayList<>();
    List<Observation> occObs = new ArrayList<>();
    List<Observation> travelObs = new ArrayList<>();
    List<Observation> pregnancyObs = new ArrayList<>();
    List<Observation> pregnancyStatusObs = new ArrayList<>();
    List<Observation> lmpObs = new ArrayList<>();
    List<Observation> postPartumObs = new ArrayList<>();
    List<Observation> pregnancyOutcomeObs = new ArrayList<>();
    List<Observation> homelessObs = new ArrayList<>();
    List<Observation> disabilityObs = new ArrayList<>();
    List<Observation> vaccineCredObs = new ArrayList<>();
    List<Observation> residencyObs = new ArrayList<>();
    List<Observation> nationalityObs = new ArrayList<>();
    List<Observation> pregnancyIntentionObs = new ArrayList<>();

    for (Resource r : socObs) {
      Observation sochisObs = (Observation) r;
      socObsList.add(sochisObs);
      data.addEntry(new BundleEntryComponent().setResource(r));
      categorizeSocialHistoryObservation(
          sochisObs,
          occObs,
          travelObs,
          pregnancyObs,
          pregnancyStatusObs,
          lmpObs,
          postPartumObs,
          pregnancyOutcomeObs,
          homelessObs,
          disabilityObs,
          vaccineCredObs,
          residencyObs,
          nationalityObs,
          pregnancyIntentionObs);
    }

    r4FhirData.addOccupationObs(occObs);
    r4FhirData.addTravelObs(travelObs);
    r4FhirData.addPregnancyObs(pregnancyObs);
    r4FhirData.addPregnancyStatusObs(pregnancyStatusObs);
    r4FhirData.addLmpObs(lmpObs);
    r4FhirData.addPostPartumObs(postPartumObs);
    r4FhirData.addPregnancyOutcomeObs(pregnancyOutcomeObs);
    r4FhirData.addHomelessObs(homelessObs);
    r4FhirData.addDisabilityObs(disabilityObs);
    r4FhirData.addVaccineCredObs(vaccineCredObs);
    r4FhirData.addResidencyObs(residencyObs);
    r4FhirData.addNationalityObs(nationalityObs);
    r4FhirData.addPregnancyIntentionObs(pregnancyIntentionObs);
  }

  private static void categorizeSocialHistoryObservation(
      Observation sochisObs,
      List<Observation> occObs,
      List<Observation> travelObs,
      List<Observation> pregnancyObs,
      List<Observation> pregnancyStatusObs,
      List<Observation> lmpObs,
      List<Observation> postPartumObs,
      List<Observation> pregnancyOutcomeObs,
      List<Observation> homelessObs,
      List<Observation> disabilityObs,
      List<Observation> vaccineCredObs,
      List<Observation> residencyObs,
      List<Observation> nationalityObs,
      List<Observation> pregnancyIntentionObs) {

    if (!sochisObs.hasCode()) {
      return;
    }

    if (isOccupationObservation(sochisObs.getCode())) {
      logger.info(" Found Occupation History Observation ");
      occObs.add(sochisObs);
    } else if (isTravelObservation(sochisObs.getCode())) {
      logger.info(" Found Travel History Observation ");
      travelObs.add(sochisObs);
    } else if (isPregnancyObservation(sochisObs.getCode())) {
      logger.info(" Found Pregnancy Observation ");
      pregnancyObs.add(sochisObs);
    } else if (isPregnancyStatusObservation(sochisObs.getCode())) {
      logger.info(" Found Pregnancy Status Observation ");
      pregnancyStatusObs.add(sochisObs);
    } else if (isLastMenstrualPeriodObservation(sochisObs.getCode())) {
      logger.info(" Found LMP Observation ");
      lmpObs.add(sochisObs);
    } else if (isPostPartumStatusObservation(sochisObs.getCode())) {
      logger.info(" Found Post Partum Status Observation ");
      postPartumObs.add(sochisObs);
    } else if (isPregnancyOutcomeObservation(sochisObs.getCode())) {
      logger.info(" Found Pregnancy Outcome Observation ");
      pregnancyOutcomeObs.add(sochisObs);
    } else if (isHomelessObservation(sochisObs.getCode())) {
      logger.info(" Found Homeless Observation ");
      homelessObs.add(sochisObs);
    } else if (isDisabilityObservation(sochisObs.getCode())) {
      logger.info(" Found Disability Observation ");
      disabilityObs.add(sochisObs);
    } else if (isVaccineCredObservation(sochisObs.getCode())) {
      logger.info(" Found Vaccine Credential Observation ");
      vaccineCredObs.add(sochisObs);
    } else if (isResidencyObservation(sochisObs.getCode())) {
      logger.info(" Found Residency Info Observation ");
      residencyObs.add(sochisObs);
    } else if (isNationalityObservation(sochisObs.getCode())) {
      logger.info(" Found Nationality Observation ");
      nationalityObs.add(sochisObs);
    } else if (isPregnancyIntentionObservation(sochisObs.getCode())) {
      logger.info(" Found Pregnancy Intention Observation ");
      pregnancyIntentionObs.add(sochisObs);
    }
  }

  private static void addSpecimens(Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the Specimen for R4FhirData ");
    ArrayList<Specimen> specimenList = new ArrayList<>();
    for (Resource r : resources) {
      specimenList.add((Specimen) r);
      data.addEntry(new BundleEntryComponent().setResource(r));
    }
    r4FhirData.setSpecimenList(specimenList);
  }

  private static void addDiagnosticReports(
      Set<Resource> resources, R4FhirData r4FhirData, Bundle data) {
    logger.info(" Setting up the Diagnostic Report for R4FhirData ");
    ArrayList<DiagnosticReport> diagReportList = new ArrayList<>();
    for (Resource r : resources) {
      diagReportList.add((DiagnosticReport) r);
      data.addEntry(new BundleEntryComponent().setResource(r));
    }
    r4FhirData.addDiagReports(diagReportList);
  }

  public static void setLocationAndServiceProviderOrganization(
      KarProcessingData kd, R4FhirData r4FhirData) {

    // Get the encounter.
    Encounter enc = kd.getContextEncounter();

    if (enc != null) {

      // Get the Service Provider organization
      if (enc.hasServiceProvider()) {
        Reference orgRef = enc.getServiceProvider();

        Resource orgRes =
            kd.getResourceById(orgRef.getReferenceElement().getIdPart(), ResourceType.Organization);

        if (orgRes != null) {

          logger.info(" Found the organization for id {}", orgRes.getId());
          Organization org = (Organization) orgRes;

          r4FhirData.setOrganization(org);
        } else {
          logger.error(
              " Did not find the service provider in KarProcessingData {}", orgRef.getId());
        }
      } else {
        logger.error(" Service Provider not set in Encounter ");
      }

      // Get the location
      if (enc.hasLocation()) {

        List<EncounterLocationComponent> locComps = enc.getLocation();

        for (EncounterLocationComponent el : locComps) {

          Reference locRef = el.getLocation();
          Resource locRes =
              kd.getResourceById(locRef.getReferenceElement().getIdPart(), ResourceType.Location);

          if (locRes != null) {

            logger.info(" Found the location for id {}", locRes.getId());
            Location loc = (Location) locRes;

            r4FhirData.setLocation(loc);
            break;
          } else {
            logger.error(" Did not find the location in KarProcessingData {}", locRef.getId());
          }
        } // for
      } else {
        logger.error(" Location is not set in Encounter ");
      }

    } else {
      logger.error(" Context Encounter is null ");
    }
  }

  public static Boolean isOccupationObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && ((c.getCode().contentEquals("11295-3") && c.getSystem().contains(LOINC_URL))
                || (c.getCode().contentEquals("11341-5") && c.getSystem().contains(LOINC_URL))
                || (c.getCode().contentEquals("21843-8") && c.getSystem().contains(LOINC_URL))
                || (c.getCode().contentEquals("74165-2") && c.getSystem().contains(LOINC_URL))
                || (c.getCode().contentEquals("224362002")
                    && c.getSystem().contains(SNOMED_INFO_SCT_URL))
                || (c.getCode().contentEquals("364703007")
                    && c.getSystem().contains(SNOMED_INFO_SCT_URL)))) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isTravelObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && (
            /*(c.getCode().contentEquals("29762-2") && c.getSystem().contains(LOINC_URL)) */
            (c.getCode().contentEquals("161085007") && c.getSystem().contains(SNOMED_INFO_SCT_URL))
                || (c.getCode().contentEquals("161086008")
                    && c.getSystem().contains(SNOMED_INFO_SCT_URL))
                || (c.getCode().contentEquals("420008001")
                    && c.getSystem().contains(SNOMED_INFO_SCT_URL))
                || (c.getCode().contentEquals("46521000175102")
                    && c.getSystem().contains(SNOMED_INFO_SCT_URL))
                || (c.getCode().contentEquals("34831000175105")
                    && c.getSystem().contains(SNOMED_INFO_SCT_URL))
                || (c.getCode().contentEquals("443846001")
                    && c.getSystem().contains(SNOMED_INFO_SCT_URL)))) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isPregnancyObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && ((c.getCode().contentEquals("90767-5") && c.getSystem().contains(LOINC_URL))
                || (c.getCode().contentEquals("146799005")
                    && c.getSystem().contains(SNOMED_INFO_SCT_URL))
                || (c.getCode().contentEquals("60001007")
                    && c.getSystem().contains(SNOMED_INFO_SCT_URL))
                || (c.getCode().contentEquals("77386006")
                    && c.getSystem().contains(SNOMED_INFO_SCT_URL)))) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isPregnancyCondition(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getSystem().contains(SNOMED_INFO_SCT_URL)
            && (c.getCode().contentEquals("77386006")
                || c.getCode().contentEquals("146799005")
                || c.getCode().contentEquals("60001007"))) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isPregnancyStatusObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getCode().contentEquals("82810-3")
            && c.getSystem().contains(LOINC_URL)) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isPregnancyIntentionObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getCode().contentEquals(CdaGeneratorConstants.PREGNANCY_INTENTION_CODE)
            && c.getSystem().contains(LOINC_URL)) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isLastMenstrualPeriodObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getCode().contentEquals(CdaGeneratorConstants.LMP_CODE)
            && c.getSystem().contains(LOINC_URL)) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isPostPartumStatusObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getCode().contentEquals("249197004")
            && c.getSystem().contains(SNOMED_INFO_SCT_URL)) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isPregnancyOutcomeObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getSystem().contains(SNOMED_INFO_SCT_URL)
            && (c.getCode().contentEquals("17369002")
                || c.getCode().contentEquals("21243004")
                || c.getCode().contentEquals(SNOMED_CODE_237364002)
                || c.getCode().contentEquals(SNOMED_CODE_237364002)
                || c.getCode().contentEquals(SNOMED_CODE_237364002))) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isHomelessObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getSystem().contains(SNOMED_INFO_SCT_URL)
            && (c.getCode().contentEquals("32911000") || c.getCode().contentEquals("105526001"))) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isDisabilityObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getSystem().contains(LOINC_URL)
            && (c.getCode().contentEquals("69856-3")
                || c.getCode().contentEquals("69857-1")
                || c.getCode().contentEquals("69858-9")
                || c.getCode().contentEquals("69859-7")
                || c.getCode().contentEquals("69860-5")
                || c.getCode().contentEquals("69861-3"))) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isVaccineCredObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getSystem().contains(LOINC_URL)
            && (c.getCode().contentEquals("11370-4"))) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isResidencyObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getSystem().contains(LOINC_URL)
            && (c.getCode().contentEquals("77983-5"))) {
          return true;
        }
      }
    }

    return false;
  }

  public static Boolean isNationalityObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && c.getSystem().contains(SNOMED_INFO_SCT_URL)
            && (c.getCode().contentEquals("186034007"))) {
          return true;
        }
      }
    }

    return false;
  }

  /**
   * The method updates the LaunchDetails.status field with the right PatientExecutionState which is
   * setup from KarProcessingData.actionStatus field.
   *
   * @param data
   * @param details
   */
  private static void setPatientStateInLaunchDetails(
      KarProcessingData data, LaunchDetails details) {

    PatientExecutionState state = new PatientExecutionState();

    // Set Trigger codes.
    List<BsaActionStatus> statuses = data.getActionStatusByType(ActionType.CHECK_TRIGGER_CODES);

    for (BsaActionStatus entry : statuses) {

      CheckTriggerCodeStatus ctcs = (CheckTriggerCodeStatus) entry;

      logger.info(" CTCS Matched Codes size");
      MatchTriggerStatus mts = new MatchTriggerStatus();

      mts.setActionId(ctcs.getActionId());
      mts.setJobStatus(getJobStatusForActionStatus(ctcs.getActionStatus()));
      mts.setTriggerMatchStatus(ctcs.getTriggerMatchStatus());
      mts.setMatchedCodes(ctcs.getMatchedCodes());
      state.setMatchTriggerStatus(mts);
    }

    EcaUtils.updateDetailStatus(details, state);
  }

  private static void removeDuplicatesAndUpdateData(
      Set<Resource> resources, String type, Map<String, List<String>> data) {

    if (data == null) {
      data = new HashMap<>();
    }

    if (type.equals(ResourceType.Observation.toString())) {
      logger.debug(" Found Observation ");
    }

    // Get he initial list of Ids
    List<String> dataIds = data.getOrDefault(type, new ArrayList<>());

    if (resources != null && !resources.isEmpty()) {

      resources.removeIf(
          resource -> {
            String resourceId = resource.getIdElement().getIdPart();
            if (dataIds.contains(resourceId)) {
              logger.info(
                  "Removing {} resource with Id {} since they are already added to the R4FhirData object",
                  type,
                  resourceId);
              return true;
            } else {
              dataIds.add(resourceId);
              return false;
            }
          });

      data.put(type, dataIds);
    }
  }

  private static JobStatus getJobStatusForActionStatus(BsaActionStatusType status) {

    if (status == BsaActionStatusType.COMPLETED) return JobStatus.COMPLETED;
    else if (status == BsaActionStatusType.SUSPENDED) return JobStatus.SUSPENDED;
    else if (status == BsaActionStatusType.FAILED) return JobStatus.ABORTED;
    else if (status == BsaActionStatusType.IN_PROGRESS) return JobStatus.IN_PROGRESS;
    else if (status == BsaActionStatusType.NOT_STARTED) return JobStatus.NOT_STARTED;
    else if (status == BsaActionStatusType.SCHEDULED) return JobStatus.SCHEDULED;
    else return JobStatus.ABORTED;
  }
}
