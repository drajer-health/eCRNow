package com.drajer.bsa.utils;

import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.eca.model.EcaUtils;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.MatchTriggerStatus;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Procedure;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
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

          addResourcesToR4FhirData(dr.getId(), data, r4FhirData, details, resources, dr.getType());
        }
      }

      addAdministrativeResources(null, data, r4FhirData, details, kd, act);
      addSecondaryResources(null, data, r4FhirData, details, kd, act);

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
      BsaAction act) {
    logger.info("BsaAction in addSecondaryResources:{}", act);

    Set<Resource> medications = kd.getResourcesByType(ResourceType.Medication.toString());
    addResourcesToR4FhirData(
        dataId, data, r4FhirData, details, medications, ResourceType.Medication.toString());

    Set<Resource> observations = kd.getResourcesByType(ResourceType.Observation.toString());
    addResourcesToR4FhirData(
        dataId, data, r4FhirData, details, observations, ResourceType.Observation.toString());
  }

  public static void addAdministrativeResources(
      String dataId,
      Bundle data,
      R4FhirData r4FhirData,
      LaunchDetails details,
      KarProcessingData kd,
      BsaAction act) {
    logger.info("BsaAction in addAdministrativeResources:{}", act);

    Set<Resource> practitioners = kd.getResourcesByType(ResourceType.Practitioner.toString());
    addResourcesToR4FhirData(
        dataId, data, r4FhirData, details, practitioners, ResourceType.Practitioner.toString());

    Set<Resource> locations = kd.getResourcesByType(ResourceType.Location.toString());
    addResourcesToR4FhirData(
        dataId, data, r4FhirData, details, locations, ResourceType.Location.toString());

    Set<Resource> orgs = kd.getResourcesByType(ResourceType.Organization.toString());
    addResourcesToR4FhirData(
        dataId, data, r4FhirData, details, orgs, ResourceType.Organization.toString());
  }

  public static void addResourcesToR4FhirData(
      String dataId,
      Bundle data,
      R4FhirData r4FhirData,
      LaunchDetails details,
      Set<Resource> resources,
      String type) {
    logger.info("Data id in addResourcesToR4FhirData:{}", dataId);

    if (resources != null && !resources.isEmpty()) {
      if (type.contentEquals(ResourceType.Patient.toString())) {

        logger.info(" Setting up the patient for R4FhirData ");
        Resource patient = resources.iterator().next();
        r4FhirData.setPatient((Patient) patient);
        details.setLaunchPatientId(patient.getIdElement().getIdPart());
        data.addEntry(new BundleEntryComponent().setResource(patient));
      } else if (type.contentEquals(ResourceType.Encounter.toString())) {

        logger.info(" Setting up the encounter for R4FhirData ");
        Resource encounter = resources.iterator().next();
        r4FhirData.setEncounter((Encounter) encounter);
        details.setEncounterId(encounter.getIdElement().getIdPart());
        data.addEntry(new BundleEntryComponent().setResource(encounter));
      } else if (type.contentEquals(ResourceType.Location.toString())) {

        logger.info(" Setting up the location for R4FhirData ");
        Resource location = resources.iterator().next();
        r4FhirData.setLocation((Location) location);
        data.addEntry(new BundleEntryComponent().setResource(location));
      } else if (type.contentEquals(ResourceType.Organization.toString())) {

        logger.info(" Setting up the organization for R4FhirData ");
        Resource organization = resources.iterator().next();
        r4FhirData.setOrganization((Organization) organization);
        data.addEntry(new BundleEntryComponent().setResource(organization));
      } else if (type.contentEquals(ResourceType.Practitioner.toString())) {

        logger.info(" Setting up the Practitioner for R4FhirData ");

        ArrayList<Practitioner> practitioners = new ArrayList<>();
        if (!resources.isEmpty()) {

          for (Resource r : resources) {
            practitioners.add((Practitioner) r);
            data.addEntry(new BundleEntryComponent().setResource(r));
          }
          r4FhirData.addPractitionersList(practitioners);
        }
      } else if (type.contentEquals(ResourceType.Condition.toString())) {

        logger.info(" Setting up the Conditions for R4FhirData ");
        ArrayList<Condition> conditionList = new ArrayList<>();
        ArrayList<Condition> encDiagList = new ArrayList<>();
        if (!resources.isEmpty()) {

          List<Condition> pregnancyConditions = new ArrayList<>();
          for (Resource r : resources) {

            Condition c = (Condition) r;
            conditionList.add(c);

            if (c.getCategoryFirstRep() != null
                && c.getCategoryFirstRep().getCodingFirstRep() != null
                && c.getCategoryFirstRep().getCodingFirstRep().getCode() != null
                && c.getCategoryFirstRep()
                    .getCodingFirstRep()
                    .getCode()
                    .contentEquals("encounter-diagnosis")) {
              encDiagList.add(c);
            }

            data.addEntry(new BundleEntryComponent().setResource(r));

            if (c.hasCode() && isPregnancyCondition(c.getCode())) {
              pregnancyConditions.add(c);
            }
          }
          r4FhirData.addPregnancyConditions(pregnancyConditions);
          r4FhirData.addConditions(conditionList);
          r4FhirData.addEncounterDiagnosisConditions(encDiagList);
        }
      } else if (type.contentEquals(ResourceType.Immunization.toString())) {

        logger.info(" Setting up the Immunization for R4FhirData ");
        ArrayList<Immunization> immList = new ArrayList<>();
        if (!resources.isEmpty()) {

          for (Resource r : resources) {
            immList.add((Immunization) r);
            data.addEntry(new BundleEntryComponent().setResource(r));
          }
          r4FhirData.setImmunizations(immList);
        }
      } else if (type.contentEquals(ResourceType.Procedure.toString())) {

        logger.info(" Setting up the Procedure for R4FhirData ");
        ArrayList<Procedure> procList = new ArrayList<>();
        if (!resources.isEmpty()) {

          for (Resource r : resources) {
            procList.add((Procedure) r);
            data.addEntry(new BundleEntryComponent().setResource(r));
          }
          r4FhirData.setProcedureList(procList);
        }
      } else if (type.contentEquals(ResourceType.MedicationRequest.toString())) {

        logger.info(" Setting up the MedicationRequest for R4FhirData ");
        ArrayList<MedicationRequest> medReqList = new ArrayList<>();
        if (!resources.isEmpty()) {

          for (Resource r : resources) {
            medReqList.add((MedicationRequest) r);
            data.addEntry(new BundleEntryComponent().setResource(r));
          }
          r4FhirData.setMedicationRequests(medReqList);
        }
      } else if (type.contentEquals(ResourceType.MedicationAdministration.toString())) {

        logger.info(" Setting up the MedicationAdministration for R4FhirData ");
        ArrayList<MedicationAdministration> medAdmList = new ArrayList<>();
        if (!resources.isEmpty()) {

          for (Resource r : resources) {
            medAdmList.add((MedicationAdministration) r);
            data.addEntry(new BundleEntryComponent().setResource(r));
          }
          r4FhirData.setMedicationAdministrations(medAdmList);
        }
      } else if (type.contentEquals(ResourceType.MedicationStatement.toString())) {

        logger.info(" Setting up the MedicationStatement for R4FhirData ");
        ArrayList<MedicationStatement> medStatementList = new ArrayList<>();
        if (!resources.isEmpty()) {

          for (Resource r : resources) {
            medStatementList.add((MedicationStatement) r);
            data.addEntry(new BundleEntryComponent().setResource(r));
          }
          r4FhirData.setMedications(medStatementList);
        }
      } else if (type.contentEquals(ResourceType.Medication.toString())) {

        logger.info(" Setting up the Medication for R4FhirData ");
        ArrayList<Medication> medList = new ArrayList<>();
        if (!resources.isEmpty()) {

          for (Resource r : resources) {
            medList.add((Medication) r);
            data.addEntry(new BundleEntryComponent().setResource(r));
          }
          r4FhirData.addMedicationList(medList);
        }
      } else if (type.contentEquals(ResourceType.ServiceRequest.toString())) {

        logger.info(" Setting up the ServiceRequest for R4FhirData ");
        ArrayList<ServiceRequest> servReqList = new ArrayList<>();
        if (!resources.isEmpty()) {

          for (Resource r : resources) {
            servReqList.add((ServiceRequest) r);
            data.addEntry(new BundleEntryComponent().setResource(r));
          }
          r4FhirData.addServiceRequests(servReqList);
        }
      } else if (type.contentEquals(ResourceType.Observation.toString())) {

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

        logger.info(" Setting up the Vital Signs for R4FhirData ");
        Set<Resource> vitalObs =
            ReportGenerationUtils.filterObservationsByCategory(
                resources, ObservationCategory.VITALSIGNS.toCode());
        ArrayList<Observation> vitalObsList = new ArrayList<>();
        if (vitalObs != null && !vitalObs.isEmpty()) {

          for (Resource r : labObs) {
            vitalObsList.add((Observation) r);
            data.addEntry(new BundleEntryComponent().setResource(r));
          }
          r4FhirData.setVitalObs(vitalObsList);
        }

        logger.info(" Setting up the SocialHistory for R4FhirData ");
        Set<Resource> socObs =
            ReportGenerationUtils.filterObservationsByCategory(
                resources, ObservationCategory.SOCIALHISTORY.toCode());
        ArrayList<Observation> socObsList = new ArrayList<>();
        if (socObs != null && !socObs.isEmpty()) {

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

          for (Resource r : socObs) {
            Observation sochisObs = (Observation) r;
            socObsList.add(sochisObs);
            data.addEntry(new BundleEntryComponent().setResource(r));

            if (sochisObs.hasCode() && isOccupationObservation(sochisObs.getCode())) {

              logger.info(" Found Occupation History Observation ");
              occObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isTravelObservation(sochisObs.getCode())) {

              logger.info(" Found Occupation History Observation ");
              travelObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isPregnancyObservation(sochisObs.getCode())) {

              logger.info(" Found Occupation History Observation ");
              pregnancyObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isPregnancyStatusObservation(sochisObs.getCode())) {

              logger.info(" Found Pregnancy Status Observation ");
              pregnancyStatusObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isLastMenstrualPeriodObservation(sochisObs.getCode())) {

              logger.info(" Found LMP Observation ");
              lmpObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isPostPartumStatusObservation(sochisObs.getCode())) {

              logger.info(" Found Post Partum Status Observation ");
              postPartumObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isPregnancyOutcomeObservation(sochisObs.getCode())) {

              logger.info(" Found Pregnancy Outcome Observation ");
              pregnancyOutcomeObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isHomelessObservation(sochisObs.getCode())) {

              logger.info(" Found Homeless Observation ");
              homelessObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isDisabilityObservation(sochisObs.getCode())) {

              logger.info(" Found Disability Observation ");
              disabilityObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isVaccineCredObservation(sochisObs.getCode())) {

              logger.info(" Found Vaccine Credential Observation ");
              vaccineCredObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isResidencyObservation(sochisObs.getCode())) {

              logger.info(" Found Residency Info Observation ");
              residencyObs.add(sochisObs);
            }

            if (sochisObs.hasCode() && isNationalityObservation(sochisObs.getCode())) {

              logger.info(" Found Nationality Observation ");
              nationalityObs.add(sochisObs);
            }
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
        }

      } else if (type.contentEquals(ResourceType.DiagnosticReport.toString())) {

        logger.info(" Setting up the Diagnostic Report for R4FhirData ");
        ArrayList<DiagnosticReport> diagReportList = new ArrayList<>();
        if (!resources.isEmpty()) {

          for (Resource r : resources) {
            diagReportList.add((DiagnosticReport) r);
            data.addEntry(new BundleEntryComponent().setResource(r));
          }
          r4FhirData.addDiagReports(diagReportList);
        }
      } else {
        logger.error(
            " Unknow Resource Type {} passed for report creation. Data from resource type : {} will not be used",
            type,
            type);
      }
    } else {
      logger.warn(" Cannot add null resources for type {}", type);
    }
  }

  public static Boolean isOccupationObservation(CodeableConcept cd) {

    if (cd != null && cd.hasCoding()) {

      List<Coding> cds = cd.getCoding();

      for (Coding c : cds) {

        if (c.hasCode()
            && c.hasSystem()
            && ((c.getCode().contentEquals("11295-3") && c.getSystem().contains("http://loinc.org"))
                || (c.getCode().contentEquals("11341-5")
                    && c.getSystem().contains("http://loinc.org"))
                || (c.getCode().contentEquals("21843-8")
                    && c.getSystem().contains("http://loinc.org"))
                || (c.getCode().contentEquals("74165-2")
                    && c.getSystem().contains("http://loinc.org"))
                || (c.getCode().contentEquals("224362002")
                    && c.getSystem().contains("http://snomed.info/sct"))
                || (c.getCode().contentEquals("364703007")
                    && c.getSystem().contains("http://snomed.info/sct")))) {
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
            && ((c.getCode().contentEquals("29762-2") && c.getSystem().contains("http://loinc.org"))
                || (c.getCode().contentEquals("161085007")
                    && c.getSystem().contains("http://snomed.info/sct"))
                || (c.getCode().contentEquals("161086008")
                    && c.getSystem().contains("http://snomed.info/sct"))
                || (c.getCode().contentEquals("420008001")
                    && c.getSystem().contains("http://snomed.info/sct"))
                || (c.getCode().contentEquals("46521000175102")
                    && c.getSystem().contains("http://snomed.info/sct"))
                || (c.getCode().contentEquals("34831000175105")
                    && c.getSystem().contains("http://snomed.info/sct"))
                || (c.getCode().contentEquals("443846001")
                    && c.getSystem().contains("http://snomed.info/sct")))) {
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
            && ((c.getCode().contentEquals("90767-5") && c.getSystem().contains("http://loinc.org"))
                || (c.getCode().contentEquals("146799005")
                    && c.getSystem().contains("http://snomed.info/sct"))
                || (c.getCode().contentEquals("60001007")
                    && c.getSystem().contains("http://snomed.info/sct"))
                || (c.getCode().contentEquals("77386006")
                    && c.getSystem().contains("http://snomed.info/sct")))) {
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
            && c.getSystem().contains("http://snomed.info/sct")
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
            && ((c.getCode().contentEquals("82810-3")
                && c.getSystem().contains("http://loinc.org")))) {
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
            && ((c.getCode().contentEquals("8665-2")
                && c.getSystem().contains("http://loinc.org")))) {
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
            && ((c.getCode().contentEquals("249197004")
                && c.getSystem().contains("http://snomed.info/sct")))) {
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
            && c.getSystem().contains("http://snomed.info/sct")
            && ((c.getCode().contentEquals("17369002")
                || c.getCode().contentEquals("21243004")
                || c.getCode().contentEquals("237364002")
                || c.getCode().contentEquals("237364002")
                || c.getCode().contentEquals("237364002")))) {
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
            && c.getSystem().contains("http://snomed.info/sct")
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
            && c.getSystem().contains("http://loinc.org")
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
            && c.getSystem().contains("http://loinc.org")
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
            && c.getSystem().contains("http://loinc.org")
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
            && c.getSystem().contains("http://snomed.info/sct")
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

  private static JobStatus getJobStatusForActionStatus(BsaActionStatusType status) {

    if (status == BsaActionStatusType.COMPLETED) return JobStatus.COMPLETED;
    else if (status == BsaActionStatusType.ABORTED) return JobStatus.ABORTED;
    else if (status == BsaActionStatusType.FAILED) return JobStatus.ABORTED;
    else if (status == BsaActionStatusType.IN_PROGRESS) return JobStatus.IN_PROGRESS;
    else if (status == BsaActionStatusType.NOT_STARTED) return JobStatus.NOT_STARTED;
    else if (status == BsaActionStatusType.SCHEDULED) return JobStatus.SCHEDULED;
    else return JobStatus.ABORTED;
  }
}
