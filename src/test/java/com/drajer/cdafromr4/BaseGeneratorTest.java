package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.eca.model.MatchTriggerStatus;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.simulator.ContentDataSimulator;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.junit.Before;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.util.ReflectionTestUtils;

@PowerMockIgnore({"javax.crypto.*"})
public class BaseGeneratorTest {

  public static final Logger logger = LoggerFactory.getLogger(BaseGeneratorTest.class);
  public static final FhirContext fhirContext = FhirContext.forR4();
  public static final String EXCEPTION_READING_FILE = "Exception Reading File";

  public static final String LAUNCH_DETAILS_FILENAME =
      "CdaTestData/LaunchDetails/LaunchDetails.json";
  public static final String PATIENT_RES_FILENAME = "CdaTestData/patient/Patient_resource.json";
  static final String ECIR_DETAILS_FILENAME = "R4/Misc/eicr.json";
  public static final String XML_FOR_II_USING_GUID =
      "<id root=\"b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9\"/>";
  R4FhirData r4FhirData;
  LaunchDetails launchDetails = loadLaunchDetailsFromFile();
  Eicr eicr = loadEicrDetailsFromFile();

  @Before
  public void setupTestCase() {
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    r4FhirData = new R4FhirData();
  }

  public LaunchDetails loadLaunchDetailsFromFile() {
    ReflectionTestUtils.setField(AESEncryption.class, "secretKey", "123");
    return TestUtils.readFileContents(
        LAUNCH_DETAILS_FILENAME, new TypeReference<LaunchDetails>() {});
  }

  public Eicr loadEicrDetailsFromFile() {

    return TestUtils.readFileContents(ECIR_DETAILS_FILENAME, new TypeReference<Eicr>() {});
  }

  public Object loadResourceDataFromFile(Class resourceType, String filename) {

    try (InputStream in = new ClassPathResource(filename).getInputStream()) {

      return fhirContext.newJsonParser().parseResource(resourceType, in);

    } catch (Exception e) {
      logger.error(EXCEPTION_READING_FILE, e);
    }
    return null;
  }

  public R4FhirData createR4Resource(R4FhirData r4FhirData, Bundle bundle) {

    List<Observation> observations = new ArrayList<>();
    List<MedicationStatement> medicationStatements = new ArrayList<>();
    List<Condition> conditions = new ArrayList<>();
    List<MedicationRequest> medicationRequests = new ArrayList<>();
    List<Medication> medications = new ArrayList<>();
    List<MedicationAdministration> medicationAdministrations = new ArrayList<>();
    List<Immunization> immunizations = new ArrayList<>();
    List<Practitioner> practitionerList = new ArrayList<>();
    List<ServiceRequest> serviceRequests = new ArrayList<>();
    List<DiagnosticReport> diagnosticReports = new ArrayList<>();
    List<BundleEntryComponent> entries = bundle.getEntry();

    entries.forEach(
        ent -> {
          ResourceType resourceType = ent.getResource().getResourceType();

          switch (resourceType) {
            case Patient:
              Patient patient = (Patient) ent.getResource();
              r4FhirData.setPatient(patient);
              logger.info(" Patient = {}", patient.getNameFirstRep().getFamily());
              break;
            case Practitioner:
              Practitioner practitioner = (Practitioner) ent.getResource();
              r4FhirData.setPractitioner(practitioner);
              practitionerList.add(practitioner);
              break;
            case Location:
              Location location = (Location) ent.getResource();
              r4FhirData.setLocation(location);
              break;
            case Encounter:
              Encounter encounter = (Encounter) ent.getResource();
              r4FhirData.setEncounter(encounter);
              break;
            case MedicationStatement:
              medicationStatements.add((MedicationStatement) ent.getResource());
              break;
            case MedicationRequest:
              medicationRequests.add((MedicationRequest) ent.getResource());
              break;
            case Medication:
              medications.add((Medication) ent.getResource());
              break;
            case Immunization:
              immunizations.add((Immunization) ent.getResource());
              break;
            case MedicationAdministration:
              medicationAdministrations.add((MedicationAdministration) ent.getResource());
              break;
            case Condition:
              conditions.add((Condition) ent.getResource());
              break;
            case ServiceRequest:
              serviceRequests.add((ServiceRequest) ent.getResource());
              break;

            case DiagnosticReport:
              diagnosticReports.add((DiagnosticReport) ent.getResource());
              break;
          }
        });

    r4FhirData.setConditions(conditions);
    r4FhirData.setImmunizations(immunizations);
    r4FhirData.setLabResults(observations);
    r4FhirData.setMedications(medicationStatements);
    r4FhirData.setMedicationRequests(medicationRequests);
    r4FhirData.setMedicationList(medications);
    r4FhirData.setMedicationAdministrations(medicationAdministrations);
    r4FhirData.setDiagReports(diagnosticReports);
    r4FhirData.setServiceRequests(serviceRequests);
    r4FhirData.setPractitionersList(practitionerList);
    return r4FhirData;
  }

  public List<DiagnosticReport> getDiagnosticReport(String filename) {
    Bundle b = loadBundleFromFile(filename);

    if (b != null) {
      logger.info(" Found Bundle ");

      List<DiagnosticReport> diagnosticReports = new ArrayList();
      List<BundleEntryComponent> entries = b.getEntry();

      for (BundleEntryComponent ent : entries) {

        if (ent.getResource().getResourceType() == ResourceType.DiagnosticReport) {
          diagnosticReports.add((DiagnosticReport) ent.getResource());
        }
      }
      return diagnosticReports;

    } else {
      logger.info("unable to find bundle");
    }
    return null;
  }

  public List<Condition> getPregnancyConditions(String filename) {
    Bundle b = loadBundleFromFile(filename);

    if (b != null) {
      logger.info(" Found Bundle ");

      List<Condition> pregnancyConditions = new ArrayList();
      List<BundleEntryComponent> entries = b.getEntry();

      for (BundleEntryComponent ent : entries) {

        if (ent.getResource().getResourceType() == ResourceType.Condition) {
          pregnancyConditions.add((Condition) ent.getResource());
        }
      }
      return pregnancyConditions;

    } else {
      logger.info("unable to find bundle");
    }
    return null;
  }

  public Patient getPatientDetails(String filename) {
    Bundle b = loadBundleFromFile(filename);

    if (b != null) {
      logger.info(" Found Bundle ");

      Patient patient = new Patient();
      List<BundleEntryComponent> entries = b.getEntry();

      for (BundleEntryComponent ent : entries) {

        if (ent.getResource().getResourceType() == ResourceType.Patient) {
          patient = (Patient) ent.getResource();
        }
      }
      return patient;

    } else {
      logger.info("unable to find bundle");
    }
    return null;
  }

  public Patient getPatientData() {
    return getPatientDetails(PATIENT_RES_FILENAME);
  }

  public List<Observation> getObs(String filename) {
    Bundle b = loadBundleFromFile(filename);

    if (b != null) {
      logger.info(" Found Bundle ");

      List<Observation> Obs = new ArrayList();
      List<BundleEntryComponent> entries = b.getEntry();

      for (BundleEntryComponent ent : entries) {

        if (ent.getResource().getResourceType() == ResourceType.Observation) {
          Obs.add((Observation) ent.getResource());
        }
      }
      return Obs;

    } else {
      logger.info("unable to find bundle");
    }
    return null;
  }

  public R4FhirData createResourceData(String filename) {
    logger.info(" Running the test ");

    Bundle b = loadBundleFromFile(filename);
    r4FhirData = new R4FhirData();

    if (b != null) {
      logger.info(" Found Bundle ");

      return createR4Resource(r4FhirData, b);
    } else {
      logger.info("error in bundle");
      return r4FhirData;
    }
  }

  public Bundle loadBundleFromFile(String filename) {
    try (InputStream in = new ClassPathResource(filename).getInputStream()) {
      return fhirContext.newJsonParser().parseResource(Bundle.class, in);
    } catch (Exception e) {
      ContentDataSimulator.logger.error(EXCEPTION_READING_FILE, e);
      return null;
    }
  }

  public List<Condition> getEncounterDiagnosisConditions(String filename) {
    Bundle b = loadBundleFromFile(filename);
    List<Condition> encounterDiagnosisConditions = new ArrayList<>();

    if (b != null) {
      BaseGeneratorTest.logger.info("Found Bundle");

      for (BundleEntryComponent ent : b.getEntry()) {

        if (ent.getResource().getResourceType() == ResourceType.Condition) {
          encounterDiagnosisConditions.add((Condition) ent.getResource());
        }
      }

      return encounterDiagnosisConditions;

    } else {
      BaseGeneratorTest.logger.info("Unable to find bundle");
      return encounterDiagnosisConditions;
    }
  }

  public void assertXmlEquals(String expectedXml, String actualXml) {
    expectedXml = StringUtils.normalizeSpace(expectedXml).trim();
    actualXml = StringUtils.normalizeSpace(actualXml).trim();
    assertEquals(expectedXml, actualXml);
  }

  public PatientExecutionState createPatientExecutionState(String matchedPath, String matchedCode) {
    PatientExecutionState patientExecutionState = new PatientExecutionState();
    patientExecutionState.setMatchTriggerStatus(new MatchTriggerStatus());
    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setMatchedPath(matchedPath);
    Set<String> codes = new HashSet<>();
    codes.add(matchedCode);
    matchedTriggerCodes.setMatchedCodes(codes);
    patientExecutionState
        .getMatchTriggerStatus()
        .setMatchedCodes(Collections.singletonList(matchedTriggerCodes));
    return patientExecutionState;
  }
}
