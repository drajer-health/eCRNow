package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.sof.model.R4FhirData;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;

public class BaseGeneratorTest {

  public static final Logger logger = LoggerFactory.getLogger(BaseGeneratorTest.class);
  public static final FhirContext fhirContext = FhirContext.forR4();

  public static final String PATIENT_RES_FILENAME = "CdaTestData/patient/Patient_resource.json";

  R4FhirData r4FhirData;

  @Before
  public void setupTestCase() {
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    r4FhirData = new R4FhirData();
  }

  public R4FhirData createR4Resource(R4FhirData r4FhirData, Bundle bundle) {

    List<Observation> observations = new ArrayList<>();
    List<MedicationStatement> medicationStatements = new ArrayList<>();
    List<Condition> conditions = new ArrayList<>();
    List<MedicationRequest> medicationRequests = new ArrayList<>();
    List<Medication> medications = new ArrayList<>();
    List<MedicationAdministration> medicationAdministrations = new ArrayList<>();
    List<Immunization> immunizations = new ArrayList<>();

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

    return r4FhirData;
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

  public Bundle loadBundleFromFile(String filename) {
    try (InputStream in = new ClassPathResource(filename).getInputStream()) {
      return fhirContext.newJsonParser().parseResource(Bundle.class, in);
    } catch (Exception e) {
      return null;
    }
  }

  public void assertXmlEquals(String expectedXml, String actualXml) {
    expectedXml = StringUtils.normalizeSpace(expectedXml).trim();
    actualXml = StringUtils.normalizeSpace(actualXml).trim();
    assertEquals(expectedXml, actualXml);
  }
}
