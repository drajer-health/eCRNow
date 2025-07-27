package com.drajer.cdafromdstu2;

import static org.junit.Assert.assertEquals;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.resource.*;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Condition;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticOrder;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticReport;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.model.dstu2.resource.Immunization;
import ca.uhn.fhir.model.dstu2.resource.Location;
import ca.uhn.fhir.model.dstu2.resource.MedicationAdministration;
import ca.uhn.fhir.model.dstu2.resource.MedicationStatement;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import ca.uhn.fhir.model.dstu2.resource.Organization;
import ca.uhn.fhir.model.dstu2.resource.Patient;
import ca.uhn.fhir.model.dstu2.resource.Practitioner;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.InputStream;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.dstu2.model.*;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.junit.Before;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.util.ReflectionTestUtils;

@PowerMockIgnore({"javax.crypto.*"})
public class BaseGenerator {

  protected Patient patient;
  protected Bundle bundle;

  public static final Logger logger =
      LoggerFactory.getLogger(com.drajer.cdafromr4.BaseGeneratorTest.class);
  public static final FhirContext fhirContext = FhirContext.forDstu2();
  public static final String EXCEPTION_READING_FILE = "Exception Reading File";
  protected static final String PATIENT_CDA_FILE = "CdaDstuTestData/Cda/Patient/Patient.xml";
  public static final String LAUNCH_DETAILS_FILENAME =
      "CdaDstuTestData/LaunchDetails/LaunchDetails.json";
  public static final String BUNDLE_RES_FILENAME = "CdaTestData/Bundle/Bundle.json";
  public static final String PATIENT_RES_FILENAME = "CdaTestData/patient/Patient_resource.json";
  static final String ECIR_DETAILS_FILENAME = "R4/Misc/eicr.json";
  public static final String XML_FOR_II_USING_GUID =
      "<id root=\"b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9\"/>";

  public List<Patient.Contact> contactList;

  Dstu2FhirData dstu2FhirDataForPatient;
  Dstu2FhirData dstu2FhirDataForBundle;
  Dstu2FhirData dstu2FhirData;
  LaunchDetails launchDetails = loadLaunchDetailsFromFile();
  Eicr eicr = loadEicrDetailsFromFile();

  @Before
  public void setupTestCase() {
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    patient = (Patient) loadResourceDataFromFile(Patient.class, PATIENT_RES_FILENAME);
    // bundle = loadResourceDataFromFile(Bundle.class, BUNDLE_RES_FILENAME);
    bundle = new Bundle();
    dstu2FhirDataForPatient = new Dstu2FhirData();
    dstu2FhirDataForBundle = new Dstu2FhirData();
    dstu2FhirDataForPatient.setPatient(patient);
    dstu2FhirDataForBundle.setPatient(patient);
    dstu2FhirDataForBundle.setData(bundle);
  }

  public LaunchDetails loadLaunchDetailsFromFile() {
    ReflectionTestUtils.setField(AESEncryption.class, "secretKey", "123");
    return TestUtils.readFileContents(
        LAUNCH_DETAILS_FILENAME, new TypeReference<LaunchDetails>() {});
  }

  public Eicr loadEicrDetailsFromFile() {

    return TestUtils.readFileContents(ECIR_DETAILS_FILENAME, new TypeReference<Eicr>() {});
  }

  public <T extends IBaseResource> T loadResourceDataFromFile(
      Class<T> resourceType, String filename) {

    try (InputStream in = new ClassPathResource(filename).getInputStream()) {

      return fhirContext.newJsonParser().parseResource(resourceType, in);

    } catch (Exception e) {
      logger.error(EXCEPTION_READING_FILE, e);
    }
    return null;
  }

  public void assertXmlEquals(String expectedXml, String actualXml) {
    expectedXml = StringUtils.normalizeSpace(expectedXml).trim();
    actualXml = StringUtils.normalizeSpace(actualXml).trim();
    assertEquals(expectedXml, actualXml);
  }

  public Bundle loadBundleFromFile(String filename) {
    try (InputStream in = new ClassPathResource(filename).getInputStream()) {
      return fhirContext.newJsonParser().parseResource(Bundle.class, in);
    } catch (Exception e) {
      logger.error(EXCEPTION_READING_FILE, e);
      return null;
    }
  }

  public static void addResourceToFhirData(Bundle bundle, Dstu2FhirData data) {
    if (bundle != null && bundle.getEntry() != null) {

      List<ca.uhn.fhir.model.dstu2.resource.Observation> observations = new ArrayList<>();
      List<ca.uhn.fhir.model.dstu2.resource.Condition> conditions = new ArrayList<>();
      List<ca.uhn.fhir.model.dstu2.resource.DiagnosticReport> diagnosticReports = new ArrayList<>();
      List<ca.uhn.fhir.model.dstu2.resource.DiagnosticOrder> diagnosticOrders = new ArrayList<>();
      List<ca.uhn.fhir.model.dstu2.resource.MedicationAdministration>
          medicationAdministrationsList = new ArrayList<>();
      List<ca.uhn.fhir.model.dstu2.resource.MedicationStatement> medicationStatements =
          new ArrayList<>();
      List<ca.uhn.fhir.model.dstu2.resource.Immunization> immunizations = new ArrayList<>();

      for (Bundle.Entry entry : bundle.getEntry()) {
        if (entry.getResource() instanceof Patient) {
          data.setPatient((Patient) entry.getResource());

        } else if (entry.getResource() instanceof Practitioner) {
          data.setPractitioner((Practitioner) entry.getResource());

        } else if (entry.getResource() instanceof Encounter) {
          data.setEncounter((Encounter) entry.getResource());

        } else if (entry.getResource() instanceof Location) {
          data.setLocation((Location) entry.getResource());

        } else if (entry.getResource() instanceof Organization) {
          data.setOrganization((Organization) entry.getResource());

        } else if (entry.getResource() instanceof Observation) {
          observations.add((Observation) entry.getResource());

        } else if (entry.getResource() instanceof Condition) {
          conditions.add((Condition) entry.getResource());
        } else if (entry.getResource() instanceof DiagnosticReport) {
          diagnosticReports.add((DiagnosticReport) entry.getResource());

        } else if (entry.getResource() instanceof DiagnosticOrder) {
          diagnosticOrders.add((DiagnosticOrder) entry.getResource());

        } else if (entry.getResource() instanceof MedicationAdministration) {
          medicationAdministrationsList.add((MedicationAdministration) entry.getResource());

        } else if (entry.getResource() instanceof MedicationStatement) {
          medicationStatements.add((MedicationStatement) entry.getResource());

        } else if (entry.getResource() instanceof Immunization) {
          immunizations.add((Immunization) entry.getResource());
        }
      }
      data.setImmunizations(immunizations);
      data.setDiagReports(diagnosticReports);
      data.setDiagOrders(diagnosticOrders);
      data.setConditions(conditions);
      data.setMedicationAdministrations(medicationAdministrationsList);
      data.setMedications(medicationStatements);
      // Apply filters once after collecting all observations and conditions
      ObservationFactory.applyAllFilters(data, observations, conditions);
    }
  }
}
