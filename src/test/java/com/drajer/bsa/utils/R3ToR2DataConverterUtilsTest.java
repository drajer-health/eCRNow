package com.drajer.bsa.utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import com.drajer.test.util.Utility;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
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
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Procedure;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class R3ToR2DataConverterUtilsTest {

  @Spy @InjectMocks R3ToR2DataConverterUtils r3ToR2DataConverterUtils;

  private static final Logger logger = LoggerFactory.getLogger(R3ToR2DataConverterUtilsTest.class);

  ClassLoader classLoader = R3ToR2DataConverterUtilsTest.class.getClassLoader();

  private static Medication medication;

  private static Observation observation;

  private static Patient patient;

  private static Practitioner practitioner;

  private static Location location;

  private static Organization organization;

  private static Encounter encounter;

  private static Condition condition;

  private static Immunization immunization;

  private static Procedure procedure;

  private static MedicationRequest medicationRequest;

  private static ServiceRequest serviceRequest;

  private static LaunchDetails launchDetails;

  private static DiagnosticReport diagnosticReport;

  private static MedicationStatement medicationStatement;

  private static MedicationAdministration medicationAdministration;

  private static R4FhirData r4FhirData = new R4FhirData();

  @Before
  public void setUp() throws Exception {
    ReflectionTestUtils.setField(AESEncryption.class, "secretKey", "123");

    FhirContext fhirContext = FhirContext.forR4();
    medication =
        fhirContext
            .newJsonParser()
            .parseResource(
                Medication.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Medication/Medication.json"));
    observation =
        fhirContext
            .newJsonParser()
            .parseResource(
                Observation.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Observation/ObservationResource.json"));
    patient =
        fhirContext
            .newJsonParser()
            .parseResource(
                Patient.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream("/R4/Patient/Patient.json"));
    encounter =
        fhirContext
            .newJsonParser()
            .parseResource(
                Encounter.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Encounter/Encounter_97953900.json"));
    condition =
        fhirContext
            .newJsonParser()
            .parseResource(
                Condition.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Condition/Condition.json"));
    immunization =
        fhirContext
            .newJsonParser()
            .parseResource(
                Immunization.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Immunization/ImmunizationResource.json"));
    procedure =
        fhirContext
            .newJsonParser()
            .parseResource(
                Procedure.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Procedure/Procedure.json"));
    medicationRequest =
        fhirContext
            .newJsonParser()
            .parseResource(
                MedicationRequest.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Medication/MedicationRequestResource.json"));
    practitioner =
        fhirContext
            .newJsonParser()
            .parseResource(
                Practitioner.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Practitioner/Practitioner_11817978.json"));
    location =
        fhirContext
            .newJsonParser()
            .parseResource(
                Location.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Location/Location.json"));
    organization =
        fhirContext
            .newJsonParser()
            .parseResource(
                Organization.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Organization/Organization.json"));
    serviceRequest =
        fhirContext
            .newJsonParser()
            .parseResource(
                ServiceRequest.class,
                ReportGenerationUtils.class.getResourceAsStream(
                    "/R4/ServiceRequest/ServiceRequest.json"));
    diagnosticReport =
        fhirContext
            .newJsonParser()
            .parseResource(
                DiagnosticReport.class,
                ReportGenerationUtils.class.getResourceAsStream(
                    "/R4/DiagnosticReport/DiagnosticReport.json"));
    medicationStatement =
        fhirContext
            .newJsonParser()
            .parseResource(
                MedicationStatement.class,
                ReportGenerationUtils.class.getResourceAsStream(
                    "/R4/Medication/MedicationStatementResource.json"));
    medicationAdministration =
        fhirContext
            .newJsonParser()
            .parseResource(
                MedicationAdministration.class,
                ReportGenerationUtils.class.getResourceAsStream(
                    "/R4/Medication/MedicationAdministrationResource.json"));
    launchDetails =
        (LaunchDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/LaunchDetails/LaunchDetails.json", LaunchDetails.class);
    launchDetails.setLastUpdated(new Date());
    r4FhirData = new R4FhirData();
  }

  @Test
  public void testConvertKarProcessingDataForCdaGeneration() throws IOException {
    KarProcessingData karProcessingData = Utility.getKarProcessingData();

    List<BsaActionStatus> bsaActionStatusList = new ArrayList<>();
    CheckTriggerCodeStatus codeStatus = mock(CheckTriggerCodeStatus.class);
    codeStatus.setActionId("73247");
    codeStatus.setActionType(ActionType.CHECK_TRIGGER_CODES);

    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setValueSet("valueSet");
    matchedTriggerCodes.setValueSetVersion("v5.0.0");
    List<MatchedTriggerCodes> codesList = new ArrayList<>();
    codesList.add(matchedTriggerCodes);
    codeStatus.setMatchedCodes(codesList);
    codeStatus.setTriggerMatchStatus(true);
    bsaActionStatusList.add(codeStatus);

    Observation socialHistory = new Observation();
    FhirContext fhirContext = FhirContext.forR4();
    socialHistory =
        fhirContext
            .newJsonParser()
            .parseResource(
                Observation.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Observation/SocialHistory.json"));

    Observation vitalSigns = new Observation();
    vitalSigns =
        fhirContext
            .newJsonParser()
            .parseResource(
                Observation.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Observation/VitalSigns.json"));

    karProcessingData.addResourceByType(ResourceType.Patient, patient);
    karProcessingData.addResourceByType(ResourceType.Practitioner, practitioner);
    karProcessingData.addResourceByType(ResourceType.Observation, observation);
    karProcessingData.addResourceByType(ResourceType.Observation, socialHistory);
    karProcessingData.addResourceByType(ResourceType.Observation, vitalSigns);
    karProcessingData.addResourceByType(ResourceType.Medication, medication);
    karProcessingData.addResourceByType(ResourceType.Organization, organization);
    karProcessingData.addResourceByType(ResourceType.Location, location);

    BsaAction bsaAction = Utility.getBsaAction();

    Pair<R4FhirData, LaunchDetails> cdaGeneration =
        r3ToR2DataConverterUtils.convertKarProcessingDataForCdaGeneration(
            karProcessingData, bsaAction);
    assertNotNull(cdaGeneration);
  }

  @Test
  public void addResourcesToR4FhirData() {
    Bundle bundle = new Bundle();
    Set<Resource> resources = new HashSet<>();

    Map<String, List<String>> uniqueResourceIdsByType = new HashMap<>();

    List<Resource> resourceList = new ArrayList<>();
    resourceList.add(patient);
    resourceList.add(condition);
    resourceList.add(encounter);
    resourceList.add(procedure);
    resourceList.add(medicationRequest);
    resourceList.add(serviceRequest);
    resourceList.add(immunization);
    resourceList.add(diagnosticReport);
    resourceList.add(medicationAdministration);
    resourceList.add(medicationStatement);
    resourceList.add(medicationRequest);
    for (Resource resource : resourceList) {
      resources.add(resource);
      String typeAsString = resource.toString().substring(22, resource.toString().indexOf('@'));
      r3ToR2DataConverterUtils.addResourcesToR4FhirData(
          "98796",
          bundle,
          r4FhirData,
          launchDetails,
          resources,
          typeAsString,
          uniqueResourceIdsByType);
      resources.remove(resource);
    }
  }

  @Test
  public void isOccupationObservation() {
    CodeableConcept codeableConcept = new CodeableConcept();
    List<Coding> codingList = new ArrayList<>();
    codingList.add(new Coding().setCode("224362002").setSystem("http://snomed.info/sct"));
    codingList.add(new Coding().setCode("364703007").setSystem("http://snomed.info/sct"));

    Iterator<Coding> iterator = codingList.iterator();
    while (iterator.hasNext()) {
      Coding coding = iterator.next();
      codeableConcept.setCoding(codingList);
      Boolean isTravelObservation =
          r3ToR2DataConverterUtils.isOccupationObservation(codeableConcept);
      assertTrue(isTravelObservation);
      iterator.remove();
    }
  }

  @Test
  public void isTravelObservation() {
    CodeableConcept codeableConcept = new CodeableConcept();
    List<Coding> codingList = new ArrayList<>();

    codingList.add(new Coding().setCode("161085007").setSystem("http://snomed.info/sct"));
    codingList.add(new Coding().setCode("161086008").setSystem("http://snomed.info/sct"));
    codingList.add(new Coding().setCode("420008001").setSystem("http://snomed.info/sct"));
    codingList.add(new Coding().setCode("46521000175102").setSystem("http://snomed.info/sct"));
    codingList.add(new Coding().setCode("34831000175105").setSystem("http://snomed.info/sct"));
    codingList.add(new Coding().setCode("224362002").setSystem("http://snomed.info/sct"));
    codingList.add(new Coding().setCode("443846001").setSystem("http://snomed.info/sct"));

    Iterator<Coding> iterator = codingList.iterator();
    while (iterator.hasNext()) {
      Coding coding = iterator.next();
      codeableConcept.setCoding(codingList);
      Boolean isTravelObservation = r3ToR2DataConverterUtils.isTravelObservation(codeableConcept);
      assertTrue(isTravelObservation);
      iterator.remove();
    }
  }

  @Test
  public void isPregnancyObservation() {
    CodeableConcept codeableConcept = new CodeableConcept();
    List<Coding> codingList = new ArrayList<>();
    codingList.add(new Coding().setCode("77386006").setSystem("http://snomed.info/sct"));
    codeableConcept.setCoding(codingList);
    Boolean isPregnancyObservation =
        r3ToR2DataConverterUtils.isPregnancyObservation(codeableConcept);
    assertTrue(isPregnancyObservation);
  }

  @Test
  public void testIsPregnancyCondition() {
    CodeableConcept codeableConcept = new CodeableConcept();
    Boolean isPregnancyObservation = r3ToR2DataConverterUtils.isPregnancyCondition(codeableConcept);
    assertFalse(isPregnancyObservation);
  }

  @Test
  public void testIsPregnancyObservation() {
    CodeableConcept codeableConcept = new CodeableConcept();
    Boolean isPregnancyObservation =
        r3ToR2DataConverterUtils.isPregnancyObservation(codeableConcept);
    assertFalse(isPregnancyObservation);
  }

  @Test
  public void testIsTravelObservation() {
    CodeableConcept codeableConcept = new CodeableConcept();
    Boolean isTravelObservation = r3ToR2DataConverterUtils.isTravelObservation(codeableConcept);
    assertFalse(isTravelObservation);
  }
}
