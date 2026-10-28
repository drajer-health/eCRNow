package com.drajer.bsa.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.fhirecr.FhirGeneratorConstants;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import com.drajer.test.util.Utility;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Goal;
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
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.Specimen;
import org.hl7.fhir.r4.model.codesystems.ObservationCategory;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class R3ToR2DataConverterUtilsTest {

  @Spy @InjectMocks R3ToR2DataConverterUtils r3ToR2DataConverterUtils;

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

    FhirContext fhirContext = FhirContext.forR4();
    Observation socialHistory =
        fhirContext
            .newJsonParser()
            .parseResource(
                Observation.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/R4/Observation/SocialHistory.json"));

    Observation vitalSigns =
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
      // ASSERTIONS - Verify method behavior
      assertNotNull(r4FhirData);
      assertNotNull(bundle);
      assertNotNull(uniqueResourceIdsByType);
    }

    // Final assertions after all iterations
    assertNotNull(r4FhirData);
    assertNotNull(r4FhirData.getPatient()); // Verify patient was set
    assertNotNull(r4FhirData.getEncounter()); // Verify encounter was set
    assertTrue(resources.isEmpty()); // After removing all, set should be empty
  }

  @Test
  public void isOccupationObservation() {
    CodeableConcept codeableConcept = new CodeableConcept();
    List<Coding> codingList = new ArrayList<>();
    codingList.add(new Coding().setCode("224362002").setSystem("http://snomed.info/sct"));
    codingList.add(new Coding().setCode("364703007").setSystem("http://snomed.info/sct"));

    Iterator<Coding> iterator = codingList.iterator();
    while (iterator.hasNext()) {
      iterator.next();
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
      iterator.next();
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

  // =====================================================================================
  // New tests added for more coverage. The existing tests above are unchanged.
  // =====================================================================================

  private static final String LOINC = "http://loinc.org";
  private static final String SNOMED = "http://snomed.info/sct";
  private static final String WRONG_SYSTEM = "http://wrong.example.org";

  @Test
  public void privateConstructor_canBeInvokedByReflection() throws Exception {
    Constructor<R3ToR2DataConverterUtils> constructor =
        R3ToR2DataConverterUtils.class.getDeclaredConstructor();
    assertTrue(Modifier.isPrivate(constructor.getModifiers()));
    constructor.setAccessible(true);
    assertNotNull(constructor.newInstance());
  }

  // ---------------- convertKarProcessingDataForCdaGeneration ----------------

  @Test
  public void convertKarProcessingData_nullKarProcessingData_returnsEmptyResult() {
    Pair<R4FhirData, LaunchDetails> result =
        R3ToR2DataConverterUtils.convertKarProcessingDataForCdaGeneration(
            null, mock(BsaAction.class));

    assertNotNull(result);
    assertNotNull(result.getValue0().getData());
    assertTrue(result.getValue0().getData().getEntry().isEmpty());
    assertNull(result.getValue0().getPatient());
  }

  @Test
  public void convertKarProcessingData_withKarAndPhm_setsDetailsAndPatient() {
    KarProcessingData kd = mockKarProcessingData();

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    lenient().when(kar.getRctcOid()).thenReturn("rctc-oid");
    lenient().when(kar.getRctcVersion()).thenReturn("rctc-version");
    lenient().when(kd.getKar()).thenReturn(kar);

    PublicHealthMessage phm = mock(PublicHealthMessage.class);
    lenient().when(phm.getSubmittedVersionNumber()).thenReturn(2);
    lenient().when(kd.getPhm()).thenReturn(phm);

    // One status per BsaActionStatusType value (plus null) -> covers getJobStatusForActionStatus
    lenient()
        .when(kd.getActionStatusByType(ActionType.CHECK_TRIGGER_CODES))
        .thenReturn(statusesForEveryActionStatusType());

    DataRequirement patientReq = new DataRequirement();
    patientReq.setId("patient-req");
    patientReq.setType(ResourceType.Patient.toString());
    DataRequirement missingReq = new DataRequirement();
    missingReq.setId("missing-req");
    missingReq.setType(ResourceType.Condition.toString());

    BsaAction act = mock(BsaAction.class);
    lenient().when(act.getInputData()).thenReturn(List.of(patientReq, missingReq));
    lenient().when(act.getRelatedDataId("patient-req")).thenReturn("rel-patient");
    lenient()
        .when(kd.getDataForId("patient-req", "rel-patient"))
        .thenReturn(new HashSet<>(Set.of(patient)));
    // No data found for this requirement -> the "resources == null" path
    lenient().when(kd.getDataForId("missing-req", (String) null)).thenReturn(null);

    Pair<R4FhirData, LaunchDetails> result =
        R3ToR2DataConverterUtils.convertKarProcessingDataForCdaGeneration(kd, act);

    LaunchDetails details = result.getValue1();
    assertEquals("rctc-oid", details.getRctcOid());
    assertEquals("rctc-version", details.getRctcVersion());
    assertEquals(Integer.valueOf(3), Integer.valueOf(details.getVersionNumber()));
    assertEquals("http://ehr.example.org/fhir", details.getEhrServerURL());
    assertNotNull(details.getStatus());
    assertSame(patient, result.getValue0().getPatient());
  }

  @Test
  public void convertKarProcessingData_withoutKarAndPhm_usesVersionOne() {
    KarProcessingData kd = mockKarProcessingData();
    BsaAction act = mock(BsaAction.class);
    lenient().when(act.getInputData()).thenReturn(new ArrayList<>());

    Pair<R4FhirData, LaunchDetails> result =
        R3ToR2DataConverterUtils.convertKarProcessingDataForCdaGeneration(kd, act);

    LaunchDetails details = result.getValue1();
    assertEquals(Integer.valueOf(1), Integer.valueOf(details.getVersionNumber()));
    assertEquals(new LaunchDetails().getRctcOid(), details.getRctcOid());
    assertEquals("patient-1|encounter-1", details.getSetId());
    // Context encounter is null -> no organization or location is set
    assertNull(result.getValue0().getOrganization());
    assertNull(result.getValue0().getLocation());
  }

  // ---------------- setLocationAndServiceProviderOrganization ----------------

  @Test
  public void setLocationAndServiceProvider_findsOrganizationAndSecondLocation() {
    KarProcessingData kd = mock(KarProcessingData.class);
    Encounter enc = new Encounter();
    enc.setServiceProvider(new Reference("Organization/org-1"));
    enc.addLocation().setLocation(new Reference("Location/missing-location"));
    enc.addLocation().setLocation(new Reference("Location/loc-1"));
    lenient().when(kd.getContextEncounter()).thenReturn(enc);
    lenient().when(kd.getResourceById("org-1", ResourceType.Organization)).thenReturn(organization);
    lenient().when(kd.getResourceById("loc-1", ResourceType.Location)).thenReturn(location);

    R4FhirData data = new R4FhirData();
    R3ToR2DataConverterUtils.setLocationAndServiceProviderOrganization(kd, data);

    assertSame(organization, data.getOrganization());
    assertSame(location, data.getLocation());
  }

  @Test
  public void setLocationAndServiceProvider_organizationAndLocationNotFound() {
    KarProcessingData kd = mock(KarProcessingData.class);
    Encounter enc = new Encounter();
    enc.setServiceProvider(new Reference("Organization/unknown-org"));
    enc.addLocation().setLocation(new Reference("Location/unknown-location"));
    lenient().when(kd.getContextEncounter()).thenReturn(enc);

    R4FhirData data = new R4FhirData();
    R3ToR2DataConverterUtils.setLocationAndServiceProviderOrganization(kd, data);

    assertNull(data.getOrganization());
    assertNull(data.getLocation());
  }

  @Test
  public void setLocationAndServiceProvider_encounterWithoutServiceProviderOrLocation() {
    KarProcessingData kd = mock(KarProcessingData.class);
    lenient().when(kd.getContextEncounter()).thenReturn(new Encounter());

    R4FhirData data = new R4FhirData();
    R3ToR2DataConverterUtils.setLocationAndServiceProviderOrganization(kd, data);

    assertNull(data.getOrganization());
    assertNull(data.getLocation());
  }

  // ---------------- addAdministrativeResources / addSecondaryResources ----------------

  @Test
  public void addAdministrativeAndSecondaryResources_addResourcesByType() {
    KarProcessingData kd = mock(KarProcessingData.class);
    lenient()
        .when(kd.getResourcesByType(ResourceType.Practitioner.toString()))
        .thenReturn(new HashSet<>(Set.of(practitioner)));
    lenient()
        .when(kd.getResourcesByType(ResourceType.Medication.toString()))
        .thenReturn(new HashSet<>(Set.of(medication)));

    Bundle bundle = new Bundle();
    R4FhirData data = new R4FhirData();
    Map<String, List<String>> ids = new HashMap<>();

    R3ToR2DataConverterUtils.addAdministrativeResources(
        null, bundle, data, launchDetails, kd, mock(BsaAction.class), ids);
    R3ToR2DataConverterUtils.addSecondaryResources(
        null, bundle, data, launchDetails, kd, mock(BsaAction.class), ids);

    assertEquals(1, data.getPractitionersList().size());
    assertEquals(1, data.getMedicationList().size());
    assertEquals(2, bundle.getEntry().size());
  }

  // ---------------- addResourcesToR4FhirData ----------------

  @Test
  public void addResources_nullResources_addsNothing() {
    Bundle bundle = new Bundle();
    R4FhirData data = new R4FhirData();

    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id", bundle, data, launchDetails, null, ResourceType.Patient.toString(), new HashMap<>());

    assertNull(data.getPatient());
    assertTrue(bundle.getEntry().isEmpty());
  }

  @Test
  public void addResources_emptyResources_addsNothing() {
    Bundle bundle = new Bundle();
    R4FhirData data = new R4FhirData();

    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        bundle,
        data,
        launchDetails,
        new HashSet<>(),
        ResourceType.Observation.toString(),
        new HashMap<>());

    assertTrue(bundle.getEntry().isEmpty());
  }

  @Test
  public void addResources_nullIdMap_stillAddsResource() {
    Bundle bundle = new Bundle();
    R4FhirData data = new R4FhirData();

    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        bundle,
        data,
        launchDetails,
        new HashSet<>(Set.of(patient)),
        ResourceType.Patient.toString(),
        null);

    assertSame(patient, data.getPatient());
  }

  @Test
  public void addResources_duplicateResource_isRemovedAndNotAddedAgain() {
    Bundle bundle = new Bundle();
    R4FhirData data = new R4FhirData();
    Map<String, List<String>> ids = new HashMap<>();
    ids.put(
        ResourceType.Patient.toString(),
        new ArrayList<>(Arrays.asList(patient.getIdElement().getIdPart())));

    Set<Resource> resources = new HashSet<>(Set.of(patient));
    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id", bundle, data, launchDetails, resources, ResourceType.Patient.toString(), ids);

    assertTrue(resources.isEmpty());
    assertNull(data.getPatient());
    assertTrue(bundle.getEntry().isEmpty());
  }

  @Test
  public void addResources_locations_setsFirstLocationAndAddsAll() {
    Location second = new Location();
    second.setId("loc-2");
    Bundle bundle = new Bundle();
    R4FhirData data = new R4FhirData();

    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        bundle,
        data,
        launchDetails,
        new HashSet<>(Set.of(location, second)),
        ResourceType.Location.toString(),
        new HashMap<>());

    assertNotNull(data.getLocation());
    // first location is added once, then every location again in the loop
    assertEquals(3, bundle.getEntry().size());
  }

  @Test
  public void addResources_organizations_setsOrganizationAndAddsAll() {
    Organization second = new Organization();
    second.setId("org-2");
    Bundle bundle = new Bundle();
    R4FhirData data = new R4FhirData();

    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        bundle,
        data,
        launchDetails,
        new HashSet<>(Set.of(organization, second)),
        ResourceType.Organization.toString(),
        new HashMap<>());

    assertNotNull(data.getOrganization());
    assertEquals(2, bundle.getEntry().size());
  }

  @Test
  public void addResources_practitioner() {
    R4FhirData data = new R4FhirData();
    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        new Bundle(),
        data,
        launchDetails,
        new HashSet<>(Set.of(practitioner)),
        ResourceType.Practitioner.toString(),
        new HashMap<>());

    assertEquals(1, data.getPractitionersList().size());
  }

  @Test
  public void addResources_conditions_splitsEncounterDiagnosisAndPregnancy() {
    Condition encDiagPregnancy = new Condition();
    encDiagPregnancy.setId("cond-1");
    encDiagPregnancy.addCategory().addCoding().setCode("encounter-diagnosis");
    encDiagPregnancy.setCode(cc(SNOMED, "77386006"));

    Condition plain = new Condition();
    plain.setId("cond-2");

    Condition otherCategory = new Condition();
    otherCategory.setId("cond-3");
    otherCategory.addCategory().addCoding().setCode("problem-list-item");
    otherCategory.setCode(cc(SNOMED, "38341003"));

    R4FhirData data = new R4FhirData();
    Bundle bundle = new Bundle();
    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        bundle,
        data,
        launchDetails,
        new HashSet<>(Set.of(encDiagPregnancy, plain, otherCategory)),
        ResourceType.Condition.toString(),
        new HashMap<>());

    assertEquals(3, data.getConditions().size());
    assertEquals(1, data.getEncounterDiagnosisConditions().size());
    assertEquals(1, data.getPregnancyConditions().size());
    assertEquals(3, bundle.getEntry().size());
  }

  @Test
  public void addResources_medicationAndSpecimen() {
    Specimen specimen = new Specimen();
    specimen.setId("spec-1");
    R4FhirData data = new R4FhirData();

    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        new Bundle(),
        data,
        launchDetails,
        new HashSet<>(Set.of(medication)),
        ResourceType.Medication.toString(),
        new HashMap<>());
    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        new Bundle(),
        data,
        launchDetails,
        new HashSet<>(Set.of(specimen)),
        ResourceType.Specimen.toString(),
        new HashMap<>());

    assertEquals(1, data.getMedicationList().size());
    assertEquals(1, data.getSpecimenList().size());
  }

  @Test
  public void addResources_unknownType_isIgnored() {
    Goal goal = new Goal();
    goal.setId("goal-1");
    Bundle bundle = new Bundle();

    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        bundle,
        new R4FhirData(),
        launchDetails,
        new HashSet<>(Set.of(goal)),
        ResourceType.Goal.toString(),
        new HashMap<>());

    assertTrue(bundle.getEntry().isEmpty());
  }

  @Test
  public void addResources_observations_splitIntoLabVitalAndSocialHistory() {
    Set<Resource> observations = new HashSet<>();
    observations.add(categorisedObs("lab-1", ObservationCategory.LABORATORY.toCode()));
    observations.add(categorisedObs("vital-1", ObservationCategory.VITALSIGNS.toCode()));
    observations.add(socialObs("occ-1", LOINC, "11295-3"));
    observations.add(socialObs("travel-1", SNOMED, "161085007"));
    observations.add(socialObs("preg-1", LOINC, "90767-5"));
    observations.add(socialObs("pregstatus-1", LOINC, "82810-3"));
    observations.add(socialObs("lmp-1", LOINC, CdaGeneratorConstants.LMP_CODE));
    observations.add(socialObs("postpartum-1", SNOMED, "249197004"));
    observations.add(socialObs("outcome-1", SNOMED, "17369002"));
    observations.add(socialObs("homeless-1", SNOMED, "32911000"));
    observations.add(socialObs("disability-1", LOINC, "69856-3"));
    observations.add(socialObs("vaccine-1", LOINC, "11370-4"));
    observations.add(socialObs("residency-1", LOINC, "77983-5"));
    observations.add(socialObs("nationality-1", SNOMED, "186034007"));
    observations.add(
        socialObs("intention-1", LOINC, CdaGeneratorConstants.PREGNANCY_INTENTION_CODE));
    Observation noCode = new Observation();
    noCode.setId("nocode-1");
    observations.add(noCode);

    R4FhirData data = new R4FhirData();
    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        new Bundle(),
        data,
        launchDetails,
        observations,
        ResourceType.Observation.toString(),
        new HashMap<>());

    assertEquals(1, data.getLabResults().size());
    assertEquals(1, data.getVitalObs().size());
    assertEquals(1, data.getOccupationObs().size());
    assertEquals(1, data.getTravelObs().size());
    assertEquals(1, data.getPregnancyObs().size());
    assertEquals(1, data.getPregnancyStatusObs().size());
    assertEquals(1, data.getLmpObs().size());
    assertEquals(1, data.getPostPartumObs().size());
    assertEquals(1, data.getPregnancyOutcomeObs().size());
    assertEquals(1, data.getHomelessObs().size());
    assertEquals(1, data.getDisabilityObs().size());
    assertEquals(1, data.getVaccineCredObs().size());
    assertEquals(1, data.getResidencyObs().size());
    assertEquals(1, data.getNationalityObs().size());
  }

  @Test
  public void addResources_observationsWithoutLabOrVital() {
    R4FhirData data = new R4FhirData();
    R3ToR2DataConverterUtils.addResourcesToR4FhirData(
        "id",
        new Bundle(),
        data,
        launchDetails,
        new HashSet<>(Set.of(socialObs("only-social", LOINC, "11295-3"))),
        ResourceType.Observation.toString(),
        new HashMap<>());

    assertTrue(data.getLabResults() == null || data.getLabResults().isEmpty());
    assertTrue(data.getVitalObs() == null || data.getVitalObs().isEmpty());
    assertEquals(1, data.getOccupationObs().size());
  }

  // ---------------- is*Observation / isPregnancyCondition ----------------

  @Test
  public void isOccupationObservation_allCodes() {
    assertDetects(
        R3ToR2DataConverterUtils::isOccupationObservation,
        LOINC,
        "11295-3",
        "11341-5",
        "21843-8",
        "74165-2");
    assertDetects(
        R3ToR2DataConverterUtils::isOccupationObservation, SNOMED, "224362002", "364703007");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isOccupationObservation);
  }

  @Test
  public void isTravelObservation_allCodes() {
    assertDetects(
        R3ToR2DataConverterUtils::isTravelObservation,
        SNOMED,
        "161085007",
        "161086008",
        "420008001",
        "46521000175102",
        "34831000175105",
        "443846001");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isTravelObservation);
  }

  @Test
  public void isPregnancyObservation_allCodes() {
    assertDetects(R3ToR2DataConverterUtils::isPregnancyObservation, LOINC, "90767-5");
    assertDetects(
        R3ToR2DataConverterUtils::isPregnancyObservation,
        SNOMED,
        "146799005",
        "60001007",
        "77386006");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isPregnancyObservation);
  }

  @Test
  public void isPregnancyCondition_allCodes() {
    assertDetects(
        R3ToR2DataConverterUtils::isPregnancyCondition,
        SNOMED,
        "77386006",
        "146799005",
        "60001007");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isPregnancyCondition);
  }

  @Test
  public void isPregnancyStatusObservation_allCodes() {
    assertDetects(R3ToR2DataConverterUtils::isPregnancyStatusObservation, LOINC, "82810-3");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isPregnancyStatusObservation);
  }

  @Test
  public void isPregnancyIntentionObservation_allCodes() {
    assertDetects(
        R3ToR2DataConverterUtils::isPregnancyIntentionObservation,
        LOINC,
        CdaGeneratorConstants.PREGNANCY_INTENTION_CODE);
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isPregnancyIntentionObservation);
  }

  @Test
  public void isLastMenstrualPeriodObservation_allCodes() {
    assertDetects(
        R3ToR2DataConverterUtils::isLastMenstrualPeriodObservation,
        LOINC,
        CdaGeneratorConstants.LMP_CODE);
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isLastMenstrualPeriodObservation);
  }

  @Test
  public void isPostPartumStatusObservation_allCodes() {
    assertDetects(R3ToR2DataConverterUtils::isPostPartumStatusObservation, SNOMED, "249197004");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isPostPartumStatusObservation);
  }

  @Test
  public void isPregnancyOutcomeObservation_allCodes() {
    assertDetects(
        R3ToR2DataConverterUtils::isPregnancyOutcomeObservation,
        SNOMED,
        "17369002",
        "21243004",
        "237364002");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isPregnancyOutcomeObservation);
  }

  @Test
  public void isHomelessObservation_allCodes() {
    assertDetects(R3ToR2DataConverterUtils::isHomelessObservation, SNOMED, "32911000", "105526001");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isHomelessObservation);
  }

  @Test
  public void isDisabilityObservation_allCodes() {
    assertDetects(
        R3ToR2DataConverterUtils::isDisabilityObservation,
        LOINC,
        "69856-3",
        "69857-1",
        "69858-9",
        "69859-7",
        "69860-5",
        "69861-3");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isDisabilityObservation);
  }

  @Test
  public void isVaccineCredObservation_allCodes() {
    assertDetects(R3ToR2DataConverterUtils::isVaccineCredObservation, LOINC, "11370-4");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isVaccineCredObservation);
  }

  @Test
  public void isResidencyObservation_allCodes() {
    assertDetects(R3ToR2DataConverterUtils::isResidencyObservation, LOINC, "77983-5");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isResidencyObservation);
  }

  @Test
  public void isNationalityObservation_allCodes() {
    assertDetects(R3ToR2DataConverterUtils::isNationalityObservation, SNOMED, "186034007");
    assertRejectsInvalidInput(R3ToR2DataConverterUtils::isNationalityObservation);
  }

  // ---------------- helpers for the new tests ----------------

  private static KarProcessingData mockKarProcessingData() {
    KarProcessingData kd = mock(KarProcessingData.class);

    NotificationContext nc = new NotificationContext();
    nc.setFhirServerBaseUrl("http://ehr.example.org/fhir");
    nc.setPatientId("patient-1");
    nc.setNotificationResourceId("encounter-1");
    lenient().when(kd.getNotificationContext()).thenReturn(nc);
    lenient().when(kd.getHealthcareSetting()).thenReturn(mock(HealthcareSetting.class));
    return kd;
  }

  private static List<BsaActionStatus> statusesForEveryActionStatusType() {
    List<BsaActionStatus> statuses = new ArrayList<>();
    List<BsaActionStatusType> types = new ArrayList<>(List.of(BsaActionStatusType.values()));
    types.add(null); // falls into the final "else" -> ABORTED
    for (BsaActionStatusType type : types) {
      CheckTriggerCodeStatus status = new CheckTriggerCodeStatus();
      status.setActionId("action-" + type);
      status.setActionStatus(type);
      status.setTriggerMatchStatus(true);
      status.setMatchedCodes(new ArrayList<>());
      statuses.add(status);
    }
    return statuses;
  }

  private static CodeableConcept cc(String system, String code) {
    CodeableConcept concept = new CodeableConcept();
    concept.addCoding().setSystem(system).setCode(code);
    return concept;
  }

  private static Observation categorisedObs(String id, String category) {
    Observation obs = new Observation();
    obs.setId(id);
    obs.addCategory()
        .addCoding()
        .setSystem(FhirGeneratorConstants.HL7_OBSERVATION_CATEGORY)
        .setCode(category);
    return obs;
  }

  private static Observation socialObs(String id, String system, String code) {
    Observation obs = new Observation();
    obs.setId(id);
    obs.setCode(cc(system, code));
    return obs;
  }

  /** Each code is detected with the right system, and rejected with a wrong system. */
  private static void assertDetects(
      Function<CodeableConcept, Boolean> check, String system, String... codes) {
    for (String code : codes) {
      assertTrue(code + " should match", check.apply(cc(system, code)));
      assertFalse(code + " with a wrong system", check.apply(cc(WRONG_SYSTEM, code)));
    }
  }

  /** Null, empty and incomplete codings, and unknown codes, are all rejected. */
  private static void assertRejectsInvalidInput(Function<CodeableConcept, Boolean> check) {
    assertFalse(check.apply(null));
    assertFalse(check.apply(new CodeableConcept()));

    CodeableConcept noCode = new CodeableConcept();
    noCode.addCoding().setSystem(LOINC);
    assertFalse(check.apply(noCode));

    CodeableConcept noSystem = new CodeableConcept();
    noSystem.addCoding().setCode("11295-3");
    assertFalse(check.apply(noSystem));

    assertFalse(check.apply(cc(LOINC, "00000-0")));
    assertFalse(check.apply(cc(SNOMED, "000000000")));
  }
}
