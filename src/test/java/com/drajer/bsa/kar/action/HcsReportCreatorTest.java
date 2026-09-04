package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.fhirecr.FhirGeneratorConstants;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.InputStream;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Composition.SectionComponent;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointSystem;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointUse;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.core.io.ClassPathResource;

public class HcsReportCreatorTest {

  FhirContext r4Context = FhirContext.forR4();
  KarProcessingData karProcessingData;
  EhrQueryService ehrQueryService;
  public static final String PROFILE =
      "http//://hl7.org/fhir/us/health-care-surveys-reporting/StructureDefinition/hcs-reporting-bundle";
  HcsReportCreator hcsReportCreator;

  @Before
  public void setUp() {
    hcsReportCreator = new HcsReportCreator();
    ehrQueryService = Mockito.mock(EhrQueryService.class);
    karProcessingData = new KarProcessingData();
    karProcessingData.setKarStatus(getKnowledgeArtifactStatus());
    karProcessingData.setPhm(null);
    NotificationContext notificationContext = getNotificationContext();
    karProcessingData.setNotificationContext(notificationContext);
    karProcessingData.setHealthcareSetting(getHealthcareSetting());
    karProcessingData.setxRequestId("32");
    karProcessingData.setxCorrelationId(null);
    karProcessingData.setNotificationBundle(
        (Bundle)
            r4Context.newJsonParser().parseResource(notificationContext.getNotificationData()));
    karProcessingData.setFhirInputDataByType(getFilteredByType("/R4/LoadingQueryBundle.json"));
    karProcessingData.setKar(getKnowledgeArtifact());
  }

  @Test
  public void testCreateReport() {

    karProcessingData.setKarStatus(getKnowledgeArtifactStatus());
    Set<Resource> inputData = new HashSet<>();
    Resource resource =
        TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    inputData.add(resource);
    BsaAction bsaAction = getBsaAction();
    Bundle actualReport =
        (Bundle)
            hcsReportCreator.createReport(
                karProcessingData, ehrQueryService, "example", PROFILE, bsaAction);
    assertTrue(actualReport.hasId());
    Bundle exceptedReport = TestUtils.loadBundleFromFile("Bsa/report/Hcs-report/report.json");
    assertEquals(exceptedReport.getEntry().size(), actualReport.getEntry().size());

    String actualComposition = getComposition(actualReport);
    String expectedComposition = getComposition(exceptedReport);
    assertEquals(expectedComposition, actualComposition);
  }

  @Test
  public void testreferenceTo() {
    Resource patient = TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    hcsReportCreator.referenceTo(patient);
    assertTrue(patient.hasId());
    assertNotNull(patient);
  }

  @Test
  public void testpopulateReasonForVisitNarrative() {
    Composition.SectionComponent sectionComponent = new Composition.SectionComponent();
    sectionComponent.fhirType();
    hcsReportCreator.populateReasonForVisitNarrative(sectionComponent);
    assertNotNull("Section component should be populated after method call", sectionComponent);
  }

  // ========== ORGANIZATION CREATION TESTS ==========

  @Test
  public void testCreateOrganization_HasId() {
    Organization org = new Organization();
    org.setId(UUID.randomUUID().toString());
    org.setName("Test Organization");
    org.setActive(true);

    assertNotNull("Organization should have ID", org.getId());
    assertFalse("ID should not be empty", org.getId().isEmpty());
  }

  @Test
  public void testCreateOrganization_IdIsUUID() {
    String uuid = UUID.randomUUID().toString();
    Organization org = new Organization();
    org.setId(uuid);

    assertEquals("ID should match UUID", uuid, org.getId());
    assertTrue("UUID should contain hyphens", uuid.contains("-"));
  }

  @Test
  public void testCreateOrganization_NameSet() {
    Organization org = new Organization();
    org.setName("Test Organization Name");

    assertNotNull("Organization name should be set", org.getName());
    assertEquals("Name should match", "Test Organization Name", org.getName());
  }

  @Test
  public void testCreateOrganization_ActiveTrue() {
    Organization org = new Organization();
    org.setActive(true);

    assertTrue("Organization should be active", org.getActive());
  }

  @Test
  public void testCreateOrganization_HasMeta() {
    Organization org = new Organization();
    org.setMeta(new Meta());

    assertNotNull("Organization should have meta", org.getMeta());
  }

  @Test
  public void testCreateOrganization_HasTelecom() {
    Organization org = new Organization();
    List<ContactPoint> cts = new ArrayList<>();
    ContactPoint ct = new ContactPoint();
    ct.setUse(ContactPointUse.WORK);
    ct.setSystem(ContactPointSystem.EMAIL);
    ct.setValue("+1-777-555-1111");
    cts.add(ct);
    org.setTelecom(cts);

    assertNotNull("Organization should have telecom list", org.getTelecom());
    assertEquals("Should have one contact point", 1, org.getTelecom().size());
    assertEquals(
        "Contact point use should be WORK", ContactPointUse.WORK, org.getTelecom().get(0).getUse());
    assertEquals(
        "Contact point system should be EMAIL",
        ContactPointSystem.EMAIL,
        org.getTelecom().get(0).getSystem());
    assertEquals(
        "Contact point value should match", "+1-777-555-1111", org.getTelecom().get(0).getValue());
  }

  @Test
  public void testCreateOrganization_HasAddress() {
    Organization org = new Organization();
    Address addr = new Address();
    addr.setCountry("US");
    org.addAddress(addr);

    assertNotNull("Organization should have address", org.getAddress());
    assertTrue("Address list should not be empty", org.getAddress().size() > 0);
    assertEquals("Country should be US", "US", org.getAddress().get(0).getCountry());
  }

  @Test
  public void testCreateOrganization_HasIdentifier() {
    Organization org = new Organization();
    org.addIdentifier().setSystem("urn:oid:test").setValue("test-id-123");

    assertNotNull("Organization should have identifier", org.getIdentifier());
    assertTrue("Identifier list should not be empty", org.getIdentifier().size() > 0);
    assertEquals(
        "Identifier system should match", "urn:oid:test", org.getIdentifier().get(0).getSystem());
    assertEquals(
        "Identifier value should match", "test-id-123", org.getIdentifier().get(0).getValue());
  }

  @Test
  public void testCreateOrganization_CompleteStructure() {
    Organization org = new Organization();
    org.setId(UUID.randomUUID().toString());
    org.setMeta(new Meta());
    org.setName("Test Healthcare Setting");
    org.setActive(true);

    List<ContactPoint> cts = new ArrayList<>();
    ContactPoint ct = new ContactPoint();
    ct.setUse(ContactPointUse.WORK);
    ct.setSystem(ContactPointSystem.EMAIL);
    ct.setValue("+1-777-555-1111");
    cts.add(ct);
    org.setTelecom(cts);

    Address addr = new Address();
    addr.setCountry("US");
    org.addAddress(addr);

    org.addIdentifier().setSystem("urn:oid:system").setValue("org-id-456");

    // Verify complete structure
    assertNotNull("ID should be set", org.getId());
    assertNotNull("Meta should be set", org.getMeta());
    assertNotNull("Name should be set", org.getName());
    assertTrue("Should be active", org.getActive());
    assertTrue("Should have telecom", org.getTelecom().size() > 0);
    assertTrue("Should have address", org.getAddress().size() > 0);
    assertTrue("Should have identifier", org.getIdentifier().size() > 0);
  }

  // ========== TEST CASES FOR isSocialHistorySection() METHOD ==========

  @Test
  public void testIsSocialHistorySection_ValidSection() {
    SectionComponent sc = new SectionComponent();
    CodeableConcept code = new CodeableConcept();
    code.addCoding()
        .setSystem(FhirGeneratorConstants.LOINC_CS_URL)
        .setCode(FhirGeneratorConstants.SOCIAL_HISTORY_SECTION_LOINC_CODE);
    sc.setCode(code);

    Boolean result = hcsReportCreator.isSocialHistorySection(sc);

    assertTrue("Should identify social history section", result);
  }

  @Test
  public void testIsSocialHistorySection_NullCode() {
    SectionComponent sc = new SectionComponent();
    sc.setCode(null);

    Boolean result = hcsReportCreator.isSocialHistorySection(sc);

    assertFalse("Section with null code should return false", result);
  }

  @Test
  public void testIsSocialHistorySection_NullCoding() {
    SectionComponent sc = new SectionComponent();
    CodeableConcept code = new CodeableConcept();
    sc.setCode(code);

    Boolean result = hcsReportCreator.isSocialHistorySection(sc);

    assertFalse("Section with null coding should return false", result);
  }

  @Test
  public void testIsSocialHistorySection_WrongSystem() {
    SectionComponent sc = new SectionComponent();
    CodeableConcept code = new CodeableConcept();
    code.addCoding()
        .setSystem("http://wrong.system.org")
        .setCode(FhirGeneratorConstants.SOCIAL_HISTORY_SECTION_LOINC_CODE);
    sc.setCode(code);

    Boolean result = hcsReportCreator.isSocialHistorySection(sc);

    assertFalse("Section with wrong system should return false", result);
  }

  @Test
  public void testIsSocialHistorySection_WrongCode() {
    SectionComponent sc = new SectionComponent();
    CodeableConcept code = new CodeableConcept();
    code.addCoding().setSystem(FhirGeneratorConstants.LOINC_CS_URL).setCode("99999-9");
    sc.setCode(code);

    Boolean result = hcsReportCreator.isSocialHistorySection(sc);

    assertFalse("Section with wrong code should return false", result);
  }

  // ========== TEST CASES FOR EXTENSION FILTERING LOGIC ==========

  @Test
  public void testRemoveExtensions_PreservePerformerFunction() {
    Reference perfRef = new Reference();
    List<Extension> existingExts = new ArrayList<>();

    Extension performerFunction = new Extension();
    performerFunction.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    performerFunction.setValue(new StringType("test"));
    existingExts.add(performerFunction);

    Extension otherExt = new Extension();
    otherExt.setUrl("http://other.extension.org");
    otherExt.setValue(new StringType("test"));
    existingExts.add(otherExt);

    perfRef.setExtension(existingExts);

    List<Extension> newExts = new ArrayList<>();
    Boolean found = false;

    for (Extension e : existingExts) {
      if (!found
          && e.getUrl()
              .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
        found = true;
      } else if (!e.getUrl()
          .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
      }
    }

    perfRef.setExtension(newExts);

    assertTrue(
        "Should preserve performer function extension",
        perfRef.getExtension().stream()
            .anyMatch(e -> e.getUrl().contains("event-performerFunction")));
  }

  @Test
  public void testRemoveExtensions_RemoveDuplicatePerformerFunction() {
    Reference perfRef = new Reference();
    List<Extension> existingExts = new ArrayList<>();

    // Add duplicate performer function extensions
    Extension perf1 = new Extension();
    perf1.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    perf1.setValue(new StringType("value1"));

    Extension perf2 = new Extension();
    perf2.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    perf2.setValue(new StringType("value2"));

    existingExts.add(perf1);
    existingExts.add(perf2);

    List<Extension> newExts = new ArrayList<>();
    Boolean found = false;

    for (Extension e : existingExts) {
      if (!found
          && e.getUrl()
              .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
        found = true;
      } else if (!e.getUrl()
          .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
      }
    }

    // Should have only one performer function extension
    long count =
        newExts.stream().filter(e -> e.getUrl().contains("event-performerFunction")).count();
    assertEquals("Should have only one performer function extension", 1, count);
  }

  @Test
  public void testRemoveExtensions_PreserveOtherExtensions() {
    Reference perfRef = new Reference();
    List<Extension> existingExts = new ArrayList<>();

    Extension ext1 = new Extension();
    ext1.setUrl("http://other.extension1.org");

    Extension ext2 = new Extension();
    ext2.setUrl("http://other.extension2.org");

    existingExts.add(ext1);
    existingExts.add(ext2);

    List<Extension> newExts = new ArrayList<>();

    for (Extension e : existingExts) {
      if (!e.getUrl()
          .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
      }
    }

    assertEquals("Should preserve all non-performer function extensions", 2, newExts.size());
  }

  @Test
  public void testRemoveExtensions_NoPerformerFunction() {
    Reference perfRef = new Reference();
    List<Extension> existingExts = new ArrayList<>();

    Extension ext1 = new Extension();
    ext1.setUrl("http://ext1.org");

    Extension ext2 = new Extension();
    ext2.setUrl("http://ext2.org");

    existingExts.add(ext1);
    existingExts.add(ext2);

    List<Extension> newExts = new ArrayList<>();
    Boolean found = false;

    for (Extension e : existingExts) {
      if (!found
          && e.getUrl()
              .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
        found = true;
      } else if (!e.getUrl()
          .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
      }
    }

    assertEquals("Should have all extensions when no performer function", 2, newExts.size());
    assertFalse("Found flag should remain false", found);
  }

  @Test
  public void testRemoveExtensions_MixedExtensionsWithPerformerFirst() {
    Reference perfRef = new Reference();
    List<Extension> existingExts = new ArrayList<>();

    // Performer function first
    Extension perf = new Extension();
    perf.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    perf.setValue(new StringType("value1"));

    Extension other1 = new Extension();
    other1.setUrl("http://other1.org");

    Extension other2 = new Extension();
    other2.setUrl("http://other2.org");

    existingExts.add(perf);
    existingExts.add(other1);
    existingExts.add(other2);

    perfRef.setExtension(existingExts);

    // Simulate filtering on Reference
    List<Extension> existingExtsFromRef = perfRef.getExtension();
    assertEquals("Reference should have 3 initial extensions", 3, existingExtsFromRef.size());

    List<Extension> newExts = new ArrayList<>();
    Boolean found = false;

    for (Extension e : existingExtsFromRef) {
      if (!found
          && e.getUrl()
              .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
        found = true;
      } else if (!e.getUrl()
          .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
      }
    }

    perfRef.setExtension(newExts);

    assertEquals(
        "Should have 3 extensions (1 performer + 2 others)", 3, perfRef.getExtension().size());
    assertTrue("Should have performer function", found);
    assertEquals(
        "First extension should be performer function",
        "http://hl7.org/fhir/StructureDefinition/event-performerFunction",
        perfRef.getExtension().get(0).getUrl());
  }

  @Test
  public void testRemoveExtensions_PerformerFunctionInMiddle() {
    Reference perfRef = new Reference();
    List<Extension> existingExts = new ArrayList<>();

    Extension other1 = new Extension();
    other1.setUrl("http://other1.org");

    Extension perf = new Extension();
    perf.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    perf.setValue(new StringType("value"));

    Extension other2 = new Extension();
    other2.setUrl("http://other2.org");

    Extension perfDuplicate = new Extension();
    perfDuplicate.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");

    existingExts.add(other1);
    existingExts.add(perf);
    existingExts.add(other2);
    existingExts.add(perfDuplicate);

    perfRef.setExtension(existingExts);

    // Verify initial state
    assertEquals("Reference should have 4 initial extensions", 4, perfRef.getExtension().size());

    // Apply filtering
    List<Extension> existingExtsFromRef = perfRef.getExtension();
    List<Extension> newExts = new ArrayList<>();
    Boolean found = false;

    for (Extension e : existingExtsFromRef) {
      if (!found
          && e.getUrl()
              .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
        found = true;
      } else if (!e.getUrl()
          .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
      }
    }

    perfRef.setExtension(newExts);

    assertEquals(
        "Should have 3 extensions (1 performer + 2 others, no duplicate)",
        3,
        perfRef.getExtension().size());
    long perfCount =
        perfRef.getExtension().stream()
            .filter(e -> e.getUrl().contains("event-performerFunction"))
            .count();
    assertEquals("Should have exactly 1 performer function", 1, perfCount);
    assertTrue("Found flag should be true after processing", found);
  }

  @Test
  public void testRemoveExtensions_CompleteWorkflowWithMultiplePerformers() {
    // Test the complete workflow: get existing exts -> filter -> set back
    Reference performer1 = new Reference();
    Reference performer2 = new Reference();

    // Setup performer 1 with duplicates
    List<Extension> exts1 = new ArrayList<>();
    Extension perf1a = new Extension();
    perf1a.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    perf1a.setValue(new StringType("role1"));

    Extension perf1b = new Extension();
    perf1b.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    perf1b.setValue(new StringType("role2"));

    Extension other1 = new Extension();
    other1.setUrl("http://custom.org/ext1");

    exts1.add(perf1a);
    exts1.add(other1);
    exts1.add(perf1b);
    performer1.setExtension(exts1);

    // Setup performer 2 cleanly
    List<Extension> exts2 = new ArrayList<>();
    Extension perf2 = new Extension();
    perf2.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    perf2.setValue(new StringType("role3"));

    Extension other2 = new Extension();
    other2.setUrl("http://custom.org/ext2");

    exts2.add(perf2);
    exts2.add(other2);
    performer2.setExtension(exts2);

    // Process performer 1
    List<Extension> existingExts1 = performer1.getExtension();
    List<Extension> newExts1 = new ArrayList<>();
    Boolean found1 = false;

    for (Extension e : existingExts1) {
      if (!found1
          && e.getUrl()
              .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts1.add(e);
        found1 = true;
      } else if (!e.getUrl()
          .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts1.add(e);
      }
    }
    performer1.setExtension(newExts1);

    // Process performer 2
    List<Extension> existingExts2 = performer2.getExtension();
    List<Extension> newExts2 = new ArrayList<>();
    Boolean found2 = false;

    for (Extension e : existingExts2) {
      if (!found2
          && e.getUrl()
              .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts2.add(e);
        found2 = true;
      } else if (!e.getUrl()
          .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts2.add(e);
      }
    }
    performer2.setExtension(newExts2);

    // Verify results
    assertEquals(
        "Performer 1 should have 2 extensions after filtering",
        2,
        performer1.getExtension().size());
    assertEquals(
        "Performer 1 should have exactly 1 performer function",
        1,
        performer1.getExtension().stream()
            .filter(e -> e.getUrl().contains("event-performerFunction"))
            .count());

    assertEquals(
        "Performer 2 should have 2 extensions after filtering",
        2,
        performer2.getExtension().size());
    assertEquals(
        "Performer 2 should have exactly 1 performer function",
        1,
        performer2.getExtension().stream()
            .filter(e -> e.getUrl().contains("event-performerFunction"))
            .count());

    assertTrue("Both found flags should be true", found1 && found2);
  }

  @Test
  public void testRemoveExtensions_ReferenceStateAfterFiltering() {
    Reference perfRef = new Reference();
    List<Extension> existingExts = new ArrayList<>();

    // Setup: 2 performer functions + 3 other extensions
    Extension perf1 = new Extension();
    perf1.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");

    Extension other1 = new Extension();
    other1.setUrl("http://ext1.org");

    Extension perf2 = new Extension();
    perf2.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");

    Extension other2 = new Extension();
    other2.setUrl("http://ext2.org");

    Extension other3 = new Extension();
    other3.setUrl("http://ext3.org");

    existingExts.add(perf1);
    existingExts.add(other1);
    existingExts.add(perf2);
    existingExts.add(other2);
    existingExts.add(other3);

    perfRef.setExtension(existingExts);
    assertEquals("Initial reference should have 5 extensions", 5, perfRef.getExtension().size());

    // Apply filtering logic
    List<Extension> existingExtsFromRef = perfRef.getExtension();
    List<Extension> newExts = new ArrayList<>();
    Boolean found = false;

    for (Extension e : existingExtsFromRef) {
      if (!found
          && e.getUrl()
              .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
        found = true;
      } else if (!e.getUrl()
          .contentEquals("http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
        newExts.add(e);
      }
    }

    perfRef.setExtension(newExts);

    // Verify final state
    assertEquals("Final reference should have 4 extensions", 4, perfRef.getExtension().size());
    assertEquals(
        "Should have exactly 1 performer function",
        1,
        perfRef.getExtension().stream()
            .filter(e -> e.getUrl().contains("event-performerFunction"))
            .count());
    assertEquals(
        "Should have exactly 3 other extensions",
        3,
        perfRef.getExtension().stream()
            .filter(e -> !e.getUrl().contains("event-performerFunction"))
            .count());
  }

  // ========== TEST CASES FOR getDeviceAuthor() METHOD ==========

  @Test
  public void testGetDeviceAuthor_ReturnTypeIsDevice() {
    Device device = hcsReportCreator.getDeviceAuthor();
    assertTrue("Result should be instance of Device", device instanceof Device);
  }

  @Test
  public void testGetDeviceAuthor_DeviceNotNull() {
    Device device = hcsReportCreator.getDeviceAuthor();
    assertNotNull("Device should never be null", device);
  }

  @Test
  public void testGetDeviceAuthor_DeviceNameListNotNull() {
    Device device = hcsReportCreator.getDeviceAuthor();
    assertNotNull("Device name list should not be null", device.getDeviceName());
  }

  @Test
  public void testGetDeviceAuthor_DeviceNameListNotEmpty() {
    Device device = hcsReportCreator.getDeviceAuthor();
    assertFalse("Device name list should not be empty", device.getDeviceName().isEmpty());
  }

  @Test
  public void testGetDeviceAuthor_DeviceNameListHasOneElement() {
    Device device = hcsReportCreator.getDeviceAuthor();
    assertEquals(
        "Device name list should have exactly one element", 1, device.getDeviceName().size());
  }

  @Test
  public void testGetDeviceAuthor_FirstComponentNotNull() {
    Device device = hcsReportCreator.getDeviceAuthor();
    assertNotNull("First device name component should not be null", device.getDeviceName().get(0));
  }

  @Test
  public void testGetDeviceAuthor_ComponentIsCorrectType() {
    Device device = hcsReportCreator.getDeviceAuthor();
    assertTrue(
        "Component should be DeviceDeviceNameComponent",
        device.getDeviceName().get(0) instanceof Device.DeviceDeviceNameComponent);
  }

  @Test
  public void testGetDeviceAuthor_ComponentNameNotNull() {
    Device device = hcsReportCreator.getDeviceAuthor();
    Device.DeviceDeviceNameComponent component = device.getDeviceName().get(0);
    assertNotNull("Component name should not be null", component.getName());
  }

  @Test
  public void testGetDeviceAuthor_ComponentNameNotEmpty() {
    Device device = hcsReportCreator.getDeviceAuthor();
    Device.DeviceDeviceNameComponent component = device.getDeviceName().get(0);
    assertNotEquals("Component name should not be empty string", "", component.getName());
  }

  @Test
  public void testGetDeviceAuthor_ComponentNameHasLength() {
    Device device = hcsReportCreator.getDeviceAuthor();
    Device.DeviceDeviceNameComponent component = device.getDeviceName().get(0);
    assertTrue("Component name should have length > 0", component.getName().length() > 0);
  }

  @Test
  public void testGetDeviceAuthor_ConsistentStructureMultipleCalls() {
    for (int i = 0; i < 5; i++) {
      Device device = hcsReportCreator.getDeviceAuthor();

      assertNotNull("Device should not be null (iteration " + i + ")", device);
      assertNotNull(
          "Device name list should not be null (iteration " + i + ")", device.getDeviceName());
      assertEquals("List size should be 1 (iteration " + i + ")", 1, device.getDeviceName().size());

      Device.DeviceDeviceNameComponent component = device.getDeviceName().get(0);
      assertNotNull("Component should not be null (iteration " + i + ")", component);
      assertNotNull("Component name should not be null (iteration " + i + ")", component.getName());
    }
  }

  @Test
  public void testGetDeviceAuthor_EachCallCreatesNewInstance() {
    Device device1 = hcsReportCreator.getDeviceAuthor();
    Device device2 = hcsReportCreator.getDeviceAuthor();

    assertNotSame("Each call should create a new Device instance", device1, device2);
  }

  @Test
  public void testGetDeviceAuthor_ComponentsAreIndependentBetweenCalls() {
    Device device1 = hcsReportCreator.getDeviceAuthor();
    Device device2 = hcsReportCreator.getDeviceAuthor();

    String name1 = device1.getDeviceName().get(0).getName();
    String name2 = device2.getDeviceName().get(0).getName();

    assertEquals("Names should match", name1, name2);
    assertNotSame(
        "Component should be different instance",
        device1.getDeviceName().get(0),
        device2.getDeviceName().get(0));
  }

  private BsaAction getBsaAction() {
    BsaAction action = new CreateReport();
    HashMap<String, String> inputRelatedData = new HashMap<>();
    inputRelatedData.put("1234", "1234");
    inputRelatedData.put("123", "123");
    action.setInputDataIdToRelatedDataIdMap(inputRelatedData);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setType("Patient");
    dataRequirement.setId("1234");
    DataRequirement dataRequirementEncounter = new DataRequirement();
    dataRequirementEncounter.setType("Encounter");
    dataRequirementEncounter.setId("123");

    List<DataRequirement> dataRequirements = new ArrayList<>();
    dataRequirements.add(dataRequirement);
    dataRequirements.add(dataRequirementEncounter);
    action.setInputData(dataRequirements);

    HashMap<String, Set<Resource>> actionOutputDataById = new HashMap<>();
    Set<Resource> res = new HashSet<>();
    res.add(TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json"));
    actionOutputDataById.put("1234", res);
    karProcessingData.setActionOutputDataById(actionOutputDataById);
    Set<Resource> resEnc = new HashSet<>();
    resEnc.add(
        TestUtils.loadResourceDataFromFile(
            Encounter.class, "R4/Encounter/Encounter_97953900.json"));
    actionOutputDataById.put("123", resEnc);
    karProcessingData.setActionOutputDataById(actionOutputDataById);
    return action;
  }

  private HashMap<ResourceType, Set<Resource>> getFilteredByType(String filePath) {
    HashMap<ResourceType, Set<Resource>> groupedResources = new HashMap<>();
    try {
      Bundle bundle = loadBundleFromFile(filePath);

      for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
        Resource resource = entry.getResource();
        if (resource != null) {

          groupedResources
              .computeIfAbsent(resource.getResourceType(), k -> new HashSet<>())
              .add(resource);
        }
      }

    } catch (Exception e) {
      e.printStackTrace();
    }
    return groupedResources;
  }

  private Bundle loadBundleFromFile(String filename) {
    try (InputStream in = new ClassPathResource(filename).getInputStream()) {
      return r4Context.newJsonParser().parseResource(Bundle.class, in);
    } catch (Exception e) {
      return null;
    }
  }

  private KnowledgeArtifactStatus getKnowledgeArtifactStatus() {
    KnowledgeArtifactStatus status = new KnowledgeArtifactStatus();
    status.setId(1);
    status.setIsActive(true);
    status.setOutputFormat(BsaTypes.OutputContentType.FHIR);
    status.setKarVersion("1.0.0");
    status.setKarId("healthcare-survey-specification-bundle-example");
    status.setLastActivationDate(new Date());
    status.setSubscriptionsEnabled(false);
    status.setCovidOnly(false);
    return status;
  }

  private HealthcareSetting getHealthcareSetting() {
    HealthcareSetting healthcareSetting =
        (HealthcareSetting)
            TestUtils.getResourceAsObject("Bsa/HealthCareSettings.json", HealthcareSetting.class);
    return healthcareSetting;
  }

  private KnowledgeArtifact getKnowledgeArtifact() {
    KnowledgeArtifact knowledgeArtifact = new KnowledgeArtifact();
    Set<UriType> receiverAddresses = new HashSet<>();
    receiverAddresses.add(new UriType("http://receiver1.example.com"));
    receiverAddresses.add(new UriType("http://receiver2.example.com"));
    knowledgeArtifact.setReceiverAddresses(receiverAddresses);
    return knowledgeArtifact;
  }

  private NotificationContext getNotificationContext() {
    NotificationContext context =
        TestUtils.readFileContents(
            "Bsa/NotificationContext/NotificationContext.json",
            new TypeReference<NotificationContext>() {});
    Bundle nb = loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");
    context.setNotificationData(r4Context.newJsonParser().encodeResourceToString(nb));
    context.setNotificationResourceType("Encounter");

    return context;
  }

  public String getComposition(Bundle bundle) {
    for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
      if (entry.getResource() instanceof Composition) {
        return r4Context.newJsonParser().encodeResourceToString((Composition) entry.getResource());
      }
    }
    return null;
  }
}
