package com.drajer.cdafromr4;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Address.AddressUse;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointSystem;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointUse;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DateType;
import org.hl7.fhir.r4.model.DecimalType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Encounter.EncounterLocationComponent;
import org.hl7.fhir.r4.model.Enumerations.AdministrativeGender;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.HumanName.NameUse;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.InstantType;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient.ContactComponent;
import org.hl7.fhir.r4.model.Patient.PatientCommunicationComponent;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceFactory;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Timing;
import org.hl7.fhir.r4.model.Type;
import org.hl7.fhir.r4.model.UriType;
import org.javatuples.Pair;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaFhirUtilitiesTest extends BaseGeneratorTest {

  Map<String, List<String>> testData = new HashMap<String, List<String>>();
  List<Coding> codes = new ArrayList<Coding>();
  public static final String INTERPRETATION_CODE = "interpretationCode";
  private static final Logger logger = LoggerFactory.getLogger(CdaFhirUtilitiesTest.class);

  @Before
  public void setUp() {
    createTestDataForStatusCodeTest();
    createTestDataForCodingXML();
  }

  @Test
  public void testGetOrganization_nullReference() {
    Reference ref = new Reference();

    List<BundleEntryComponent> entries = new ArrayList<BundleEntryComponent>();
    Encounter en = new Encounter();
    en.setServiceProvider(ref);

    Organization testOrg = CdaFhirUtilities.getOrganization(entries, en);
    assertNull(testOrg);
  }

  @Test
  public void testGetAddressXml() {

    List<Address> addrs = new ArrayList<Address>();
    Address ad1 = new Address();
    ad1.setUse(AddressUse.HOME);
    StringType line = new StringType();
    line.setValue("142 Example Drive");
    List<StringType> lines = new ArrayList<StringType>();
    lines.add(line);
    ad1.setLine(lines);
    ad1.setCity("Rockville");
    ad1.setState("MD");
    ad1.setPostalCode("20874");
    ad1.setCountry("US");
    addrs.add(ad1);

    String result = CdaFhirUtilities.getAddressXml(addrs, false);

    assertTrue(result.contains("142 Example Drive"));
    assertTrue(result.contains("20874"));
    assertTrue(result.contains("MD"));
    assertTrue(result.contains("Rockville"));

    addrs.clear();
    Address ad2 = new Address();
    ad1.setUse(AddressUse.BILLING);
    StringType line2 = new StringType();
    line2.setValue("142 Example Drive");
    List<StringType> lines2 = new ArrayList<StringType>();
    lines2.add(line2);
    ad2.setLine(lines2);
    ad2.setCity("Rockville");
    ad2.setState("MD");
    ad2.setPostalCode("20874");
    ad2.setCountry("US");
    addrs.add(ad2);

    String result2 = CdaFhirUtilities.getAddressXml(addrs, false);

    assertTrue(result2.contains("142 Example Drive"));
    assertTrue(result2.contains("20874"));
    assertTrue(result2.contains("MD"));
    assertTrue(result2.contains("Rockville"));

    addrs.clear();
    String result3 = CdaFhirUtilities.getAddressXml(addrs, false);

    assertTrue(result3.contains("nullFlavor=\"NI\""));

    addrs.clear();
    Address ad3 = new Address();
    StringType line3 = new StringType();
    line3.setValue("142 Example Drive");
    List<StringType> lines3 = new ArrayList<StringType>();
    lines3.add(line3);
    ad3.setLine(lines3);
    ad3.setCity("Rockville");
    ad3.setState("MD");
    ad3.setPostalCode("20874");
    ad3.setCountry("US");
    addrs.add(ad3);

    String result4 = CdaFhirUtilities.getAddressXml(addrs, false);

    assertTrue(result4.contains("142 Example Drive"));
    assertTrue(result4.contains("20874"));
    assertTrue(result4.contains("MD"));
    assertTrue(result4.contains("Rockville"));
  }

  @Test
  public void testMultipleGetAddressXml() {

    List<Address> addrs = new ArrayList<Address>();
    Address ad1 = new Address();
    ad1.setUse(AddressUse.HOME);
    StringType line = new StringType();
    line.setValue("142 Example Drive");
    List<StringType> lines = new ArrayList<StringType>();
    lines.add(line);
    ad1.setLine(lines);
    ad1.setCity("Rockville");
    ad1.setState("MD");
    ad1.setPostalCode("20874");
    ad1.setCountry("US");
    addrs.add(ad1);
    Address ad2 = new Address();
    ad1.setUse(AddressUse.BILLING);
    StringType line2 = new StringType();
    line2.setValue("185 Example Drive");
    List<StringType> lines2 = new ArrayList<StringType>();
    lines2.add(line2);
    ad2.setLine(lines2);
    ad2.setCity("Ashton");
    ad2.setState("ND");
    ad2.setPostalCode("30874");
    ad2.setCountry("US");
    addrs.add(ad2);
    Address ad3 = new Address();
    StringType line3 = new StringType();
    line3.setValue("108 Example Drive");
    List<StringType> lines3 = new ArrayList<StringType>();
    lines3.add(line3);
    ad3.setLine(lines3);
    ad3.setCity("Neterhart");
    ad3.setState("SD");
    ad3.setPostalCode("40874");
    ad3.setCountry("US");
    addrs.add(ad3);

    String result = CdaFhirUtilities.getAddressXml(addrs, true);

    assertTrue(result.contains("142 Example Drive"));
    assertTrue(result.contains("185 Example Drive"));
    assertTrue(result.contains("108 Example Drive"));
  }

  @Test
  public void testGetOrganization_withOrgEntry() {
    Reference ref = new Reference();
    ref.setReference("ResId");
    BundleEntryComponent bundleEntry = new BundleEntryComponent();
    Resource org = ResourceFactory.createResource("Organization");
    org.setId("ResId");
    bundleEntry.setResource(org);

    List<BundleEntryComponent> entries = new ArrayList<BundleEntryComponent>();
    entries.add(bundleEntry);
    Encounter en = new Encounter();
    en.setServiceProvider(ref);

    Organization testOrg = CdaFhirUtilities.getOrganization(entries, en);
    assertNotNull(testOrg);
  }

  @Test
  public void testGetOrganization_nullBundleComponent() {
    Reference ref = new Reference();
    ref.setReference("ResId_1");
    BundleEntryComponent bundleEntry = new BundleEntryComponent();
    Resource org = ResourceFactory.createResource("Organization");
    org.setId("ResId");
    bundleEntry.setResource(org);

    List<BundleEntryComponent> entries = new ArrayList<BundleEntryComponent>();
    entries.add(bundleEntry);
    Encounter en = new Encounter();
    en.setServiceProvider(ref);

    Organization testOrg = CdaFhirUtilities.getOrganization(entries, en);
    assertNull(testOrg);
  }

  @Test
  public void testGetOrganization_withLocationEntry() {
    EncounterLocationComponent loc = new EncounterLocationComponent();

    Reference ref = new Reference();
    ref.setReference("ResId");
    loc.setLocation(ref);
    BundleEntryComponent bundleEntry = new BundleEntryComponent();
    Resource location = ResourceFactory.createResource("Location");
    location.setId("ResId");
    bundleEntry.setResource(location);

    List<BundleEntryComponent> entries = new ArrayList<BundleEntryComponent>();
    entries.add(bundleEntry);
    Encounter en = new Encounter();
    List<EncounterLocationComponent> locationList = new ArrayList<EncounterLocationComponent>();
    locationList.add(loc);
    en.setLocation(locationList);

    Location testLocation = CdaFhirUtilities.getLocation(entries, en);
    assertNotNull(testLocation);
  }

  @Test
  public void testGetOrganization_withNullLocationComp() {
    EncounterLocationComponent loc = null;

    Reference ref = new Reference();
    ref.setReference("ResId");
    BundleEntryComponent bundleEntry = new BundleEntryComponent();
    Resource location = ResourceFactory.createResource("Location");
    location.setId("ResId");
    bundleEntry.setResource(location);

    List<BundleEntryComponent> entries = new ArrayList<BundleEntryComponent>();
    entries.add(bundleEntry);
    Encounter en = new Encounter();
    List<EncounterLocationComponent> locationList = new ArrayList<EncounterLocationComponent>();
    locationList.add(loc);
    en.setLocation(locationList);

    Location testLocation = CdaFhirUtilities.getLocation(entries, en);
    assertNull(testLocation);
  }

  @Test
  public void testGetCodingXmlForMappedConceptDomain() {
    String expectedResult =
        "<interpretationCode code=\"A\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"Abnormal\"><translation code=\"N\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"Normal\"/>";
    expectedResult +=
        "\n"
            + "<translation code=\"L\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"Low\"/>";
    expectedResult += "\n" + "</interpretationCode>" + "\n";
    String actualResult =
        CdaFhirUtilities.getCodingXmlForMappedConceptDomain(
            INTERPRETATION_CODE, codes, INTERPRETATION_CODE, false);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void testGetCodingXmlForMappedConceptDomainWithNullFlavor() {
    String expectedResult = "<interpretationCode nullFlavor=\"NI\"/>" + "\n";
    codes = new ArrayList<Coding>();
    String actualResult =
        CdaFhirUtilities.getCodingXmlForMappedConceptDomain(
            INTERPRETATION_CODE, codes, INTERPRETATION_CODE, true);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void testGetCodingXmlForValueForMappedConceptDomain() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"A\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"Abnormal\"><translation code=\"N\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"Normal\"/>";
    expectedResult +=
        "\n"
            + "<translation code=\"L\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"Low\"/>";
    expectedResult += "\n" + "</value>" + "\n";
    String actualResult =
        CdaFhirUtilities.getCodingXmlForValueForMappedConceptDomain(
            INTERPRETATION_CODE, codes, INTERPRETATION_CODE, false);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void testGetCodingXmlForValueForMappedConceptDomainWithNullFlavor() {
    String expectedResult = "<interpretationCode xsi:type=\"CD\" nullFlavor=\"NI\"/>" + "\n";
    codes = new ArrayList<Coding>();
    String actualResult =
        CdaFhirUtilities.getCodingXmlForValueForMappedConceptDomain(
            INTERPRETATION_CODE, codes, INTERPRETATION_CODE, true);
    assertEquals(expectedResult, actualResult);
  }

  private void createTestDataForStatusCodeTest() {
    ObjectMapper mapper = TestUtils.getJsonMapper();
    String testDataJson = TestUtils.getFileContentAsString("R4/Misc/FhirStatusCodeTestData.json");
    Map<String, String> testMap = null;
    try {
      testMap = mapper.readValue(testDataJson, Map.class);
    } catch (JsonProcessingException e) {
      logger.error("Error in reading the test data:::", e);
    }
    if (testMap != null) {
      Set<String> dataKeySet = testMap.keySet();

      for (String key : dataKeySet) {

        String value = testMap.get(key);

        List<String> valueList = Arrays.stream(value.split("\\|")).collect(Collectors.toList());

        testData.put(key, valueList);
      }
    }
  }

  private void createTestDataForCodingXML() {
    String testDataJson = TestUtils.getFileContentAsString("R4/Misc/FhirToCDAMappedCodes.json");
    JSONArray array = new JSONArray(testDataJson);
    for (Object obj : array) {
      JSONObject jsonObj = (JSONObject) obj;
      Coding coding = new Coding();
      coding.setCode(jsonObj.getString("code"));
      coding.setDisplay(jsonObj.getString("display"));
      coding.setSystem(jsonObj.getString("system"));
      codes.add(coding);
    }
  }

  @Test
  public void testGetIdentifierForType() {
    List<Identifier> ids = new ArrayList<>();
    ids.add(
        new Identifier()
            .setType(
                new CodeableConcept()
                    .addCoding(
                        new Coding()
                            .setSystem(CdaGeneratorConstants.FHIR_IDENTIFIER_TYPE_SYSTEM)
                            .setCode("MR")))
            .setValue("123456"));
    ids.add(
        new Identifier()
            .setType(
                new CodeableConcept()
                    .addCoding(
                        new Coding()
                            .setSystem(CdaGeneratorConstants.FHIR_IDTYPE_SYSTEM)
                            .setCode("DL")))
            .setValue("ABC123"));
    ids.add(
        new Identifier()
            .setType(
                new CodeableConcept()
                    .addCoding(
                        new Coding()
                            .setSystem(CdaGeneratorConstants.FHIR_IDTYPE_SYSTEM)
                            .setCode("PPN")))
            .setValue("123-45-6789"));
    String type = "MR";

    List<Identifier> returnIds = CdaFhirUtilities.getIdentifierForType(ids, type);

    assertNotNull(returnIds);
    assertFalse(returnIds.isEmpty());
    assertEquals(1, returnIds.size());
  }

  @Test
  public void testGetGuardianContact() {
    List<ContactComponent> contacts = new ArrayList<>();
    ContactComponent guardianContact = new ContactComponent();
    CodeableConcept relationship = new CodeableConcept();
    Coding coding = new Coding();
    coding.setSystem(CdaGeneratorConstants.FHIR_CONTACT_RELATIONSHIP_CODESYSTEM);
    coding.setCode(CdaGeneratorConstants.FHIR_EMERGENCY_CONTACT_VALUE);
    relationship.addCoding(coding);
    guardianContact.setRelationship(Collections.singletonList(relationship));
    contacts.add(guardianContact);
    ContactComponent result = CdaFhirUtilities.getGuardianContact(contacts);
    assertEquals(guardianContact, result);
  }

  @Test
  public void testGetIdentifierForSystem() {
    List<Identifier> ids = new ArrayList<>();
    ids.add(
        new Identifier()
            .setType(
                new CodeableConcept()
                    .addCoding(
                        new Coding()
                            .setSystem(CdaGeneratorConstants.FHIR_IDENTIFIER_TYPE_SYSTEM)
                            .setCode("MR")))
            .setValue("123456")
            .setSystem("http://acme.org/mrns"));
    ids.add(
        new Identifier()
            .setType(
                new CodeableConcept()
                    .addCoding(
                        new Coding()
                            .setSystem(CdaGeneratorConstants.FHIR_IDTYPE_SYSTEM)
                            .setCode("DL")))
            .setValue("ABC123"));

    String system = "http://acme.org/mrns";

    Identifier returnId = CdaFhirUtilities.getIdentifierForSystem(ids, system);

    assertNotNull(returnId);

    Identifier returnId1 = CdaFhirUtilities.getIdentifierForSystem(null, system);
    assertNull(returnId1);
  }

  @Test
  public void testGetCodingExtension() {
    List<Extension> exts = new ArrayList<>();
    Extension extension = new Extension();
    String extUrl = "http://example.com/ext";

    String subExtUrl = "http://example.com/subext";

    Coding codingValue = new Coding();

    codingValue.setSystem(CdaGeneratorConstants.FHIR_IDENTIFIER_TYPE_SYSTEM);
    codingValue.setCode("MR");
    extension.setUrl(extUrl);
    extension.setValue(codingValue);

    exts.add(extension);
    Coding result = CdaFhirUtilities.getCodingExtension(exts, extUrl, subExtUrl);

    assertNotNull(result);
  }

  @Test
  public void testGetCodingExtensionListWithSubExtension() {
    List<Extension> exts = new ArrayList<>();
    Extension extension = new Extension();
    Extension subExtension = new Extension();

    String extUrl = "http://example.com/ext";

    String subExtUrl = "http://example.com/subext";

    extension.setUrl(extUrl);
    exts.add(extension);

    Coding codingValue = new Coding();

    codingValue.setSystem(CdaGeneratorConstants.FHIR_IDENTIFIER_TYPE_SYSTEM);
    codingValue.setCode("MR");
    subExtension.setUrl(subExtUrl);
    subExtension.setValue(codingValue);

    exts.add(subExtension);
    Coding result = CdaFhirUtilities.getCodingExtension(exts, extUrl, subExtUrl);

    assertNull(result);
  }

  @Test
  public void testGetCodingExtensionWithOnlyExtUrl() {
    List<Extension> exts = new ArrayList<>();
    Extension extension = new Extension();
    String extUrl = "http://example.com/ext";

    Coding codingValue = new Coding();

    codingValue.setSystem(CdaGeneratorConstants.FHIR_IDENTIFIER_TYPE_SYSTEM);
    codingValue.setCode("MR");
    extension.setUrl(extUrl);
    extension.setValue(codingValue);

    exts.add(extension);
    Coding result = CdaFhirUtilities.getCodingExtension(exts, extUrl);

    assertNotNull(result);
    assertEquals(codingValue, result);
  }

  @Test
  public void testGetCodeExtension() {
    List<Extension> exts = new ArrayList<>();
    Extension extension = new Extension();
    String extUrl = "http://example.com/ext";
    CodeType extCodeType = new CodeType();
    extension.setUrl(extUrl);
    extension.setValue(extCodeType);

    exts.add(extension);
    CodeType result = CdaFhirUtilities.getCodeExtension(exts, extUrl);

    assertNotNull(result);
    assertEquals(extCodeType, result);
  }

  @Test
  public void testGetLanguage() {
    List<PatientCommunicationComponent> comms = new ArrayList<>();
    PatientCommunicationComponent patientCommunication = new PatientCommunicationComponent();

    CodeableConcept languageCodeableConcept = new CodeableConcept();

    Coding language = new Coding();

    language.setCode("en-US");
    language.setSystem("http://hl7.org/fhir/ValueSet/languages");
    language.setDisplay("English (United States)");

    languageCodeableConcept.setText("patient Language");
    languageCodeableConcept.setCoding(Collections.singletonList(language));
    patientCommunication.setLanguage(languageCodeableConcept);
    comms.add(patientCommunication);

    Coding result = CdaFhirUtilities.getLanguage(comms);
    assertNotNull(result);
    assertEquals(language, result);
  }

  @Test
  public void testGetCodingForCodeSystem() {
    CodeableConcept languageCodeableConcept = new CodeableConcept();

    Coding language = new Coding();

    language.setCode("en-US");
    language.setSystem("http://hl7.org/fhir/ValueSet/languages");
    language.setDisplay("English (United States)");

    languageCodeableConcept.setText("patient Language");
    languageCodeableConcept.setCoding(Collections.singletonList(language));
    Coding result =
        CdaFhirUtilities.getCodingForCodeSystem(
            languageCodeableConcept, "http://hl7.org/fhir/ValueSet/languages");
    assertNotNull(result);
    assertEquals(language, result);
  }

  @Test
  public void testGetLanguageForCodeSystem() {
    List<PatientCommunicationComponent> comms = new ArrayList<>();
    PatientCommunicationComponent patientCommunication = new PatientCommunicationComponent();

    CodeableConcept languageCodeableConcept = new CodeableConcept();

    Coding language = new Coding();

    language.setCode("en-US");
    language.setSystem("http://hl7.org/fhir/ValueSet/languages");
    language.setDisplay("English (United States)");

    languageCodeableConcept.setText("patient Language");
    languageCodeableConcept.setCoding(Collections.singletonList(language));
    patientCommunication.setLanguage(languageCodeableConcept);
    comms.add(patientCommunication);

    Pair<Coding, Boolean> result =
        CdaFhirUtilities.getLanguageForCodeSystem(comms, "http://hl7.org/fhir/ValueSet/languages");
    assertNotNull(result);
    assertEquals(language, result.getValue0());
  }

  @Test
  public void testGetTelecomXml() {

    List<ContactPoint> cps = new ArrayList<ContactPoint>();
    ContactPoint cp1 = new ContactPoint();
    cp1.setUse(ContactPointUse.HOME);
    cp1.setSystem(ContactPointSystem.PHONE);
    cp1.setValue("1234567890");
    cps.add(cp1);
    ContactPoint cp2 = new ContactPoint();
    cp2.setUse(ContactPointUse.MOBILE);
    cp2.setSystem(ContactPointSystem.PHONE);
    cp2.setValue("0987654321");
    cps.add(cp2);
    ContactPoint cp3 = new ContactPoint();
    cp3.setUse(ContactPointUse.WORK);
    cp3.setSystem(ContactPointSystem.EMAIL);
    cp3.setValue("a@b.com");
    cps.add(cp3);

    String result = CdaFhirUtilities.getTelecomXml(cps, false);

    assertTrue(result.contains("(123)456-7890"));
    assertTrue(result.contains("(098)765-4321"));
    assertTrue(result.contains("a@b.com"));

    String result2 = CdaFhirUtilities.getTelecomXml(cps, true);
    assertTrue(result2.contains("(123)456-7890"));
    assertFalse(result2.contains("(098)765-4321"));
    assertFalse(result2.contains("a@b.com"));

    String result3 = CdaFhirUtilities.getTelecomXml(null, true);
    String expected = "<telecom nullFlavor=\"NI\"/>";
    assertEquals(expected, result3.trim());
  }

  @Test
  public void testGetEmailXmlWithNoEmails() {
    String result = CdaFhirUtilities.getEmailXml(null);
    String expected = "<telecom nullFlavor=\"NI\"/>";
    assertEquals(expected, result.trim());
  }

  @Test
  public void testGetEmailXmlWithContactPoints() {
    List<ContactPoint> cps = new ArrayList<>();
    ContactPoint cp1 = new ContactPoint();
    cp1.setUse(ContactPointUse.HOME);
    cp1.setSystem(ContactPointSystem.PHONE);
    cp1.setValue("1234567890");
    cps.add(cp1);
    ContactPoint cp2 = new ContactPoint();
    cp2.setUse(ContactPointUse.MOBILE);
    cp2.setSystem(ContactPointSystem.PHONE);
    cp2.setValue("0987654321");
    cps.add(cp2);
    ContactPoint cp3 = new ContactPoint();
    cp3.setUse(ContactPointUse.WORK);
    cp3.setSystem(ContactPointSystem.EMAIL);
    cp3.setValue("a@b.com");
    cps.add(cp3);

    String result = CdaFhirUtilities.getEmailXml(cps);
    String expected = "<telecom value=\"mailto:a@b.com\"/>";
    assertEquals(expected, result.trim());
  }

  @Test
  public void testIsCodingPresentForCodeSystem() {

    List<Coding> codings1 = new ArrayList<>();
    codings1.add(new Coding().setSystem("http://example.com/system1").setCode("code1"));
    codings1.add(new Coding().setSystem("http://example.com/system2").setCode("code2"));
    codings1.add(new Coding().setSystem("http://example.com/system3").setCode("code3"));
    Boolean result1 =
        CdaFhirUtilities.isCodingPresentForCodeSystem(codings1, "http://example.com/system2");
    assertTrue(result1);

    List<Coding> codings2 = new ArrayList<>();
    codings2.add(new Coding().setSystem("http://example.com/system1").setCode("code1"));
    codings2.add(new Coding().setSystem("http://example.com/system3").setCode("code3"));
    Boolean result2 =
        CdaFhirUtilities.isCodingPresentForCodeSystem(codings2, "http://example.com/system2");
    assertFalse(result2);
  }

  @Test
  public void testGetCodingDisplayForCodeSystem() {

    String codeSystemUrl = "http://loinc.org";
    Boolean csOptional = false;
    List<Coding> codings = new ArrayList<>();
    codings.add(
        new Coding().setSystem("http://loinc.org").setCode("1234").setDisplay("Test Loinc"));
    codings.add(
        new Coding().setSystem("http://snomed.info").setCode("5678").setDisplay("Test Snomed"));
    codings.add(new Coding().setSystem("http://fhir.org").setCode("abcd").setDisplay("Test Fhir"));

    Pair<String, Boolean> result1 =
        CdaFhirUtilities.getCodingDisplayForCodeSystem(codings, codeSystemUrl, csOptional);

    assertEquals("Test Loinc", result1.getValue0());
    assertTrue(result1.getValue1());

    List<Coding> codings1 = new ArrayList<>();
    codings1.add(new Coding().setSystem("http://loinc.org").setCode("1234"));
    codings1.add(new Coding().setSystem("http://snomed.info").setCode("5678"));

    Pair<String, Boolean> result2 =
        CdaFhirUtilities.getCodingDisplayForCodeSystem(codings1, codeSystemUrl, csOptional);

    assertTrue(result2.getValue1());

    Pair<String, Boolean> result3 =
        CdaFhirUtilities.getCodingDisplayForCodeSystem(codings, "", true);

    assertFalse(result3.getValue1());
  }

  @Test
  public void testCodeableConceptDisplayForCodeSystem() {

    List<CodeableConcept> cds = new ArrayList<>();
    Coding coding = new Coding();
    coding.setCode("260385009");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("");
    cds.add(
        new CodeableConcept().addCoding(coding).setText("Negative for Chlamydia Trachomatis rRNA"));
    String expectedOutput = "Negative for Chlamydia Trachomatis rRNA";
    String actual =
        CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(
            cds, CdaGeneratorConstants.FHIR_SNOMED_URL, false);

    assertEquals(expectedOutput, actual);

    cds.clear();

    coding = new Coding();
    coding.setCode("260385009");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("");
    cds.add(new CodeableConcept().addCoding(coding).setText(""));
    expectedOutput = "Unknown";
    actual =
        CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(
            cds, CdaGeneratorConstants.FHIR_SNOMED_URL, false);

    assertEquals(expectedOutput, actual);

    cds.clear();

    actual =
        CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(
            cds, CdaGeneratorConstants.FHIR_SNOMED_URL, false);

    assertEquals(expectedOutput, actual);
  }

  @Test
  public void testGetActualDateFromDateTimeType() {
    DateTimeType dateTimeType = new DateTimeType("2022-04-01T00:00:00Z");
    Pair<Date, TimeZone> actual = CdaFhirUtilities.getActualDate(dateTimeType);
    assertEquals(dateTimeType.getValue(), actual.getValue0());
    assertEquals(dateTimeType.getTimeZone(), actual.getValue1());
  }

  @Test
  public void testGetActualDateFromPeriodStart() {
    Date expected = new Date(1648780800000L); // April 1, 2022 00:00:00 GMT
    Period period = new Period().setStartElement(new DateTimeType(expected));
    Pair<Date, TimeZone> actual = CdaFhirUtilities.getActualDate(period);
    assertEquals(expected, actual.getValue0());
    assertNotNull(actual.getValue1());
  }

  @Test
  @Ignore
  public void testGetActualDateFromPeriodEnd() {
    Date expected = new Date(1648867199000L); // April 1, 2022 23:59:59 GMT
    Period period = new Period().setEndElement(new DateTimeType(expected));
    period.setStartElement(null);
    Pair<Date, TimeZone> actual = CdaFhirUtilities.getActualDate(period);
    assertEquals(expected, actual.getValue0());
    assertNull(actual.getValue1());
  }

  @Test
  public void testGetActualDateFromInstantType() {
    InstantType instantType = new InstantType("2022-04-01T00:00:00Z");
    Pair<Date, TimeZone> actual = CdaFhirUtilities.getActualDate(instantType);
    assertEquals(instantType.getValue(), actual.getValue0());
    assertEquals(instantType.getTimeZone(), actual.getValue1());
  }

  @Test
  public void testGetActualDateFromTimingWithRepeatBounds() {
    Date expected = new Date(1648780800000L); // April 1, 2022 00:00:00 GMT
    Period repeatBounds = new Period().setStartElement(new DateTimeType(expected));
    Timing timing =
        new Timing().setRepeat(new Timing.TimingRepeatComponent().setBounds(repeatBounds));
    Pair<Date, TimeZone> actual = CdaFhirUtilities.getActualDate(timing);
    assertEquals(expected, actual.getValue0());
    assertNotNull(actual.getValue1());
  }

  @Test
  public void testGetActualDateFromTimingWithoutRepeatBounds() {
    Timing timing = new Timing();
    Pair<Date, TimeZone> actual = CdaFhirUtilities.getActualDate(timing);
    assertNull(actual.getValue0());
    assertNull(actual.getValue1());
  }

  @Test
  public void testGetCodeableConceptXmlForCodeSystem() {
    List<CodeableConcept> cds = new ArrayList<>();
    Coding coding = new Coding();
    coding.setCode("260385009");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("Negative");
    cds.add(
        new CodeableConcept().addCoding(coding).setText("Negative for Chlamydia Trachomatis rRNA"));

    String cdName = "code";
    Boolean valueTrue = true;
    String codeSystemUrl = "http://snomed.info/sct";
    Boolean csOptional = true;
    String expectedXml =
        "<value xsi:type=\"CD\" code=\"260385009\" codeSystem=\"2.16.840.1.113883.6.96\" codeSystemName=\"SNOMED-CT\" displayName=\"Negative\"></value>\r\n"
            + "";

    String actualXml =
        CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
            cds, cdName, valueTrue, codeSystemUrl, csOptional);

    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetCodeableConceptXmlForCodeSystem1() {
    List<CodeableConcept> cds = new ArrayList<>();
    Coding coding = new Coding();
    coding.setCode("260385009");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("Negative");
    cds.add(
        new CodeableConcept().addCoding(coding).setText("Negative for Chlamydia Trachomatis rRNA"));

    String cdName = "code";
    Boolean valueTrue = false;
    String codeSystemUrl = "http://snomed.info/sct";
    Boolean csOptional = true;
    String expectedXml =
        "<code code=\"260385009\" codeSystem=\"2.16.840.1.113883.6.96\" codeSystemName=\"SNOMED-CT\" displayName=\"Negative\"></code>"
            + "";

    String actualXml =
        CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
            cds, cdName, valueTrue, codeSystemUrl, csOptional);

    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetCodingForValidCodeSystems() {

    List<CodeableConcept> cds = new ArrayList<>();
    Coding coding = new Coding();
    coding.setCode("260385009");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("Negative");
    cds.add(
        new CodeableConcept().addCoding(coding).setText("Negative for Chlamydia Trachomatis rRNA"));
    List<Coding> codings = CdaFhirUtilities.getCodingForValidCodeSystems(cds);

    assertThat(codings).isNotNull().isNotEmpty();
  }

  @Test
  public void testGetCodeableConceptXmlWithValueTrue() {
    List<CodeableConcept> cds = new ArrayList<>();
    Coding coding = new Coding();
    coding.setCode("260385009");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("Negative");

    cds.add(
        new CodeableConcept().addCoding(coding).setText("Negative for Chlamydia Trachomatis rRNA"));

    String cdName = "code";
    Boolean valueTrue = true;

    String actualXml = CdaFhirUtilities.getCodeableConceptXml(cds, cdName, valueTrue);

    String expectedXml =
        "<value xsi:type=\"CD\" code=\"260385009\" codeSystem=\"2.16.840.1.113883.6.96\" codeSystemName=\"SNOMED-CT\" displayName=\"Negative\"></value>";

    assertThat(actualXml).isNotNull().isNotEmpty();

    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetCodeableConceptXmlWithValueFalse() {
    List<CodeableConcept> cds = new ArrayList<>();
    Coding coding = new Coding();
    coding.setCode("260385009");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("Negative");

    cds.add(
        new CodeableConcept().addCoding(coding).setText("Negative for Chlamydia Trachomatis rRNA"));

    String cdName = "code";
    Boolean valueTrue = false;
    String actualXml = CdaFhirUtilities.getCodeableConceptXml(cds, cdName, valueTrue);

    String expectedXml =
        "<code code=\"260385009\" codeSystem=\"2.16.840.1.113883.6.96\" codeSystemName=\"SNOMED-CT\" displayName=\"Negative\"></code>"
            + "";

    assertThat(actualXml).isNotNull().isNotEmpty();
    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetCodeableConceptXmlForMappedConceptDomain() {

    List<CodeableConcept> cds = new ArrayList<>();
    Coding coding = new Coding();
    coding.setCode("HH");
    coding.setSystem("http://terminology.hl7.org/CodeSystem/v3-ObservationInterpretation");

    coding.setDisplay("HH");

    cds.add(new CodeableConcept().addCoding(coding).setText("High Observation"));

    Boolean valueTrue = false;

    Boolean includeNullFlavor = true;
    String actualXml =
        CdaFhirUtilities.getCodeableConceptXmlForMappedConceptDomain(
            CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
            cds,
            CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
            valueTrue,
            includeNullFlavor);

    String expectedXml =
        "<interpretationCode code=\"HH\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"HH\"></interpretationCode>\r\n"
            + "";

    assertThat(actualXml).isNotNull().isNotEmpty();
    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetCodeableConceptXmlForMappedConceptDomain_WithOnlyText() {

    List<CodeableConcept> cds = new ArrayList<>();
    Coding coding = new Coding();
    cds.add(new CodeableConcept().addCoding(coding).setText("interpretationCode text 1"));
    cds.add(new CodeableConcept().addCoding(coding).setText("interpretationCode text 2"));

    Boolean valueTrue = false;

    Boolean includeNullFlavor = true;
    String actualXml =
        CdaFhirUtilities.getCodeableConceptXmlForMappedConceptDomain(
            CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
            cds,
            CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
            valueTrue,
            includeNullFlavor);

    String expectedXml =
        "<interpretationCode nullFlavor=\"OTH\">\r\n"
            + "<originalText>\r\n"
            + "interpretationCode text 1</originalText>\r\n"
            + "\r\n"
            + "</interpretationCode>\r\n"
            + "<interpretationCode nullFlavor=\"OTH\">\r\n"
            + "<originalText>\r\n"
            + "interpretationCode text 2</originalText>\r\n"
            + "\r\n"
            + "</interpretationCode>";
    assertThat(actualXml).isNotNull().isNotEmpty();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetCodeableConceptXmlForMappedConceptDomainWithValueTrue() {

    List<CodeableConcept> cds = new ArrayList<>();
    Coding coding = new Coding();
    coding.setCode("HH");
    coding.setSystem("http://terminology.hl7.org/CodeSystem/v3-ObservationInterpretation");

    coding.setDisplay("HH");

    cds.add(new CodeableConcept().addCoding(coding).setText("High Observation"));

    Boolean valueTrue = true;

    Boolean includeNullFlavor = true;
    String actualXml =
        CdaFhirUtilities.getCodeableConceptXmlForMappedConceptDomain(
            CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
            cds,
            CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
            valueTrue,
            includeNullFlavor);

    String expectedXml =
        "<value xsi:type=\"CD\" code=\"HH\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"HH\"></value>\r\n"
            + "";

    assertThat(actualXml).isNotNull().isNotEmpty();
    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetCodingXmlForValueWithEmptyCodeList() {
    List<Coding> codes = new ArrayList<>();
    String cdName = "testCd";
    String contentRef = "testRef";
    String result = CdaFhirUtilities.getCodingXmlForValue(codes, cdName, contentRef);
    assertEquals("<testCd xsi:type=\"CD\" nullFlavor=\"NI\"/>", result.trim());
  }

  @Test
  public void testGetCodingXmlForValueWithSingleCode() {
    List<Coding> codes = new ArrayList<>();
    codes.add(new Coding("http://example.com/codeSystem", "123", "Test Code"));
    String cdName = CdaGeneratorConstants.VAL_EL_NAME;

    String expected = "<value xsi:type=\"CD\" nullFlavor=\"NI\"/>";
    String result = CdaFhirUtilities.getCodingXmlForValue(codes, cdName, null);
    result = result.replaceAll("\n", "");
    assertEquals(expected.trim(), result.trim());
  }

  @Test
  public void testGetCodingXmlForValueWithMultipleCodes() {
    List<Coding> codes = new ArrayList<>();
    codes.add(new Coding("http://snomed.info/sct", "260385009", "Test Code 1"));
    codes.add(new Coding("http://snomed.info/sct", "260385009", "Test Code 2"));
    String cdName = "testCd";
    String contentRef = "testRef";
    String expected =
        "<value xsi:type=\"CD\" code=\"260385009\" codeSystem=\"2.16.840.1.113883.6.96\" codeSystemName=\"SNOMED-CT\" displayName=\"Test Code 1\"><originalText>\r\n"
            + "<reference value=\"#testRef\"/>\r\n"
            + "</originalText>\r\n"
            + "<translation code=\"260385009\" codeSystem=\"2.16.840.1.113883.6.96\" codeSystemName=\"SNOMED-CT\" displayName=\"Test Code 2\"/>\r\n"
            + "</value>";
    String result = CdaFhirUtilities.getCodingXmlForValue(codes, cdName, contentRef);

    assertXmlEquals(expected, result);
  }

  @Test
  public void testGetDateTimeTypeXml() {

    DateTimeType dateTimeType = new DateTimeType("2023-04-03T15:30:00-04:00");
    String expectedXml = "<effectiveTime value=\"20230403153000-0400\"/>\r\n" + "";
    String actualXml = CdaFhirUtilities.getDateTimeTypeXml(dateTimeType, "effectiveTime");
    assertEquals(expectedXml.trim(), actualXml.trim());

    // Test with a null DateTimeType object
    expectedXml = "<effectiveTime nullFlavor=\"NI\"/>";
    actualXml = CdaFhirUtilities.getDateTimeTypeXml(null, "effectiveTime");
    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetDateTypeXml() {
    DateType dateType = new DateType("2023-04-03");
    String expectedXml = "<effectiveTime value=\"20230403\"/>" + "";
    String actualXml = CdaFhirUtilities.getDateTypeXml(dateType, "effectiveTime");
    assertEquals(expectedXml.trim(), actualXml.trim());

    // Test with a null DateTimeType object
    expectedXml = "<effectiveTime nullFlavor=\"NI\"/>";
    actualXml = CdaFhirUtilities.getDateTypeXml(null, "effectiveTime");
    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetDisplayStringForDateTimeType() {

    DateTimeType dateTimeType = new DateTimeType("2023-04-03T15:30:00-04:00");
    String expectedXml = "20230403153000-0400" + "";
    String actualXml = CdaFhirUtilities.getDisplayStringForDateTimeType(dateTimeType);
    assertEquals(expectedXml.trim(), actualXml.trim());

    // Test with a null DateTimeType object
    expectedXml = "Unknown";
    actualXml = CdaFhirUtilities.getDisplayStringForDateTimeType(null);
    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetPeriodXml() {
    Period period = new Period();
    period.setStartElement(new DateTimeType("2022-04-03T15:30:00-04:00"));
    period.setEndElement(new DateTimeType("2023-04-03T15:30:00-04:00"));
    String expectedXml =
        "<effectiveTime><low value=\"20220403153000-0400\"/><high value=\"20230403153000-0400\"/></effectiveTime>";

    String actualXml = CdaFhirUtilities.getPeriodXml(period, "effectiveTime", false);
    actualXml = actualXml.replaceAll("\n", "");
    assertEquals(expectedXml.trim(), actualXml.trim());

    // Test with a null Period object
    expectedXml = "<effectiveTime nullFlavor=\"NI\"/>";
    actualXml = CdaFhirUtilities.getPeriodXml(null, "effectiveTime", false);
    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetQuantityXml() {
    Quantity quantity = new Quantity(10.5);
    quantity.setCode("mg");
    String expectedXml = "<value xsi:type=\"PQ\" value=\"10.5\" unit=\"mg\"/>";
    String actualXml = CdaFhirUtilities.getQuantityXml(quantity, "doseQuantity", true);
    assertEquals(expectedXml.trim(), actualXml.trim());

    expectedXml = "<doseQuantity xsi:type=\"PQ\" nullFlavor=\"NI\"/>";
    actualXml = CdaFhirUtilities.getQuantityXml(null, "doseQuantity", true);
    assertEquals(expectedXml.trim(), actualXml.trim());

    Quantity quantity1 = new Quantity(10.5);
    String expectedXm1 = "<value xsi:type=\"PQ\" value=\"10.5\" unit=\"1\"/>";
    String actualXml1 = CdaFhirUtilities.getQuantityXml(quantity1, "doseQuantity", true);
    assertEquals(expectedXm1.trim(), actualXml1.trim());
  }

  @Test
  public void testGetBirthSexXml() {
    String expectedXml =
        "<value xsi:type=\"CD\" code=\"M\" codeSystem=\"2.16.840.1.113883.5.1\" codeSystemName=\"Administrative Gender\" displayName=\"Male\"/>\r\n"
            + "";
    String actualXml = CdaFhirUtilities.getBirthSexXml("M");
    assertEquals(expectedXml.trim(), actualXml.trim());

    expectedXml =
        "<value xsi:type=\"CD\" code=\"F\" codeSystem=\"2.16.840.1.113883.5.1\" codeSystemName=\"Administrative Gender\" displayName=\"Female\"/>\r\n"
            + "";
    actualXml = CdaFhirUtilities.getBirthSexXml("F");
    assertEquals(expectedXml.trim(), actualXml.trim());

    expectedXml = "<value xsi:type=\"CD\" nullFlavor=\"UNK\"/>";
    actualXml = CdaFhirUtilities.getBirthSexXml("UNK");
    assertEquals(expectedXml.trim(), actualXml.trim());

    expectedXml = "<value xsi:type=\"CD\" nullFlavor=\"NI\"/>";
    actualXml = CdaFhirUtilities.getBirthSexXml(null);
    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetReligiousAffiliationXml() {

    Coding coding = new Coding();

    coding.setSystem(CdaGeneratorConstants.FHIR_RELIGIOUS_AFFILIATION_URL);
    coding.setCode("1001");
    coding.setDisplay("Anglican");
    String actualXml = CdaFhirUtilities.getReligiousAffiliationXml(coding);

    String expectedXml =
        "<religiousAffiliationCode code=\"1001\" codeSystem=\"2.16.840.1.113883.5.1076\" codeSystemName=\"v3-ReligiousAffiliation\" displayName=\"Anglican\"/>";

    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetGenderXmlWithNullFlavor() {

    String actualXml = CdaFhirUtilities.getGenderXml(AdministrativeGender.UNKNOWN);
    String expectedXml = "<administrativeGenderCode nullFlavor=\"UNK\"/>";

    assertEquals(expectedXml.trim(), actualXml.trim());

    // test pass null value
    expectedXml = "<administrativeGenderCode nullFlavor=\"NI\"/>";

    actualXml = CdaFhirUtilities.getGenderXml(null);

    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetNameXmlWithValues() {

    List<HumanName> humanNames = new ArrayList<>();
    HumanName humanName = new HumanName();
    List<StringType> theGiven = new ArrayList<>();
    theGiven.add(new StringType("john"));
    theGiven.add(new StringType("mark"));

    humanName.setUse(NameUse.OFFICIAL);
    humanName.setFamily("parker");
    humanName.setGiven(theGiven);

    humanNames.add(humanName);

    String expectedXml =
        "<given qualifier=\"PR\">john</given><given qualifier=\"PR\">mark</given><family>parker</family>";

    String nameXml = CdaFhirUtilities.getNameXml(humanNames);
    nameXml = nameXml.replace("\n", "");

    assertEquals(expectedXml.trim(), nameXml.trim());
  }

  @Test
  public void testGetNameXmlWithEmptyValue() {

    List<HumanName> humanNames = new ArrayList<>();
    HumanName humanName = new HumanName();
    List<StringType> theGiven = new ArrayList<>();
    theGiven.add(new StringType("john"));
    theGiven.add(new StringType("mark"));

    humanName.setUse(NameUse.OFFICIAL);
    humanName.setFamily("parker");
    humanName.setGiven(theGiven);

    humanNames.add(humanName);

    String expectedXml = "<given nullFlavor=\"NI\"/><family nullFlavor=\"NI\"/>";

    String nameXml = CdaFhirUtilities.getNameXml(null);
    nameXml = nameXml.replace("\n", "");

    assertEquals(expectedXml.trim(), nameXml.trim());
  }

  @Test
  public void testGetStringForCoding() {
    Coding coding = new Coding();
    coding.setSystem("http://loinc.org");
    coding.setCode("15074-8");
    coding.setDisplay("Glucose [Moles/volume] in Blood");

    String actual = CdaFhirUtilities.getStringForCoding(coding);

    assertEquals("Glucose [Moles/volume] in Blood", actual);

    Coding coding1 = new Coding();

    coding1.setCodeElement(new CodeType("15074-8"));
    coding1.setSystemElement(new UriType("http://loinc.org"));

    actual = CdaFhirUtilities.getStringForCoding(coding1);

    assertEquals("http://loinc.org|15074-8", actual);
  }

  @Test
  public void testGetStringForDates() {
    Pair<Date, TimeZone> onset =
        new Pair<>(new Date(1645778400000L), TimeZone.getTimeZone("UTC")); // Date:
    // 2022-02-25T00:00:00Z
    Pair<Date, TimeZone> abatement =
        new Pair<>(new Date(1645864800000L), TimeZone.getTimeZone("UTC")); // Date:
    // 2022-02-26T00:00:00Z
    Pair<Date, TimeZone> recorded =
        new Pair<>(new Date(1645833600000L), TimeZone.getTimeZone("UTC")); // Date:
    // 2022-02-25T18:00:00Z

    String expected =
        "Sat Feb 26 08:40:00 UTC 2022|Sat Feb 26 00:00:00 UTC 2022|Fri Feb 25 08:40:00 UTC 2022";
    String result = CdaFhirUtilities.getStringForDates(recorded, onset, abatement);

    assertEquals(expected, result);
  }

  @Test
  public void testGetCombinationStringForCodeSystem_WithValue() {
    CodeableConcept codeableConcept = new CodeableConcept();
    Coding coding = new Coding();
    coding.setCode("260385009");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("Negative");

    codeableConcept.addCoding(coding);
    codeableConcept.setText("Negative for Chlamydia Trachomatis rRNA");
    String codeSystemUrl = "http://snomed.info/sct";
    Boolean csOptional = true;
    String expectedXml = "Negative-Negative";

    String actualXml =
        CdaFhirUtilities.getCombinationStringForCodeSystem(
            codeableConcept, coding, codeSystemUrl, csOptional);
    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetCombinationStringForCodeSystem_WithCodeAndValue() {
    CodeableConcept codeableConcept = new CodeableConcept();
    StringType value = new StringType("Test value");

    Coding coding = new Coding();

    coding.setCode("260385009");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("Negative");

    codeableConcept.addCoding(coding);
    codeableConcept.setText("Negative for Chlamydia Trachomatis rRNA");
    String codeSystemUrl = "http://snomed.info/sct";
    Boolean csOptional = true;
    String expectedXml = "Negative-";

    // Act
    String actualXml =
        CdaFhirUtilities.getCombinationStringForCodeSystem(
            codeableConcept, value, codeSystemUrl, csOptional);

    // Assert
    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetCombinationStringForCodeSystem() {
    CodeableConcept code = new CodeableConcept();
    code.addCoding()
        .setSystem("http://snomed.info/sct")
        .setCode("260385009")
        .setDisplay("Test code");
    CodeableConcept value = new CodeableConcept();
    value
        .addCoding()
        .setSystem("http://snomed.info/sct")
        .setCode("260385008")
        .setDisplay("Test value");
    String codeSystemUrl = "http://snomed.info/sct";
    Boolean csOptional = false;
    String expectedResult = "Test code-Test value";
    String actualResult =
        CdaFhirUtilities.getCombinationStringForCodeSystem(code, value, codeSystemUrl, csOptional);
    assertEquals(expectedResult.trim(), actualResult.trim());
  }

  @Test
  public void testGetStringForQuantity() {

    Quantity quantity = new Quantity();
    quantity.setValue(100);

    quantity.setCode("mg");

    quantity.setSystemElement(new UriType("http://snomed.info/sct"));

    quantity.setUnit("milligrams");
    quantity.setValueElement(new DecimalType(100));

    String expectedResult = "100|http://snomed.info/sct|mg";
    String actualResult = CdaFhirUtilities.getStringForQuantity(quantity);
    assertEquals(expectedResult, actualResult);

    // Check No code
    Quantity quantity1 = new Quantity();
    quantity1.setValue(100);
    quantity1.setValueElement(new DecimalType(100));

    expectedResult = "100";
    actualResult = CdaFhirUtilities.getStringForQuantity(quantity1);
    assertEquals(expectedResult, actualResult);

    // passing null quantity
    expectedResult = "Unknown";
    actualResult = CdaFhirUtilities.getStringForQuantity(null);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void testGetStringForMedicationType() {

    List<Medication> medList = null;
    Resource resource =
        (Resource)
            loadResourceDataFromFile(
                MedicationRequest.class, "CdaTestData/Medication/medicationRequest.json");

    String actual = CdaFhirUtilities.getStringForMedicationType(resource, null);
    assertNotNull(actual);
  }

  @Test
  public void testGetStringForMedicationTypeWithMedicationRequestAndReference() {
    List<Medication> medList = null;
    MedicationRequest mr = new MedicationRequest();
    Reference med = new Reference("#med123");
    mr.setMedication(med);
    mr.setContained(getContained());
    String expected = "Unknown";
    String actual = CdaFhirUtilities.getStringForMedicationType(mr, null);
    assertEquals(expected, actual);
  }

  @Test
  public void testGetStringForMedicationTypeWithMedicationRequestAndCodeableConcept() {
    List<Medication> medList = null;
    MedicationRequest mr = new MedicationRequest();
    CodeableConcept cc = new CodeableConcept();
    Coding coding = new Coding().setCode("1234").setDisplay("Simvastatin");
    cc.addCoding(coding);
    mr.setMedication(cc);
    String expected = "Simvastatin";
    String actual = CdaFhirUtilities.getStringForMedicationType(mr, null);
    assertEquals(expected, actual);
  }

  @Test
  @Ignore
  public void testGetStringForMedicationTypeWithMedicationAdministrationAndReference() {
    List<Medication> medList = null;
    MedicationAdministration ma = new MedicationAdministration();
    Reference med = new Reference("#medication");
    ma.setMedication(med);
    ma.setContained(getContained());
    String expected = "Metformin";
    String actual = CdaFhirUtilities.getStringForMedicationType(ma, null);
    assertEquals(expected, actual);
  }

  @Test
  public void testGetStringForMedicationTypeWithMedicationAdministrationAndCodeableConcept() {
    List<Medication> medList = null;
    MedicationAdministration ma = new MedicationAdministration();
    CodeableConcept cc = new CodeableConcept();
    Coding coding = new Coding().setCode("5678").setDisplay("Atorvastatin");
    cc.addCoding(coding);
    ma.setMedication(cc);
    String expected = "Atorvastatin";
    String actual = CdaFhirUtilities.getStringForMedicationType(ma, null);
    assertEquals(expected, actual);
  }

  @Test
  public void testGetStringForMedicationTypeWithMedicationStatementAndReference() {
    List<Medication> medList = null;
    MedicationStatement ms = new MedicationStatement();
    Reference med = new Reference("#med789");
    ms.setMedication(med);
    ms.setContained(getContained());
    String expected = "Unknown";
    String actual = CdaFhirUtilities.getStringForMedicationType(ms, null);
    assertEquals(expected, actual);
  }

  @Test
  public void testGetStringForMedicationTypeWithMedicationStatementAndCodeableConcept() {
    List<Medication> medList = null;
    MedicationStatement ms = new MedicationStatement();
    CodeableConcept cc = new CodeableConcept();
    Coding coding = new Coding().setCode("9012").setDisplay("Ibuprofen");
    cc.addCoding(coding);
    ms.setMedication(cc);
    String expected = "Ibuprofen";
    String actual = CdaFhirUtilities.getStringForMedicationType(ms, null);
    assertEquals(expected, actual);
  }

  private List<Resource> getContained() {
    List<Resource> contained = new ArrayList<>();

    Medication med = new Medication();
    med.setId("#medication");
    CodeableConcept cc = new CodeableConcept();
    Coding coding = new Coding().setCode("70618").setDisplay("ibuprofen");
    cc.addCoding(coding);
    med.setCode(cc);
    contained.add(med);

    return contained;
  }

  @Test
  public void testGetMatchedCodesForResourceAndUrl() {

    String matchResourceType = "Observation";
    String csUrl = "http://loinc.org";

    List<String> actual =
        CdaFhirUtilities.getMatchedValuesForResourceAndUrl(launchDetails, matchResourceType, csUrl);
    assertThat(actual).isNotEmpty();
  }

  @Test
  public void testGetMatchedValuesForResourceAndUrl() {

    String matchResourceType = "Observation";
    String csUrl = "http://loinc.org";

    List<String> actual =
        CdaFhirUtilities.getMatchedCodesForResourceAndUrl(launchDetails, matchResourceType, csUrl);
    assertThat(actual).isNotEmpty();
  }

  @Test
  public void testIsCodePresent() {

    List<String> codes = Arrays.asList("code1", "code2", "code3");
    String code = "code2";
    Boolean result = CdaFhirUtilities.isCodePresent(codes, code);
    assertEquals(true, result);

    Boolean result1 = CdaFhirUtilities.isCodePresent(codes, "code4");
    assertEquals(false, result1);
  }

  @Test
  public void testGetMatchingCodeFromCodingForCodeSystem() {

    List<String> matchedCodes = Arrays.asList("26464-2", "718-7");
    List<Coding> cds =
        Arrays.asList(
            new Coding().setSystem("http://loinc.org").setCode("26464-2"),
            new Coding().setSystem("http://loinc.org").setCode("718-7"),
            new Coding().setSystem("http://loinc.org").setCode("48642-3"));
    String csUrl = "http://loinc.org";
    String result =
        CdaFhirUtilities.getMatchingCodeFromCodingForCodeSystem(matchedCodes, cds, csUrl);
    assertEquals("26464-2", result);
  }

  @Test
  public void testGetMatchingCodeFromTypeForCodeSystem() {

    List<String> matchedCodes = Arrays.asList("26464-2", "718-7");
    Coding coding = new Coding();
    coding.setSystem("http://loinc.org");
    coding.setCode("26464-2");
    String csUrl = "http://loinc.org";
    String result =
        CdaFhirUtilities.getMatchingCodeFromTypeForCodeSystem(matchedCodes, coding, csUrl);
    assertEquals("26464-2", result);
  }

  @Test
  public void testGetMatchingCodeFromTypeForCodeSystemWithCodeableConcept() {

    List<String> matchedCodes = Arrays.asList("26464-2", "718-7");
    CodeableConcept cds = new CodeableConcept();
    List<Coding> codings =
        Arrays.asList(
            new Coding().setSystem("http://loinc.org").setCode("26464-2"),
            new Coding().setSystem("http://loinc.org").setCode("718-7"),
            new Coding().setSystem("http://loinc.org").setCode("48642-3"));

    cds.setCoding(codings);
    String csUrl = "http://loinc.org";
    String result = CdaFhirUtilities.getMatchingCodeFromTypeForCodeSystem(matchedCodes, cds, csUrl);
    assertEquals("26464-2", result);
  }

  @Test
  public void testGetCodeForNameUse() {
    List<HumanName> humanNames = new ArrayList<>();
    HumanName humanName = new HumanName();
    List<StringType> theGiven = new ArrayList<>();
    theGiven.add(new StringType("john"));
    theGiven.add(new StringType("mark"));

    humanName.setUse(NameUse.OFFICIAL);
    humanName.setFamily("parker");
    humanName.setGiven(theGiven);

    humanNames.add(humanName);

    String result = CdaFhirUtilities.getCodeForNameUse(humanNames);

    assertEquals("L", result);
  }

  @ParameterizedTest
  @CsvSource({
    "active, active",
    "in-progress, active",
    "intended, active",
    "not-taken, active",
    "completed, completed",
    "entered-in-error, nullified",
    "stopped, aborted",
    "on-hold, suspended",
    "unknown, held",
    "draft, held",
    "cancelled, cancelled"
  })
  public void testGetStatusCodeForFhirMedStatusCodes(String input, String expectedOutput) {

    assertEquals(expectedOutput, CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(input));
  }

  @Test
  public void testGetStatusCodeForFhirMedStatusCodes1() {

    assertEquals("active", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("active"));

    assertEquals("active", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("in-progress"));

    assertEquals("active", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("intended"));

    assertEquals("active", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("not-taken"));

    assertEquals("completed", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("completed"));

    assertEquals("active", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("in-progress"));

    assertEquals(
        "nullified", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("entered-in-error"));

    assertEquals("aborted", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("stopped"));
    assertEquals("suspended", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("on-hold"));

    assertEquals("held", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("unknown"));

    assertEquals("held", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("draft"));

    assertEquals("cancelled", CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes("cancelled"));
  }

  @Test
  public void testGetXmlForCodeableConceptWithCDAndValueSetAndVersion() {

    Coding coding1 = new Coding();
    coding1.setSystem("http://loinc.org");
    coding1.setCode("15074-8");
    coding1.setDisplay("Glucose [Moles/volume] in Blood");

    Coding coding2 = new Coding();
    coding2.setCode("code");
    coding2.setSystem("system");

    CodeableConcept codeableConcept1 = new CodeableConcept();

    codeableConcept1.addCoding(coding1);

    boolean actual =
        testGetXmlForCodeableConceptWithCDAndValueSetAndVersion2(
            CdaGeneratorConstants.CODE_EL_NAME,
            "15074-8",
            "http://loinc.org",
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            "",
            "",
            codeableConcept1,
            CdaGeneratorConstants.FHIR_LOINC_URL,
            "1",
            false,
            "<code code=\"15074-8\" codeSystem=\"http://loinc.org\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"><originalText><reference value=\"#1\"/></originalText></code>");

    assertTrue(actual);
    boolean actual1 =
        testGetXmlForCodeableConceptWithCDAndValueSetAndVersion2(
            CdaGeneratorConstants.CODE_EL_NAME,
            "15074-8",
            "http://loinc.org",
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            "",
            "",
            codeableConcept1,
            CdaGeneratorConstants.FHIR_LOINC_URL,
            "1",
            true,
            "<code xsi:type=\"CD\" code=\"15074-8\" codeSystem=\"http://loinc.org\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></code>");
    assertTrue(actual1);
  }

  public static boolean testGetXmlForCodeableConceptWithCDAndValueSetAndVersion2(
      String elementName,
      String code,
      String codeSystem,
      String codeSystemName,
      String valueSet,
      String valueSetVersion,
      CodeableConcept cc,
      String csUrl,
      String contentRef,
      Boolean valueElem,
      String expectedOutput) {
    String actualOutput =
        CdaFhirUtilities.getXmlForCodeableConceptWithCDAndValueSetAndVersion(
            elementName,
            code,
            codeSystem,
            codeSystemName,
            valueSet,
            valueSetVersion,
            cc,
            csUrl,
            contentRef,
            valueElem);
    actualOutput = actualOutput.replaceAll("\n", "");
    assertEquals(expectedOutput.trim(), actualOutput.trim());
    return true;
  }

  @Test
  public void testGetMaritalStatusXml() {
    CodeableConcept cd = new CodeableConcept();
    Coding coding = new Coding();
    coding.setSystem(CdaGeneratorConstants.FHIR_MARITAL_STATUS_URL);
    coding.setCode("M");
    cd.addCoding(coding);
    String expectedXml =
        "<"
            + CdaGeneratorConstants.MARITAL_STATUS_CODE_EL_NAME
            + " code=\""
            + "M"
            + "\" codeSystem=\""
            + "2.16.840.1.113883.5.2"
            + "\" codeSystemName=\""
            + "v3-MaritalStatus"
            + "\"/>";
    String actualXml = CdaFhirUtilities.getMaritalStatusXml(cd);

    assertEquals(expectedXml.trim(), actualXml.trim());
  }

  @Test
  public void testGetGenderXml() {

    Map<String, String> testData = new HashMap<>();
    testData.put(
        "male",
        "<administrativeGenderCode code=\"M\" codeSystem=\"2.16.840.1.113883.5.1\" codeSystemName=\"HL7AdministrativeGenderCode\" displayName=\"male\"/>");
    testData.put(
        "female",
        "<administrativeGenderCode code=\"F\" codeSystem=\"2.16.840.1.113883.5.1\" codeSystemName=\"HL7AdministrativeGenderCode\" displayName=\"female\"/>");
    testData.put(
        "other",
        "<administrativeGenderCode code=\"UN\" codeSystem=\"2.16.840.1.113883.5.1\" codeSystemName=\"HL7AdministrativeGenderCode\" displayName=\"unknown\"/>");

    for (Map.Entry<String, String> entry : testData.entrySet()) {
      String actualXml =
          CdaFhirUtilities.getGenderXml(AdministrativeGender.fromCode(entry.getKey()));
      String expectedXml = entry.getValue();

      assertEquals(expectedXml.trim(), actualXml.trim());
    }
  }

  @Test
  public void testGetXmlForType() {

    String expectedValue;
    String actualValue;

    Coding coding1 = new Coding();
    coding1.setSystem("http://loinc.org");
    coding1.setCode("15074-8");
    coding1.setDisplay("Glucose [Moles/volume] in Blood");

    Coding coding2 = new Coding();
    coding2.setCode("code");
    coding2.setSystem("system");

    CodeableConcept codeableConcept1 = new CodeableConcept();

    codeableConcept1.addCoding(coding1);

    CodeableConcept codeableConcept2 = new CodeableConcept();
    codeableConcept2.setText("text");
    codeableConcept2.addCoding(coding2);

    Quantity quantity = new Quantity();
    quantity.setValue(new BigDecimal("10"));
    quantity.setCode("mg");
    quantity.setSystemElement(new UriType("http://snomed.info/sct"));
    quantity.setValueElement(new DecimalType(10));

    DateTimeType dateTimeType = new DateTimeType("2022-01-01T00:00:00Z");

    Timing timing = new Timing();
    timing.setRepeat(new Timing.TimingRepeatComponent());
    timing.getRepeat().setBounds(new Period());

    Period period = new Period();
    period.setStart(new Date(2323223232L));
    period.setEnd(new Date(2523223232L));

    CodeType codeType = new CodeType("code");

    StringType stringType = new StringType("string");

    expectedValue =
        "<code code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></code>";
    actualValue =
        CdaFhirUtilities.getXmlForType(coding1, CdaGeneratorConstants.CODE_EL_NAME, false);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());

    expectedValue =
        "<value xsi:type=\"CD\" code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></value>";
    actualValue = CdaFhirUtilities.getXmlForType(coding1, CdaGeneratorConstants.CODE_EL_NAME, true);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());

    expectedValue =
        "<code code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></code>";
    actualValue =
        CdaFhirUtilities.getXmlForType(codeableConcept1, CdaGeneratorConstants.CODE_EL_NAME, false);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());

    expectedValue = "<value value=\"10\" unit=\"mg\"/>";
    actualValue =
        CdaFhirUtilities.getXmlForType(quantity, CdaGeneratorConstants.VAL_EL_NAME, false);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());

    expectedValue = "<code code=\"code\"/>";
    actualValue =
        CdaFhirUtilities.getXmlForType(codeType, CdaGeneratorConstants.CODE_EL_NAME, false);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());

    expectedValue = "<effectiveTime value=\"20220101000000+0000\"/>";
    actualValue =
        CdaFhirUtilities.getXmlForType(dateTimeType, CdaGeneratorConstants.EFF_TIME_EL_NAME, false);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());

    expectedValue =
        "<effectiveTime>\r\n"
            + "<low nullFlavor=\"NI\"/>\r\n"
            + "<high nullFlavor=\"NI\"/>\r\n"
            + "</effectiveTime>";
    actualValue =
        CdaFhirUtilities.getXmlForType(timing, CdaGeneratorConstants.EFF_TIME_EL_NAME, false);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    expectedValue = StringUtils.normalizeSpace(expectedValue).trim();
    assertEquals(expectedValue, actualValue);

    expectedValue =
        "<effectiveTime xsi:type=\"IVL_TS\"><low value=\"19700127212023+0000\"/> <high value=\"19700130045343+0000\"/> </effectiveTime>";
    actualValue =
        CdaFhirUtilities.getXmlForType(period, CdaGeneratorConstants.EFF_TIME_EL_NAME, true);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    expectedValue = StringUtils.normalizeSpace(expectedValue).trim();
    assertEquals(expectedValue, actualValue);

    expectedValue = "<code>string</code>";
    actualValue =
        CdaFhirUtilities.getXmlForType(stringType, CdaGeneratorConstants.CODE_EL_NAME, false);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());

    expectedValue = "<code nullFlavor=\"NI\"/>";
    actualValue = CdaFhirUtilities.getXmlForType(null, CdaGeneratorConstants.CODE_EL_NAME, false);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());

    expectedValue = "<value xsi:type=\"ST\">No Value</value>";
    actualValue = CdaFhirUtilities.getXmlForType(null, CdaGeneratorConstants.CODE_EL_NAME, true);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());
  }

  @Test
  public void testGetXmlForTypeForValueIvlTsEffectiveTime() {

    DateTimeType dt = new DateTimeType("2023-04-19T12:30:00-07:00");
    String expected = "<effectiveTime value=\"20230419123000-0700\"/>";

    String result = CdaFhirUtilities.getXmlForTypeForValueIvlTsEffectiveTime("effectiveTime", dt);

    assertEquals(expected.trim(), result.trim());
  }

  @Test
  public void testGetStringForTypeFromCoding() {
    Coding coding =
        new Coding()
            .setSystem("http://example.com/codes")
            .setCode("1234")
            .setDisplay("Example Code");
    String result = CdaFhirUtilities.getStringForType(coding);
    assertEquals("Example Code", result);
  }

  @Test
  public void testGetStringForTypeFromCodeableConcept() {
    CodeableConcept concept =
        new CodeableConcept()
            .setText("Example Concept")
            .addCoding(new Coding().setSystem("http://example.com/codes").setCode("1234"))
            .addCoding(new Coding().setSystem("http://example.com/codes").setCode("5678"));
    String result = CdaFhirUtilities.getStringForType(concept);
    assertEquals("Example Concept", result);
  }

  @Test
  public void testGetStringForTypeFromQuantity() {
    Quantity quantity =
        new Quantity()
            .setValue(123.45)
            .setUnit("mg")
            .setSystem("http://unitsofmeasure.org")
            .setCode("mg");

    String result = CdaFhirUtilities.getStringForType(quantity);

    assertEquals("123.45|http://unitsofmeasure.org|mg", result);
  }

  @Test
  public void testGetStringForTypeFromDateTimeType() {
    DateTimeType dateTime = new DateTimeType("2023-05-05T10:00:00-04:00");
    String result = CdaFhirUtilities.getStringForType(dateTime);
    assertEquals("20230505100000-0400", result);
  }

  @Test
  public void testGetStringForTypeFromTiming() {
    Timing timing =
        new Timing()
            .setRepeat(
                new Timing.TimingRepeatComponent()
                    .setBounds(
                        new Period()
                            .setStartElement(new DateTimeType("2023-05-01T00:00:00Z"))
                            .setEndElement(new DateTimeType("2023-05-31T23:59:59Z"))));
    String result = CdaFhirUtilities.getStringForType(timing);
    assertEquals("20230501000000+0000|20230531235959+0000", result);
  }

  @Test
  public void testGetStringForTypeFromPeriod() {
    Period period =
        new Period()
            .setStartElement(new DateTimeType("2023-05-01T00:00:00Z"))
            .setEndElement(new DateTimeType("2023-05-31T23:59:59Z"));
    String result = CdaFhirUtilities.getStringForType(period);
    assertEquals("20230501000000+0000|20230531235959+0000", result);
  }

  @Test
  public void testGetXmlForTypeForValueIvlTsEffectiveTime_WithDifferentInputs() {
    DateTimeType dateTimeType = new DateTimeType("2022-01-01T00:00:00Z");
    String expectedValue = "";
    String actualValue = null;

    Timing timing = new Timing();
    timing.setRepeat(new Timing.TimingRepeatComponent());
    timing.getRepeat().setBounds(new Period());

    Period period = new Period();
    period.setStart(new Date(2323223232L));
    period.setEnd(new Date(2523223232L));

    expectedValue = "<effectiveTime value=\"20220101000000+0000\"/>\r\n";
    actualValue =
        CdaFhirUtilities.getXmlForTypeForValueIvlTsEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, dateTimeType);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());

    expectedValue = "";
    actualValue =
        CdaFhirUtilities.getXmlForTypeForValueIvlTsEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, null);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    assertEquals(expectedValue.trim(), actualValue.trim());

    expectedValue =
        "<effectiveTime>\r\n"
            + "<low value=\"19700127212023+0000\"/>\r\n"
            + "<high value=\"19700130045343+0000\"/>\r\n"
            + "</effectiveTime>";
    actualValue =
        CdaFhirUtilities.getXmlForTypeForValueIvlTsEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, period);
    actualValue = StringUtils.normalizeSpace(actualValue).trim();
    expectedValue = StringUtils.normalizeSpace(expectedValue).trim();
    assertEquals(expectedValue, actualValue);
  }

  @Test
  public void testGetXmlForTypeForCodeSystem() {

    Coding coding1 = new Coding();
    coding1.setSystem("http://loinc.org");
    coding1.setCode("15074-8");
    coding1.setDisplay("Glucose [Moles/volume] in Blood");

    Coding coding2 = new Coding();
    coding2.setCode("code");
    coding2.setSystem("system");

    CodeableConcept codeableConcept1 = new CodeableConcept();
    codeableConcept1.addCoding(coding1);

    CodeableConcept codeableConcept2 = new CodeableConcept();
    codeableConcept2.setText("text");
    codeableConcept2.addCoding(coding2);

    testGetXmlForTypeForCodeSystem(
        coding1,
        CdaGeneratorConstants.CODE_EL_NAME,
        false,
        CdaGeneratorConstants.FHIR_LOINC_URL,
        false,
        "<code code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></code>\r\n");

    testGetXmlForTypeForCodeSystem(
        coding1,
        CdaGeneratorConstants.CODE_EL_NAME,
        true,
        CdaGeneratorConstants.FHIR_LOINC_URL,
        false,
        "<value xsi:type=\"CD\" code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></value>\r\n");

    testGetXmlForTypeForCodeSystem(
        coding1,
        CdaGeneratorConstants.CODE_EL_NAME,
        true,
        CdaGeneratorConstants.FHIR_ICD10_CM_URL,
        false,
        "<code xsi:type=\"CD\" nullFlavor=\"OTH\"><translation code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"/></value>");

    testGetXmlForTypeForCodeSystem(
        codeableConcept1,
        CdaGeneratorConstants.CODE_EL_NAME,
        false,
        CdaGeneratorConstants.FHIR_LOINC_URL,
        false,
        "<code code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></code>\r\n");

    testGetXmlForTypeForCodeSystem(
        null,
        CdaGeneratorConstants.CODE_EL_NAME,
        false,
        CdaGeneratorConstants.FHIR_LOINC_URL,
        false,
        "<code nullFlavor=\"NI\"/>");

    testGetXmlForTypeForCodeSystem(
        null,
        CdaGeneratorConstants.CODE_EL_NAME,
        true,
        CdaGeneratorConstants.FHIR_LOINC_URL,
        false,
        "<value xsi:type=\"CD\" nullFlavor=\"NI\"/>");
  }

  public static void testGetXmlForTypeForCodeSystem(
      Type dt,
      String elName,
      Boolean valFlag,
      String codeSystemUrl,
      Boolean csOptional,
      String expectedOutput) {
    String actualOutput =
        CdaFhirUtilities.getXmlForTypeForCodeSystem(dt, elName, valFlag, codeSystemUrl, csOptional);

    actualOutput = actualOutput.replaceAll("\n", "");
    assertEquals(expectedOutput.trim(), actualOutput.trim());
  }

  @Test
  public void testGetStatusCodeForFhirMedStatusCodes() {

    Set<String> testSet = testData.keySet();
    for (String dataKey : testSet) {
      for (String input : testData.get(dataKey)) {
        String output = CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(input);
        assertEquals(dataKey, output);
      }
    }
  }

  @Test
  public void testGetAllCodingsFromExtension() {
    List<Extension> exts = new ArrayList<>();
    Extension extension = new Extension();
    String extUrl = "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity";

    String subExtUrl = "detailed";

    Coding codingValue = new Coding();

    codingValue.setSystem("urn:oid:2.16.840.1.113883.6.238");
    codingValue.setCode("2178-2");
    codingValue.setDisplay("Latin American");

    extension.setUrl(extUrl);
    extension.setValue(codingValue);

    exts.add(extension);
    List<Coding> result = CdaFhirUtilities.getAllCodingsFromExtension(exts, extUrl, subExtUrl);

    assertThat(result).isNotEmpty();
    assertEquals(codingValue.getCode(), result.get(0).getCode());
  }

  @Test
  public void testGetAllCodingsFromExtensionListWithSubExtension1() {
    List<Extension> exts = new ArrayList<>();
    Extension extension = new Extension();
    Extension subExtension = new Extension();

    String extUrl = "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity";

    String subExtUrl = "detailed";

    extension.setUrl(extUrl);

    Coding codingValue = new Coding();

    codingValue.setSystem("urn:oid:2.16.840.1.113883.6.238");
    codingValue.setCode("2178-2");
    codingValue.setDisplay("Latin American");
    subExtension.setUrl(subExtUrl);
    subExtension.setValue(codingValue);

    extension.addExtension(subExtension);
    exts.add(extension);
    List<Coding> result = CdaFhirUtilities.getAllCodingsFromExtension(exts, extUrl, subExtUrl);

    assertThat(result).isNotEmpty();
    assertEquals(codingValue.getCode(), result.get(0).getCode());
  }

  @Test
  public void testGetAllCodingsFromExtensionListWithSubExtension2() {
    List<Extension> exts = new ArrayList<>();
    Extension extension = new Extension();
    Extension subExtension = new Extension();
    CodeableConcept valueCodeableConcept = new CodeableConcept();
    String extUrl = "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity";
    String subExtUrl = "detailed";
    extension.setUrl(extUrl);

    Coding codingValue = new Coding();

    codingValue.setSystem("urn:oid:2.16.840.1.113883.6.238");
    codingValue.setCode("2178-2");
    codingValue.setDisplay("Latin American");

    valueCodeableConcept.addCoding(codingValue);

    subExtension.setUrl(subExtUrl);
    subExtension.setValue(valueCodeableConcept);

    extension.addExtension(subExtension);
    exts.add(extension);
    List<Coding> result = CdaFhirUtilities.getAllCodingsFromExtension(exts, extUrl, subExtUrl);

    assertThat(result).isNotEmpty();
    assertEquals(codingValue.getCode(), result.get(0).getCode());
  }

  @Test
  public void testGetAllCodingsFromExtensionWithOnlyExtUrl() {
    List<Extension> exts = new ArrayList<>();
    Extension extension = new Extension();
    String extUrl = "http://example.com/ext";

    Coding codingValue = new Coding();

    codingValue.setSystem(CdaGeneratorConstants.FHIR_IDENTIFIER_TYPE_SYSTEM);
    codingValue.setCode("MR");
    extension.setUrl(extUrl);
    extension.setValue(codingValue);

    exts.add(extension);
    List<Coding> result = CdaFhirUtilities.getAllCodingsFromExtension(exts, extUrl, "");

    assertThat(result).isNotEmpty();
    assertEquals(codingValue, result.get(0));
  }

  public void testGetStringForCodeableConcept_TextNotNull() {
    CodeableConcept cd = new CodeableConcept();
    cd.setText("Display-Text");
    String result = CdaFhirUtilities.getStringForCodeableConcept(cd);
    assertEquals("Display-Text", result);
  }

  @Test
  public void testGetStringForCodeableConcept_TextNull() {
    CodeableConcept cd = new CodeableConcept();

    String result = CdaFhirUtilities.getStringForCodeableConcept(cd);
    assertThat(result).isBlank();
  }

  @Test
  public void testGetStringForCodeableConcept_TextNullAndCodingNotNull() {
    CodeableConcept cd = new CodeableConcept();
    Coding coding = new Coding();
    coding.setSystem("http://loinc.org");
    coding.setCode("15074-8");
    coding.setDisplay("Glucose [Moles/volume] in Blood");

    cd.addCoding(coding);
    String result = CdaFhirUtilities.getStringForCodeableConcept(cd);
    assertEquals("Glucose [Moles/volume] in Blood", result);
  }

  @Test
  public void testGetStringForCodeableConcept_WithEmptyValue() {

    assertThat(CdaFhirUtilities.getStringForCodeableConcept(null)).isBlank();
  }

  @Test
  public void testGetStringForCodeableConcept_CodingListWithMultipleCodings() {
    CodeableConcept cd = new CodeableConcept();
    Coding coding1 = new Coding();
    coding1.setSystem("http://loinc.org");
    coding1.setCode("15074-8");
    coding1.setDisplay("Glucose [Moles/volume] in Blood-1");

    Coding coding2 = new Coding();
    coding2.setSystem("http://loinc.org");
    coding2.setCode("15074-8");
    coding2.setDisplay("Glucose [Moles/volume] in Blood-2");

    cd.addCoding(coding1);
    cd.addCoding(coding2);

    String result = CdaFhirUtilities.getStringForCodeableConcept(cd);
    assertEquals("Glucose [Moles/volume] in Blood-1 | Glucose [Moles/volume] in Blood-2", result);
  }
}
