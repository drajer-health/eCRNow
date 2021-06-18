package com.drajer.cdafromr4;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Address.AddressUse;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointSystem;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointUse;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Encounter.EncounterLocationComponent;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceFactory;
import org.hl7.fhir.r4.model.StringType;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaFhirUtilitiesTest {

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
  public void testGetOrganization_nullRefrence() {
    Reference ref = new Reference();

    List<BundleEntryComponent> entries = new ArrayList<BundleEntryComponent>();
    Encounter en = new Encounter();
    en.setServiceProvider(ref);

    Organization testOrg = CdaFhirUtilities.getOrganization(entries, en);
    assertNull(testOrg);
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
    assertFalse(result.contains("a@b.com"));

    String result2 = CdaFhirUtilities.getTelecomXml(cps, true);
    assertTrue(result2.contains("(123)456-7890"));
    assertFalse(result2.contains("(098)765-4321"));
    assertFalse(result2.contains("a@b.com"));
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

    String result = CdaFhirUtilities.getAddressXml(addrs);

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

    String result2 = CdaFhirUtilities.getAddressXml(addrs);

    assertTrue(result2.contains("142 Example Drive"));
    assertTrue(result2.contains("20874"));
    assertTrue(result2.contains("MD"));
    assertTrue(result2.contains("Rockville"));

    addrs.clear();
    String result3 = CdaFhirUtilities.getAddressXml(addrs);

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

    String result4 = CdaFhirUtilities.getAddressXml(addrs);

    assertTrue(result4.contains("142 Example Drive"));
    assertTrue(result4.contains("20874"));
    assertTrue(result4.contains("MD"));
    assertTrue(result4.contains("Rockville"));
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
    List<EncounterLocationComponent> locationList =
        new ArrayList<Encounter.EncounterLocationComponent>();
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
    List<EncounterLocationComponent> locationList =
        new ArrayList<Encounter.EncounterLocationComponent>();
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
  public void testGetCodingXmlForMappedConceptDomainWithNullFalvor() {
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
  public void testGetCodingXmlForValueForMappedConceptDomainWithNullFalvor() {
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
}
