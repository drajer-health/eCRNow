package com.drajer.cdafromr4;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
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
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Encounter.EncounterLocationComponent;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceFactory;
import org.junit.Before;
import org.junit.Test;

public class CdaFhirUtilitiesTest {

  Map<String, List<String>> testData = new HashMap<String, List<String>>();

  @Before
  public void setUp() {
    createTestDataForStatusCodeTest();
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

  private void createTestDataForStatusCodeTest() {
    ObjectMapper mapper = TestUtils.getJsonMapper();
    String testDataJson = TestUtils.getFileContentAsString("R4/Misc/FhirStatusCodeTestData.json");
    Map<String, String> testMap = null;
    try {
      testMap = mapper.readValue(testDataJson, Map.class);
    } catch (JsonProcessingException e) {
      System.out.println("Fix this");
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
}
