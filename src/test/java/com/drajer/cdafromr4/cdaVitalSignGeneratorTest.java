package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.math.BigDecimal;
import java.util.*;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class cdaVitalSignGeneratorTest extends BaseGeneratorTest {

  private static final String VITAL_CDA_FILE = "CdaTestData/Cda/vital/vital.xml";

  private static final String EMPTY_VITAL_CDA_FILE = "CdaTestData/Cda/vital/EmptyVital.xml";
  private static final String VERSION = CdaGeneratorConstants.CDA_EICR_VERSION_R11;

  @Test
  public void testGenerateVitalsSection() {

    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/bundle/vital.json");

    List<Bundle.BundleEntryComponent> entries = b.getEntry();
    Bundle bundle = new Bundle();
    Set<Resource> resourceSet = new LinkedHashSet<>(); // Initialize HashSet outside the loop

    Map<ResourceType, Set<Resource>> resourcesByType = new HashMap<>();

    for (Bundle.BundleEntryComponent ent : entries) {
      Resource resource = ent.getResource();
      ResourceType resourceType = resource.getResourceType();

      resourcesByType.computeIfAbsent(resourceType, k -> new LinkedHashSet<>()).add(resource);
    }

    Map<String, List<String>> uniqueResourceIdsByType = new HashMap<>();
    for (Bundle.BundleEntryComponent ent : entries) {

      ResourceType resourceType = ent.getResource().getResourceType();

      resourceSet.addAll(resourcesByType.getOrDefault(resourceType, Collections.EMPTY_SET));

      if (!resourceSet.isEmpty()) {
        R3ToR2DataConverterUtils.addResourcesToR4FhirData(
            "1",
            bundle,
            data,
            launchDetails,
            resourceSet,
            resourceType.toString(),
            uniqueResourceIdsByType);
        resourceSet.clear();
        resourcesByType.remove(resourceType);
      }
    }
    data.getVitalObs().sort(Comparator.comparing(Observation::getId));

    String expectedXml = TestUtils.getFileContentAsString(VITAL_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml = CdaVitalSignsGenerator.generateVitalsSection(data, launchDetails, VERSION);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateEmptyLabVitals() {
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_VITAL_CDA_FILE);
    String actualXml = CdaVitalSignsGenerator.generateEmptyLabVitals();
    assertXmlEquals(expectedXml, actualXml);
  }

  private static final String LOINC = CdaGeneratorConstants.FHIR_LOINC_URL;

  private Observation vital(String id, String system, String code) {
    Observation obs = new Observation();
    obs.setId(id);
    if (system != null) {
      obs.setCode(new CodeableConcept().addCoding(new Coding().setSystem(system).setCode(code)));
    }
    return obs;
  }

  private Quantity qty(String value, String unit) {
    Quantity q = new Quantity();
    q.setValue(new BigDecimal(value));
    q.setUnit(unit);
    return q;
  }

  @Test
  public void testGetValidVitals_filtersNonLoincAndUncoded() {
    R4FhirData data = new R4FhirData();
    data.setVitalObs(
        new ArrayList<>(
            Arrays.asList(
                vital("keep", LOINC, "8867-4"),
                vital("drop-snomed", CdaGeneratorConstants.FHIR_SNOMED_URL, "271649006"),
                vital("drop-uncoded", null, null))));

    List<Observation> valid = CdaVitalSignsGenerator.getValidVitals(data);

    assertNotNull(valid);
    assertEquals("only the LOINC-coded vital must survive", 1, valid.size());
    assertEquals("keep", valid.get(0).getIdElement().getIdPart());
  }

  @Test
  public void testGetValidVitals_withNoVitals() {
    assertTrue(
        "no vitals must yield an empty list",
        CdaVitalSignsGenerator.getValidVitals(new R4FhirData()).isEmpty());

    R4FhirData empty = new R4FhirData();
    empty.setVitalObs(new ArrayList<>());
    assertTrue(
        "an empty vitals list must yield an empty list",
        CdaVitalSignsGenerator.getValidVitals(empty).isEmpty());
  }

  @Test
  public void testGetNarrativeTextValue_allArms() {
    assertEquals(
        "a null observation must yield the unknown placeholder",
        CdaGeneratorConstants.UNKNOWN_VALUE,
        CdaVitalSignsGenerator.getNarrativeTextValue(null));

    Observation valued = vital("v-1", LOINC, "8867-4");
    valued.setValue(qty("72", "/min"));
    assertNotNull(
        "a valued observation must produce narrative text",
        CdaVitalSignsGenerator.getNarrativeTextValue(valued));

    Observation componentOnly = vital("v-2", LOINC, "85354-9");
    Observation.ObservationComponentComponent c = new Observation.ObservationComponentComponent();
    c.setCode(new CodeableConcept().addCoding(new Coding().setSystem(LOINC).setCode("8480-6")));
    c.setValue(qty("120", "mm[Hg]"));
    componentOnly.addComponent(c);
    assertNotNull(
        "a component-bearing observation must produce narrative text",
        CdaVitalSignsGenerator.getNarrativeTextValue(componentOnly));

    assertEquals(
        "neither value nor component must yield the unknown placeholder",
        CdaGeneratorConstants.UNKNOWN_VALUE,
        CdaVitalSignsGenerator.getNarrativeTextValue(vital("v-3", LOINC, "8867-4")));
  }

  @Test
  public void testGenerateVitalsSection_withNoVitals() {
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String xml =
        CdaVitalSignsGenerator.generateVitalsSection(new R4FhirData(), launchDetails, VERSION);

    assertNotNull("an empty vitals section must still be produced", xml);
  }
}
