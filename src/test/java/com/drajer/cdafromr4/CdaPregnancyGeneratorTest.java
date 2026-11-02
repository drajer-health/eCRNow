package com.drajer.cdafromr4;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.assertj.core.api.Assertions;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DateTimeType;
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
public class CdaPregnancyGeneratorTest extends BaseGeneratorTest {

  private static final String VERSION = CdaGeneratorConstants.CDA_EICR_VERSION_R31;

  private static final String PREGNANCY_CDA_FILE = "CdaTestData/Cda/Pregnancy/pregnancy.xml";

  @Test
  public void testGeneratePregnancySection() {

    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/bundle/pregnancy.json");

    List<Bundle.BundleEntryComponent> entries = b.getEntry();
    Bundle bundle = new Bundle();
    Set<Resource> resourceSet = new LinkedHashSet<>();

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
    data.setData(bundle);

    String expectedXml = TestUtils.getFileContentAsString(PREGNANCY_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    PowerMockito.when(CdaGeneratorUtils.getXmlForII(any())).thenReturn(XML_FOR_II_USING_GUID);

    PowerMockito.when(CdaGeneratorUtils.getXmlForII(any(), any()))
        .thenReturn(
            "<id root=\"2.16.840.1.113883.1.1.1.1\""
                + " extension=\"5e70e24f-4d8b-4989-8453-dcc15b427f71\"/>");

    String actualXml = CdaPregnancyGenerator.generatePregnancySection(data, launchDetails, VERSION);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGeneratePregnancySection_withEmptyData() {

    R4FhirData data = new R4FhirData();

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    PowerMockito.when(CdaGeneratorUtils.getXmlForII(any())).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml = CdaPregnancyGenerator.generatePregnancySection(data, launchDetails, VERSION);

    Assertions.assertThat(actualXml).isEmpty();
  }

  @Test
  public void testGenerateEmptyPregnancySection() {
    String xml = CdaPregnancyGenerator.generateEmptyPregnancySection();

    assertNotNull("An empty section must be produced", xml);
    assertTrue("section element must be opened", xml.contains("<section"));
    assertTrue(
        "nullFlavor must be set on the empty section", xml.contains(CdaGeneratorConstants.NF_NI));
    assertTrue("narrative placeholder must be present", xml.contains("No Social History"));
    assertTrue("section element must be closed", xml.contains("</section>"));
  }

  @Test
  public void testGeneratePregnancySectionHeaderAndEndHeader() {
    String header = CdaPregnancyGenerator.generatePregnancySectionHeader("");
    assertNotNull("A header must be produced", header);
    assertTrue("component element must be opened", header.contains("<component"));
    assertTrue("section element must be opened", header.contains("<section"));

    String nfHeader =
        CdaPregnancyGenerator.generatePregnancySectionHeader(CdaGeneratorConstants.NF_NI);
    assertNotNull("A nullFlavor header must be produced", nfHeader);
    assertTrue("nullFlavor must be emitted", nfHeader.contains(CdaGeneratorConstants.NF_NI));

    String end = CdaPregnancyGenerator.generatePregnancySectionEndHeader();
    assertTrue("section must be closed", end.contains("</section>"));
    assertTrue("component must be closed", end.contains("</component>"));
  }

  @Test
  public void testGenerateEddEntryRelationship_observationWithAndWithoutValue() {
    Observation withEdd = new Observation();
    withEdd.setId("edd-1");
    withEdd.setCode(
        new CodeableConcept()
            .addCoding(
                new Coding().setSystem(CdaGeneratorConstants.FHIR_LOINC_URL).setCode("11778-8")));
    withEdd.setValue(new DateTimeType(new Date()));

    String xml = CdaPregnancyGenerator.generateEddEntryRelationship(withEdd);
    assertNotNull("Must return a value for an observation carrying an EDD", xml);

    Observation withoutEdd = new Observation();
    withoutEdd.setId("edd-2");

    String empty = CdaPregnancyGenerator.generateEddEntryRelationship(withoutEdd);
    assertNotNull("Must return a value rather than throw when no EDD is present", empty);
  }

  @Test
  public void testGeneratePregnancyObservationEntry_observationVariants() {
    Observation obs = new Observation();
    obs.setId("preg-1");
    obs.setCode(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem(CdaGeneratorConstants.FHIR_LOINC_URL)
                    .setCode("82810-3")
                    .setDisplay("Pregnancy status")));
    obs.setEffective(new DateTimeType(new Date()));
    obs.setValue(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem(CdaGeneratorConstants.FHIR_SNOMED_URL)
                    .setCode("77386006")
                    .setDisplay("Pregnant")));

    String xml = CdaPregnancyGenerator.generatePregnancyObservationEntry(obs, launchDetails);
    assertNotNull("An entry must be produced", xml);
    assertTrue("entry element must be opened", xml.contains("<entry"));

    Observation bare = new Observation();
    bare.setId("preg-2");

    String bareXml = CdaPregnancyGenerator.generatePregnancyObservationEntry(bare, launchDetails);
    assertNotNull("Must still return a value with no code, effective or value", bareXml);
  }

  @Test
  public void testGenerateXmlForGestationalAge_withAndWithoutQuantity() {
    Quantity q = new Quantity();
    q.setValue(new java.math.BigDecimal("30"));
    q.setUnit("wk");

    CodeableConcept code =
        new CodeableConcept()
            .addCoding(
                new Coding().setSystem(CdaGeneratorConstants.FHIR_LOINC_URL).setCode("11884-4"));

    String xml =
        CdaPregnancyGenerator.generateXmlForGestationalAge(
            new Observation(), launchDetails, q, code);
    assertNotNull("Must produce gestational age xml", xml);

    String noQty =
        CdaPregnancyGenerator.generateXmlForGestationalAge(
            new Observation(), launchDetails, null, code);
    assertNotNull("Must handle a null quantity", noQty);
  }

  @Test
  public void testGeneratePregnancySection_withNoPregnancyData() {
    R4FhirData data = new R4FhirData();

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String xml = CdaPregnancyGenerator.generatePregnancySection(data, launchDetails, VERSION);

    assertNotNull("A value must be returned rather than null", xml);
    assertTrue("no pregnancy data must yield an empty string", xml.isEmpty());
  }
}
