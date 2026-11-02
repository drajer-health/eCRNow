package com.drajer.cdafromr4;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.*;
import java.util.ArrayList;
import java.util.Collections;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class CdaHistoryOfPresentIllnessGeneratorTest extends BaseGeneratorTest {

  static final String CONDITION_JSON_FILE_PATH =
      "R4/Condition/Condition-Problem-WithCovidTriggerCode.json";
  private static final String HISTORY_OF_PATIENT_ILLNESS_CDA_FILE =
      "CdaTestData/Cda/HistoryOfPresentIllness/HistoryOfPresentIllness.xml";
  private static final String HISTORY_OF_PHI_CDA_FILE =
      "CdaTestData/Cda/HistoryOfPresentIllness/HistoryOfPresentIllness1.xml";

  @Test
  public void testGenerateHistoryOfPresentIllnessSection() {
    R4FhirData r4FhirData = createResourceData(CONDITION_JSON_FILE_PATH);

    r4FhirData.setEncounterDiagnosisConditions(r4FhirData.getConditions());
    String expectedXml = TestUtils.getFileContentAsString(HISTORY_OF_PATIENT_ILLNESS_CDA_FILE);

    String actualXml =
        CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
            new R4FhirData(), CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateHPIData() {

    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/bundle/condition.json");

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
    data.setData(bundle);

    String expectedXml = TestUtils.getFileContentAsString(HISTORY_OF_PHI_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml = CdaHistoryOfPresentIllnessGenerator.generateHPIData(data);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateHistoryOfPresentIllnessSectionWithEmptyConditions() {
    String expectedXml =
        """
        <component>\r
        <section nullFlavor="NI">\r
        <templateId root="1.3.6.1.4.1.19376.1.5.3.1.3.4"/>\r
        <code code="10164-2" codeSystem="2.16.840.1.113883.6.1" codeSystemName="LOINC" displayName="History of Present Illness"/>\r
        <title>History of Present Illness</title>\r
        <text>No History of Present Illness Information</text>\r
        </section>\r
        </component>""";

    String actualXml =
        CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
            r4FhirData, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateHPIData_withAndWithoutConditions() {
    R4FhirData withConds = new R4FhirData();
    Condition c = new Condition();
    c.setId("hpi-cond-1");
    c.setCode(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem(CdaGeneratorConstants.FHIR_SNOMED_URL)
                    .setCode("386661006")
                    .setDisplay("Fever"))
            .setText("Fever"));
    withConds.setEncounterDiagnosisConditions(new ArrayList<>(Collections.singletonList(c)));

    String xml = CdaHistoryOfPresentIllnessGenerator.generateHPIData(withConds);
    assertNotNull("HPI data must be produced", xml);
    assertTrue("section must be opened", xml.contains("<section"));

    String none = CdaHistoryOfPresentIllnessGenerator.generateHPIData(new R4FhirData());
    assertNotNull("HPI data must still be produced with no conditions", none);
  }

  @Test
  public void testGenerateHpiHeader_bothArms() {
    String nf = CdaHistoryOfPresentIllnessGenerator.generateHpiHeader(CdaGeneratorConstants.NF_NI);
    assertNotNull(nf);
    assertTrue("nullFlavor section must be emitted", nf.contains(CdaGeneratorConstants.NF_NI));

    String normal = CdaHistoryOfPresentIllnessGenerator.generateHpiHeader("");
    assertNotNull(normal);
    assertTrue("a plain section must be opened", normal.contains("<section"));

    assertNotNull(CdaHistoryOfPresentIllnessGenerator.generateHpiEndHeader());
  }
}
