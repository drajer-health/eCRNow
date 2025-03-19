package com.drajer.cdafromr4;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.hl7.fhir.r4.model.Bundle;
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
            r4FhirData, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

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
    //    data.getLabResults().sort(Comparator.comparing(Observation::getId));
    //    data.getDiagReports().sort(Comparator.comparing(DiagnosticReport::getId));
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
        "<component>\r\n"
            + "<section nullFlavor=\"NI\">\r\n"
            + "<templateId root=\"1.3.6.1.4.1.19376.1.5.3.1.3.4\"/>\r\n"
            + "<code code=\"10164-2\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"History of Present Illness\"/>\r\n"
            + "<title>History of Present Illness</title>\r\n"
            + "<text>No History of Present Illness Information</text>\r\n"
            + "</section>\r\n"
            + "</component>";

    String actualXml =
        CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
            new R4FhirData(), CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertXmlEquals(expectedXml, actualXml);
  }
}
