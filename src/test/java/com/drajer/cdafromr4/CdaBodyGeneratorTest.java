package com.drajer.cdafromr4;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.Test;

public class CdaBodyGeneratorTest extends BaseGeneratorTest {

  private static final String EMPTY_SECTION_BODY_CDA_FILE =
      "CdaTestData/Cda/BodySection/emptySectionBody.xml";

  @Test
  public void testGenerateCdaBodyWithEmptyComponent() {
    String expectedXml =
        "<component>\r\n" + "<structuredBody>\r\n" + "</structuredBody>\r\n" + "</component>";
    String actualXml =
        CdaBodyGenerator.generateCdaBody(null, null, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateCdaBodyWithEmptyData() {
    R4FhirData r4FhirData = createEmptyR4FhirData();
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_SECTION_BODY_CDA_FILE);
    String actualXml =
        CdaBodyGenerator.generateCdaBody(
            r4FhirData, null, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertXmlEquals(expectedXml, actualXml);
  }

  private R4FhirData createEmptyR4FhirData() {
    R4FhirData r4FhirData = new R4FhirData();
    r4FhirData.setPatient(new Patient());
    return r4FhirData;
  }

  @Test
  public void testBody() {

    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/bundle/996.json");

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

    String actualXml =
        CdaBodyGenerator.generateCdaBody(
            data, launchDetails, CdaGeneratorConstants.CDA_EICR_VERSION_R11);
    assertNotNull(actualXml);
    assertEquals("", actualXml);
  }
}
