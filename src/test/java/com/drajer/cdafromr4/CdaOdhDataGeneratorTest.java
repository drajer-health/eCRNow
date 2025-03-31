package com.drajer.cdafromr4;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
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
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class CdaOdhDataGeneratorTest extends BaseGeneratorTest {

  private static final String ODH_CDA_FILE = "CdaTestData/Cda/odh/odh.xml";

  private static final String EMPTY_ODH_CDA_FILE = "CdaTestData/Cda/odh/empty_odh.xml";

  @Test
  public void testGenerateOdhSection() {

    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/bundle/odh.json");

    List<Bundle.BundleEntryComponent> entries = b.getEntry();
    Bundle bundle = new Bundle();
    Set<Resource> resourceSet = new HashSet<>(); // Initialize HashSet outside the loop

    Map<String, List<String>> uniqueResourceIdsByType = new HashMap<>();
    for (Bundle.BundleEntryComponent ent : entries) {

      resourceSet.add(ent.getResource());
      ResourceType resourceType = ent.getResource().getResourceType();
      R3ToR2DataConverterUtils.addResourcesToR4FhirData(
          "1",
          bundle,
          data,
          launchDetails,
          resourceSet,
          resourceType.toString(),
          uniqueResourceIdsByType);
      resourceSet.clear();
    }

    data.setData(bundle);

    String expectedXml = TestUtils.getFileContentAsString(ODH_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml = CdaOdhDataGenerator.generateOdhSection(data, launchDetails, "");

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateOdhSection_withEmptyData() {

    R4FhirData data = new R4FhirData();

    String expectedXml = TestUtils.getFileContentAsString(EMPTY_ODH_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml = CdaOdhDataGenerator.generateOdhSection(data, launchDetails, "");

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateEmptyOdhSection() {
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_ODH_CDA_FILE);
    String actualXml = CdaOdhDataGenerator.generateEmptyOdhSection();
    assertXmlEquals(expectedXml, actualXml);
  }
}
