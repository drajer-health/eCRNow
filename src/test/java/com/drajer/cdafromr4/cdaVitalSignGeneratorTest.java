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
public class cdaVitalSignGeneratorTest extends BaseGeneratorTest {

  private static final String VITAL_CDA_FILE = "CdaTestData/Cda/vital/vital.xml";

  private static final String EMPTY_VITAL_CDA_FILE = "CdaTestData/Cda/vital/EmptyVital.xml";

  @Test
  public void testGenerateVitalsSection() {

    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/bundle/vital.json");

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
    //    data.getLabResults().sort(Comparator.comparing(Observation::getId));
    //    data.getDiagReports().sort(Comparator.comparing(DiagnosticReport::getId));
    data.setData(bundle);

    String expectedXml = TestUtils.getFileContentAsString(VITAL_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml = CdaVitalSignsGenerator.generateVitalsSection(data, launchDetails, "");

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateEmptyLabVitals() {
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_VITAL_CDA_FILE);
    String actualXml = CdaVitalSignsGenerator.generateEmptyLabVitals();
    assertXmlEquals(expectedXml, actualXml);
  }
}
