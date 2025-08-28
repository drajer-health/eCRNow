package com.drajer.cdafromdstu2;

import static org.junit.Assert.*;

import ca.uhn.fhir.model.dstu2.resource.Bundle;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.test.util.TestUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class Dstu2CdaProblemGeneratorTest extends BaseGenerator {

  private static final String PROBLEM_CDA_FILE = "CdaDstuTestData/cda/Problem/problem.xml";
  private static final String EMPTY_PROBLEM_CDA_FILE =
      "CdaDstuTestData/cda/Problem/empty_problem.xml";
  private static final String PROBLEM_BUNDLE_FILE = "CdaDstuTestData/Bundle/ProblemBundle.json";

  @Test
  public void testGenerateProblemSection() {
    Dstu2FhirData data = new Dstu2FhirData();
    Bundle bundle = loadBundleFromFile(PROBLEM_BUNDLE_FILE);
    addResourceToFhirData(bundle, data);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
    String expectedXml = TestUtils.getFileContentAsString(PROBLEM_CDA_FILE);
    String actualXml = Dstu2CdaProblemGenerator.generateProblemSection(data, launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateProblemSection_Empty() {
    Dstu2FhirData data = new Dstu2FhirData();
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_PROBLEM_CDA_FILE);
    String actualXml = Dstu2CdaProblemGenerator.generateProblemSection(data, launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }
}
