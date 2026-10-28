package com.drajer.cdafromr4;

import static org.junit.Assert.*;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
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

  private static final String VERSION = CdaGeneratorConstants.CDA_EICR_VERSION_R31;

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

    String actualXml = CdaOdhDataGenerator.generateOdhSection(data, launchDetails, VERSION);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateOdhSection_withEmptyData() {

    R4FhirData data = new R4FhirData();

    String expectedXml = TestUtils.getFileContentAsString(EMPTY_ODH_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml = CdaOdhDataGenerator.generateOdhSection(data, launchDetails, VERSION);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateEmptyOdhSection() {
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_ODH_CDA_FILE);
    String actualXml = CdaOdhDataGenerator.generateEmptyOdhSection();
    assertXmlEquals(expectedXml, actualXml);
  }

  private static final String LOINC = CdaGeneratorConstants.FHIR_LOINC_URL;

  private Observation bareOdhObs(String id, String loincCode) {
    Observation obs = new Observation();
    obs.setId(id);
    obs.setCode(new CodeableConcept().addCoding(new Coding().setSystem(LOINC).setCode(loincCode)));
    return obs;
  }

  private ObservationComponentComponent component(String loincCode, String displayValue) {
    ObservationComponentComponent c = new ObservationComponentComponent();
    c.setCode(new CodeableConcept().addCoding(new Coding().setSystem(LOINC).setCode(loincCode)));
    if (displayValue != null) {
      c.setValue(
          new CodeableConcept()
              .addCoding(new Coding().setSystem(LOINC).setCode(displayValue))
              .setText(displayValue));
    }
    return c;
  }

  private R4FhirData dataWith(Observation... obs) {
    R4FhirData data = new R4FhirData();
    data.setOccupationObs(new ArrayList<>(Arrays.asList(obs)));
    return data;
  }

  @Test
  public void testGenerateOccHistoryEntry_buildsCompleteEntry() {
    Observation obs = bareOdhObs("occ-hist-1", CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_CODE);
    obs.setEffective(new DateTimeType(new Date()));
    obs.setValue(
        new CodeableConcept()
            .addCoding(new Coding().setSystem(LOINC).setCode("12345-6").setDisplay("Nurse"))
            .setText("Nurse"));

    String xml = CdaOdhDataGenerator.generateOccHistoryEntry(obs, launchDetails);

    assertNotNull("An entry must be produced", xml);
    assertTrue("entry element must be opened", xml.contains("<entry"));
    assertTrue("observation act must be present", xml.contains("<observation"));
    assertTrue(
        "SNOMED occupational history code must be emitted",
        xml.contains(CdaGeneratorConstants.SNOMED_OCC_HISTORY_CODE));
    assertTrue(
        "LOINC translation must be emitted",
        xml.contains(CdaGeneratorConstants.LOINC_OCC_HISTORY_CODE));
    assertTrue(
        "statusCode must be completed", xml.contains(CdaGeneratorConstants.COMPLETED_STATUS));
    assertTrue("effectiveTime must be emitted", xml.contains("effectiveTime"));
    assertTrue("value must be emitted", xml.contains("<value"));
    assertTrue("entry element must be closed", xml.contains("</entry>"));
  }

  @Test
  public void testGenerateOccHistoryEntry_withoutEffectiveDateOrValue() {
    Observation obs = bareOdhObs("occ-hist-2", CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_CODE);

    String xml = CdaOdhDataGenerator.generateOccHistoryEntry(obs, launchDetails);

    assertNotNull("An entry must still be produced", xml);
    assertTrue("entry element must be opened", xml.contains("<entry"));
    assertTrue("entry element must be closed", xml.contains("</entry>"));
  }

  @Test
  public void testGenerateOdhSection_observationsWithoutComponentsOrValues() {
    R4FhirData data =
        dataWith(
            bareOdhObs("bare-past-present", CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_CODE),
            bareOdhObs("bare-usual", CdaGeneratorConstants.USUAL_OCCUPATION_CODE),
            bareOdhObs("bare-employment", CdaGeneratorConstants.EMPLOYMENT_STATUS_CODE));

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String xml = CdaOdhDataGenerator.generateOdhSection(data, launchDetails, VERSION);

    assertNotNull("A section must still be produced", xml);
    assertTrue("section must be opened", xml.contains("<section"));
    assertTrue("the narrative table must be built", xml.contains("<table"));
    assertTrue("section must be closed", xml.contains("</section>"));
  }

  @Test
  public void testGenerateOdhSection_componentsWithUnrecognisedCodes() {
    Observation pastPresent =
        bareOdhObs("wrong-comp-past", CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_CODE);
    pastPresent.addComponent(component("00000-0", "not-an-industry"));
    pastPresent.setValue(
        new CodeableConcept()
            .addCoding(new Coding().setSystem(LOINC).setCode("11111-1"))
            .setText("Welder"));

    Observation usual = bareOdhObs("wrong-comp-usual", CdaGeneratorConstants.USUAL_OCCUPATION_CODE);
    usual.addComponent(component("99999-9", "not-a-usual-industry"));

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String xml =
        CdaOdhDataGenerator.generateOdhSection(
            dataWith(pastPresent, usual), launchDetails, VERSION);

    assertNotNull("A section must still be produced", xml);
    assertTrue("section must be opened", xml.contains("<section"));
    assertTrue("section must be closed", xml.contains("</section>"));
  }

  @Test
  public void testGenerateOdhSection_skipsNonOdhObservations() {
    Observation nonOdh = bareOdhObs("non-odh", "85354-9"); // blood pressure panel

    R4FhirData data = new R4FhirData();
    data.setOccupationObs(new ArrayList<>(Arrays.asList(nonOdh)));

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String xml = CdaOdhDataGenerator.generateOdhSection(data, launchDetails, VERSION);

    assertNotNull("A section must still be produced", xml);
    assertTrue("section must be opened", xml.contains("<section"));
    assertFalse("no ODH entry should be emitted", xml.contains("<entry"));
  }

  @Test
  public void testAddPastOrPresentIndustryObservation_withAndWithoutComponent() {
    Observation withIndustry =
        bareOdhObs("ind-1", CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_CODE);
    withIndustry.addComponent(
        component(CdaGeneratorConstants.PAST_PRESENT_INDUSTRY_CODE, "Hospital"));

    String xml =
        CdaOdhDataGenerator.addPastOrPresentIndustryObservation(withIndustry, launchDetails);
    assertNotNull("An industry observation must be produced", xml);

    Observation withoutIndustry =
        bareOdhObs("ind-2", CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_CODE);

    String empty =
        CdaOdhDataGenerator.addPastOrPresentIndustryObservation(withoutIndustry, launchDetails);
    assertNotNull("Must return a value rather than throw when no component matches", empty);
  }
}
