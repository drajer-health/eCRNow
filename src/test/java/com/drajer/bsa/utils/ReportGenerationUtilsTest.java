package com.drajer.bsa.utils;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.test.simulator.ContentDataSimulator;
import java.io.InputStream;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.core.io.ClassPathResource;

@RunWith(MockitoJUnitRunner.class)
public class ReportGenerationUtilsTest {

  public static final FhirContext fhirContext = FhirContext.forR4();
  private static Set<Resource> resources = new HashSet<>();
  public static final String SMOKING_STATUS_OBS = "/R4/Observation/SmokingStatus.json";
  public static final String OBSERVATION_BUNDLE =
      "/R4/Observation/ObservationWithCovidTriggerCode.json";

  public static final String DIAGNOSTIC_REPORT = "/R4/DiagnosticReport/DiagnosticReports.json";
  public static final String EXCEPTION_READING_FILE = "Exception Reading File";

  @Test
  public void testFilterObservationsByCategory() {
    FhirContext fhirContext = FhirContext.forR4();
    Bundle bundle =
        fhirContext
            .newJsonParser()
            .parseResource(
                Bundle.class,
                ReportGenerationUtilsTest.class.getResourceAsStream(
                    "/R4/Observation/ObservationStart.json"));
    bundle
        .getEntry()
        .forEach(
            (e) -> {
              resources.add(e.getResource());
            });
    Set<Resource> filteredObservation =
        ReportGenerationUtils.filterObservationsByCategory(resources, "social-history");
    assertEquals(6, filteredObservation.size());
  }

  @Test
  public void testHasCode_MatchingLoincCodeExists() {
    CodeableConcept codeableConcept = new CodeableConcept();
    codeableConcept.setCoding(
        Arrays.asList(
            new Coding().setSystem("http://loinc.org").setCode("12345-6"),
            new Coding().setSystem("http://loinc.org").setCode("67890-1")));

    assertTrue(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", codeableConcept));
  }

  @Test
  public void testHasCode_NoMatchingLoincCode() {
    CodeableConcept codeableConcept = new CodeableConcept();
    codeableConcept.setCoding(
        Arrays.asList(
            new Coding().setSystem("http://loinc.org").setCode("54321-0"),
            new Coding().setSystem("http://loinc.org").setCode("98765-4")));

    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", codeableConcept));
  }

  @Test
  public void testHasCode_NullCodeableConcept() {
    CodeableConcept cd = null;
    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", cd));
  }

  @Test
  public void testHasCode_EmptyCodingList() {
    CodeableConcept codeableConcept = new CodeableConcept();
    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", codeableConcept));
  }

  @Test
  public void testHasCode_NullCodings() {
    Coding cd = null;
    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", cd));
  }

  @Test
  public void testHasCode_CodingsWithoutSystem() {
    Coding coding = new Coding();
    coding.setCode("12345-6");

    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", coding));
  }

  @Test
  public void testHasCode_CodingsWithoutCode() {
    Coding coding = new Coding();
    coding.setSystem("http://loinc.org");

    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", coding));
  }

  @Test
  public void testHasCode_MatchingLoincCode() {
    Coding coding = new Coding();
    coding.setSystem("http://loinc.org");
    coding.setCode("12345-6");

    assertTrue(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", coding));
  }

  @Test
  public void testHasCode_WrongCode() {
    Coding coding = new Coding();
    coding.setSystem("http://loinc.org");
    coding.setCode("67890-1");

    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", coding));
  }

  @Test
  public void testHasCode_WrongSystem() {
    Coding coding = new Coding();
    coding.setSystem("http://snomed.info/sct");
    coding.setCode("12345-6");

    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", coding));
  }

  @Test
  public void testHasCode_NullCoding() {
    Coding cd = null;
    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", cd));
  }

  @Test
  public void testHasCode_CodingWithoutSystem() {
    Coding coding = new Coding();
    coding.setCode("12345-6");

    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", coding));
  }

  @Test
  public void testHasCode_CodingWithoutCode() {
    Coding coding = new Coding();
    coding.setSystem("http://loinc.org");

    assertFalse(ReportGenerationUtils.hasCode("http://loinc.org", "12345-6", coding));
  }

  @Test
  public void testGetTextForCodings_MultipleDisplays() {
    Coding coding1 = new Coding().setDisplay("Display 1");
    Coding coding2 = new Coding().setDisplay("Display 2");

    List<Coding> codings = Arrays.asList(coding1, coding2);

    String result = ReportGenerationUtils.getTextForCodings(codings);

    assertEquals("Display 1, Display 2", result);
  }

  @Test
  public void testGetTextForCodings_SingleDisplay() {
    Coding coding1 = new Coding().setDisplay("Display 1");

    List<Coding> codings = Collections.singletonList(coding1);

    String result = ReportGenerationUtils.getTextForCodings(codings);

    assertEquals("Display 1", result);
  }

  @Test
  public void testGetTextForCodings_NoDisplay() {
    Coding coding1 = new Coding();

    List<Coding> codings = Collections.singletonList(coding1);

    String result = ReportGenerationUtils.getTextForCodings(codings);

    assertEquals("No Information", result);
  }

  @Test
  public void testGetTextForCodings_EmptyList() {
    List<Coding> codings = Collections.emptyList();

    String result = ReportGenerationUtils.getTextForCodings(codings);

    assertEquals("No Information", result);
  }

  @Test
  public void testGetTextForCodings_NullList() {
    List<Coding> codings = null;

    String result = ReportGenerationUtils.getTextForCodings(codings);

    assertEquals("No Information", result);
  }

  @Test
  public void testGetTextForCodings_MixedWithAndWithoutDisplay() {
    Coding coding1 = new Coding().setDisplay("Display 1");
    Coding coding2 = new Coding();

    List<Coding> codings = Arrays.asList(coding1, coding2);

    String result = ReportGenerationUtils.getTextForCodings(codings);

    assertEquals("Display 1", result);
  }

  @Test
  public void testGetTextForCodeableConcepts_WithText() {
    CodeableConcept cc1 = new CodeableConcept();
    cc1.setText("Text 1");

    CodeableConcept cc2 = new CodeableConcept();
    cc2.setText("Text 2");

    List<CodeableConcept> ccs = Arrays.asList(cc1, cc2);

    String result = ReportGenerationUtils.getTextForCodeableConcepts(ccs);

    assertEquals("Text 1, Text 2", result);
  }

  @Test
  public void testGetTextForCodeableConcepts_WithoutText_ButWithCoding() {
    Coding coding1 = new Coding().setCode("12345-6").setSystem("http://loinc.org");
    CodeableConcept cc1 = new CodeableConcept();
    cc1.addCoding(coding1);

    CodeableConcept cc2 = new CodeableConcept();

    List<CodeableConcept> ccs = Arrays.asList(cc1, cc2);

    String result = ReportGenerationUtils.getTextForCodeableConcepts(ccs);

    assertTrue(result.contains("Information:"));
  }

  @Test
  public void testGetTextForCodeableConcepts_WithoutText_WithoutCoding() {
    CodeableConcept cc1 = new CodeableConcept();
    CodeableConcept cc2 = new CodeableConcept();

    List<CodeableConcept> ccs = Arrays.asList(cc1, cc2);

    String result = ReportGenerationUtils.getTextForCodeableConcepts(ccs);

    assertEquals("Information: ", result);
  }

  @Test
  public void testGetTextForCodeableConcepts_EmptyList() {
    List<CodeableConcept> ccs = Collections.emptyList();

    String result = ReportGenerationUtils.getTextForCodeableConcepts(ccs);

    assertEquals("No Information", result);
  }

  @Test
  public void testGetTextForCodeableConcepts_NullList() {
    List<CodeableConcept> ccs = null;

    String result = ReportGenerationUtils.getTextForCodeableConcepts(ccs);

    assertEquals("No Information", result);
  }

  @Test
  public void testGetTextForCodeableConcepts_MixedWithTextAndCoding() {
    Coding coding1 = new Coding().setCode("12345-6").setSystem("http://loinc.org");
    CodeableConcept cc1 = new CodeableConcept();
    cc1.addCoding(coding1);
    cc1.setText("Text 1");

    CodeableConcept cc2 = new CodeableConcept();

    List<CodeableConcept> ccs = Arrays.asList(cc1, cc2);

    String result = ReportGenerationUtils.getTextForCodeableConcepts(ccs);

    assertEquals("Text 1", result);
  }

  @Test
  public void getSmokingStatusObservationTest() {
    Bundle bundle = loadBundleFromFile(SMOKING_STATUS_OBS);
    Set<Resource> obsSet = new HashSet<>();
    for (Bundle.BundleEntryComponent data : bundle.getEntry()) {
      obsSet.add(data.getResource());
    }
    Set<Resource> resourceSet = ReportGenerationUtils.getSmokingStatusObservation(obsSet);
    assertNotNull(resourceSet);
  }

  @Test
  public void getSmokingStatusObservationTestWithEmptyResouce() {
    Set<Resource> obsSet = new HashSet<>();
    Set<Resource> resourceSet = ReportGenerationUtils.getSmokingStatusObservation(obsSet);
    assertTrue(resourceSet.isEmpty());
  }

  @Test
  public void getSmokingStatusObservationTestWithNullResouce() {
    Set<Resource> resourceSet = ReportGenerationUtils.getSmokingStatusObservation(null);
    assertTrue(resourceSet.isEmpty());
  }

  @Test
  public void getSmokingStatusObservationTestWithResouce() {

    Set<Resource> obsSet = new HashSet<>();
    obsSet.add(new Observation());
    Set<Resource> resourceSet = ReportGenerationUtils.getSmokingStatusObservation(null);
    assertTrue(resourceSet.isEmpty());
  }

  @Test
  public void testFilterDiagnosticReports_withResults() {
    Bundle bundle = loadBundleFromFile(DIAGNOSTIC_REPORT);
    Set<Resource> diagSet = new HashSet<>();
    for (Bundle.BundleEntryComponent data : bundle.getEntry()) {
      diagSet.add(data.getResource());
    }
    Set<Resource> resourceSet = ReportGenerationUtils.filterDiagnosticReports(diagSet, true);
    assertNotNull(resourceSet);
    assertFalse(resourceSet.isEmpty());
  }

  @Test
  public void testFilterDiagnosticReport_withNoResults() {
    Bundle bundle = loadBundleFromFile(DIAGNOSTIC_REPORT);
    Set<Resource> diagSet = new HashSet<>();
    for (Bundle.BundleEntryComponent data : bundle.getEntry()) {
      diagSet.add(data.getResource());
    }
    Set<Resource> resourceSet = ReportGenerationUtils.filterDiagnosticReports(diagSet, false);
    assertNotNull(resourceSet);
    assertFalse(resourceSet.isEmpty());
  }

  @Test
  public void testFilterDiagnosticReports_nullInput() {
    Set<Resource> filtered = ReportGenerationUtils.filterDiagnosticReports(null, true);
    assertTrue(filtered.isEmpty());
  }

  @Test
  public void testFilterDiagnosticReports_emptySet() {
    Set<Resource> filtered = ReportGenerationUtils.filterDiagnosticReports(new HashSet<>(), true);
    assertTrue(filtered.isEmpty());
  }

  public Bundle loadBundleFromFile(String filename) {
    try (InputStream in = new ClassPathResource(filename).getInputStream()) {
      return fhirContext.newJsonParser().parseResource(Bundle.class, in);
    } catch (Exception e) {
      ContentDataSimulator.logger.error(EXCEPTION_READING_FILE, e);
      return null;
    }
  }

  @Test
  public void getFilterSocialHistoryObservations() {
    Bundle bundle = loadBundleFromFile(SMOKING_STATUS_OBS);
    Set<Resource> obsSet = new HashSet<>();
    for (Bundle.BundleEntryComponent data : bundle.getEntry()) {
      obsSet.add(data.getResource());
    }
    Set<Resource> resourceSet = ReportGenerationUtils.filterSocialHistoryObservations(obsSet);
    assertFalse(resourceSet.isEmpty());
  }

  @Test
  public void getFilterSocialHistoryObservations_withNoSocialHistoryInput() {
    Bundle bundle = loadBundleFromFile(OBSERVATION_BUNDLE);
    Set<Resource> obsSet = new HashSet<>();
    for (Bundle.BundleEntryComponent data : bundle.getEntry()) {
      obsSet.add(data.getResource());
    }
    Set<Resource> resourceSet = ReportGenerationUtils.filterSocialHistoryObservations(obsSet);
    assertTrue(resourceSet.isEmpty());
  }

  @Test
  public void getFilterSocialHistoryObservations_withNoInput() {
    Set<Resource> resourceSet = ReportGenerationUtils.filterSocialHistoryObservations(null);
    assertTrue(resourceSet.isEmpty());
  }

  @Test
  public void getFilterSocialHistoryObservations_withEmptyInput() {
    Set<Resource> resourceSet =
        ReportGenerationUtils.filterSocialHistoryObservations(new HashSet<>());
    assertTrue(resourceSet.isEmpty());
  }
}
