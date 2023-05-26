package com.drajer.bsa.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;

import ca.uhn.fhir.context.FhirContext;
import java.util.HashSet;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Resource;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class ReportGenerationUtilsTest {

  private static Set<Resource> resources = new HashSet<>();

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
}
