package com.drajer.test.util;

import java.util.List;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.v3.POCDMT000040ClinicalDocument;
import org.hl7.v3.POCDMT000040Section;

public class EICRValidator {

  public static void validate(
      POCDMT000040ClinicalDocument clinicalDoc, String sectionName, List<String> resourceFiles) {
    POCDMT000040Section section = ValidationUtils.getSection(clinicalDoc, sectionName);

    if (sectionName.equalsIgnoreCase("ENCOUNTERS")) {
      Encounter r4Encounter =
          (Encounter) TestUtils.getR4ResourceFromJson(resourceFiles.get(0), Encounter.class);
      ValidationUtils.validateEncounterSection(r4Encounter, section);
    } else if (sectionName.equalsIgnoreCase("VISITS")) {
      Encounter r4Encounter =
          (Encounter) TestUtils.getR4ResourceFromJson(resourceFiles.get(0), Encounter.class);
      ValidationUtils.validateReasonForVisitSection(r4Encounter, section);
    }
  }
}
