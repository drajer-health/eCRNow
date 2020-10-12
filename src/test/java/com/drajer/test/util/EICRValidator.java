package com.drajer.test.util;

import java.util.List;
import java.util.Map;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.v3.POCDMT000040ClinicalDocument;
import org.hl7.v3.POCDMT000040Section;

public class EICRValidator {

  public static void validate(
      POCDMT000040ClinicalDocument clinicalDoc,
      List<String> validationSectionList,
      Map<String, List<String>> allResourceFiles) {

    for (String sectionName : validationSectionList) {
      POCDMT000040Section section = ValidationUtils.getSection(clinicalDoc, sectionName);
      String resourceFileName = "";
      // TODO This can be made more abstract
      if (sectionName.equalsIgnoreCase("ENCOUNTERS")) {
        resourceFileName = "Encounter";
        Encounter r4Encounter =
            (Encounter)
                TestUtils.getR4ResourceFromJson(
                    allResourceFiles.get(resourceFileName).get(0), Encounter.class);
        ValidationUtils.validateEncounterSection(r4Encounter, section);

      } else if (sectionName.equalsIgnoreCase("VISITS")) {
        resourceFileName = "Encounter";
        Encounter r4Encounter =
            (Encounter)
                TestUtils.getR4ResourceFromJson(
                    allResourceFiles.get(resourceFileName).get(0), Encounter.class);
        ValidationUtils.validateReasonForVisitSection(r4Encounter, section);
      }
      // TODO for other sections
    }
  }
}
