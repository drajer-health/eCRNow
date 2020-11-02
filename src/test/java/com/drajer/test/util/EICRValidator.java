package com.drajer.test.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.v3.POCDMT000040ClinicalDocument;
import org.hl7.v3.POCDMT000040Section;

public class EICRValidator {

  public static void validate(
      POCDMT000040ClinicalDocument clinicalDoc,
      List<String> validationSectionList,
      Map<String, List<String>> allResourceFiles) {

    for (String sectionName : validationSectionList) {
      POCDMT000040Section section = TestUtils.getSection(clinicalDoc, sectionName);
      String resourceFileName = "";
      // TODO This can be made more abstract
      if (sectionName.equalsIgnoreCase("HEADER")) {

        Patient r4Patient =
            (Patient)
                TestUtils.getR4ResourceFromJson(
                    allResourceFiles.get("Patient").get(0), Patient.class);
        Encounter r4Encounter =
            (Encounter)
                TestUtils.getR4ResourceFromJson(
                    allResourceFiles.get("Encounter").get(0), Encounter.class);
        Organization r4Organization =
            (Organization)
                TestUtils.getR4ResourceFromJson(
                    allResourceFiles.get("Organization").get(0), Organization.class);

        List<Practitioner> r4Practitioners = new ArrayList<>();
        for (String practitioner : allResourceFiles.get("Practitioner")) {
          Practitioner r4Practitioner =
              (Practitioner) TestUtils.getR4ResourceFromJson(practitioner, Practitioner.class);
          r4Practitioners.add(r4Practitioner);
        }

        ValidationUtils.validateHeader(
            r4Patient, r4Encounter, r4Practitioners, r4Organization, clinicalDoc);

      } else if (sectionName.equalsIgnoreCase("ENCOUNTERS")) {
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
      } else if (sectionName.equalsIgnoreCase("PROBLEMS")) {
        resourceFileName = "Condition";
        Bundle conditionBundle =
            TestUtils.getR4BundleFromJson(allResourceFiles.get(resourceFileName).get(0));
        List<Condition> conditions = new ArrayList<>();

        for (BundleEntryComponent entry : conditionBundle.getEntry()) {
          Condition condition = (Condition) entry.getResource();
          conditions.add(condition);
        }
        ValidationUtils.validateProblemSection(conditions, section);
      } else if (sectionName.equalsIgnoreCase("ILLNESS")) {
        resourceFileName = "Condition";
        Bundle conditionBundle =
            TestUtils.getR4BundleFromJson(allResourceFiles.get(resourceFileName).get(0));
        List<Condition> conditions = new ArrayList<>();

        for (BundleEntryComponent entry : conditionBundle.getEntry()) {
          Condition condition = (Condition) entry.getResource();
          conditions.add(condition);
        }
        ValidationUtils.validatePresentIllnessSection(conditions, section);
      } else if (sectionName.equalsIgnoreCase("HISTORY")) {
        resourceFileName = "Patient";
        Patient r4Patient =
            (Patient)
                TestUtils.getR4ResourceFromJson(
                    allResourceFiles.get(resourceFileName).get(0), Patient.class);

        List<Extension> listExtensions = r4Patient.getExtension();

        ValidationUtils.validateSocialHistory(listExtensions, section);
      } else if (sectionName.equalsIgnoreCase("IMMUNIZATIONS")) {
        resourceFileName = "Immunization";
        Bundle immunizationBundle =
            TestUtils.getR4BundleFromJson(allResourceFiles.get(resourceFileName).get(0));
        List<Immunization> immunizations = new ArrayList<>();

        for (BundleEntryComponent entry : immunizationBundle.getEntry()) {
          Immunization immunization = (Immunization) entry.getResource();
          immunizations.add(immunization);
        }
        ValidationUtils.validateImmunizationsSection(immunizations, section);
      }
      // TODO for other sections
    }
  }
}
