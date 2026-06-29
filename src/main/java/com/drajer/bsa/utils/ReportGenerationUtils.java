package com.drajer.bsa.utils;

import com.drajer.cdafromr4.CdaFhirUtilities;
import com.drajer.fhirecr.FhirGeneratorConstants;
import java.util.*;
import java.util.stream.Collectors;
import org.hl7.fhir.dstu3.model.codesystems.ObservationCategory;
import org.hl7.fhir.r4.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReportGenerationUtils {

  private static final Logger logger = LoggerFactory.getLogger(ReportGenerationUtils.class);

  private static final String SMOKING_STATUS_CODE = "72166-2";

  private static final Set<String> PREGNANCY_CODES =
      Set.of(
          "249197004", // Postpartum - SNOMED
          "63893-2", // Pregnancy Outcome - LOINC
          "82810-3" // Pregnancy Status - LOINC
          );

  private static final List<Map<String, String>> SOCIAL_HISTORY_CODES =
      List.of(

          // Section code
          Map.of("system", "http://loinc.org", "code", "29762-2"),

          // Tobacco / Smoking
          Map.of("system", "http://loinc.org", "code", "72166-2"),
          Map.of("system", "http://loinc.org", "code", "11367-0"),
          Map.of("system", "http://snomed.info/sct", "code", "229819007"),
          Map.of("system", "http://snomed.info/sct", "code", "365980008"),

          // Alcohol use
          Map.of("system", "http://loinc.org", "code", "74013-4"),
          Map.of("system", "http://loinc.org", "code", "11343-1"),
          Map.of("system", "http://snomed.info/sct", "code", "228273003"),

          // Substance / Drug use
          Map.of("system", "http://loinc.org", "code", "11344-9"),
          Map.of("system", "http://snomed.info/sct", "code", "228366006"),

          // Occupational Data for Health (ODH)
          Map.of("system", "http://loinc.org", "code", "11341-5"),
          Map.of("system", "http://loinc.org", "code", "21843-8"),
          Map.of("system", "http://loinc.org", "code", "87510-4"),
          Map.of("system", "http://loinc.org", "code", "74165-2"),
          Map.of("system", "http://snomed.info/sct", "code", "364703007"),

          // Travel History
          Map.of("system", "http://loinc.org", "code", "8691-8"),
          Map.of("system", "http://snomed.info/sct", "code", "420008001"),

          // Home Environment
          Map.of("system", "http://loinc.org", "code", "71802-3"),
          Map.of("system", "http://snomed.info/sct", "code", "224229001"),

          // Disability Status
          Map.of("system", "http://loinc.org", "code", "69858-6"),
          Map.of("system", "http://snomed.info/sct", "code", "363787002"),

          // Country / Residence / Nationality
          Map.of("system", "http://loinc.org", "code", "77983-5"),
          Map.of("system", "http://snomed.info/sct", "code", "186034007"),
          Map.of("system", "http://loinc.org", "code", "46463-6"),

          // Exposure / Contact Information
          Map.of("system", "http://loinc.org", "code", "85657-3"),
          Map.of("system", "http://snomed.info/sct", "code", "418038007"),

          // General social context
          Map.of("system", "http://snomed.info/sct", "code", "365508006"));

  private ReportGenerationUtils() {}

  public static Set<Resource> filterObservationsByCategory(Set<Resource> res, String category) {

    logger.info(" Getting observations for category {}", category);
    Set<Resource> returnVal = new HashSet<>();
    if (res != null) {
      for (Resource r : res) {

        Observation obs = (Observation) (r);

        if (obs.getCategoryFirstRep() != null
            && obs.getCategoryFirstRep().getCodingFirstRep() != null
            && obs.getCategoryFirstRep().getCodingFirstRep().getSystem() != null
            && obs.getCategoryFirstRep()
                .getCodingFirstRep()
                .getSystem()
                .contentEquals(FhirGeneratorConstants.HL7_OBSERVATION_CATEGORY)
            && obs.getCategoryFirstRep().getCodingFirstRep().getCode() != null
            && obs.getCategoryFirstRep().getCodingFirstRep().getCode().contentEquals(category)) {

          returnVal.add(r);
        }
      }
    }
    return returnVal;
  }

  public static Set<Resource> filterSocialHistoryObservations(Set<Resource> res) {

    Set<Resource> returnVal = new HashSet<>();
    if (res != null) {
      logger.info(" Total No of Observations = {}", res.size());

      for (Resource r : res) {

        Observation obs = (Observation) (r);

        if (!observationBelongsToCategory(obs, ObservationCategory.LABORATORY.toCode())
            && !observationBelongsToCategory(obs, ObservationCategory.VITALSIGNS.toCode())) {

          returnVal.add(r);
        }
      }
    }

    return returnVal;
  }

  public static Boolean observationBelongsToCategory(Observation obs, String category) {

    Boolean retVal = false;
    if (obs != null && obs.hasCategory()) {

      List<CodeableConcept> cds = obs.getCategory();

      for (CodeableConcept cd : cds) {

        if (cd.hasCoding()) {

          List<Coding> codings = cd.getCoding();

          for (Coding c : codings) {

            if (c.getSystem().contentEquals(FhirGeneratorConstants.HL7_OBSERVATION_CATEGORY)
                && c.getCode().contentEquals(category)) {

              retVal = true;
              break;
            }
          }
        }
      }
    }

    return retVal;
  }

  public static Set<Resource> filterDiagnosticReports(Set<Resource> res, Boolean resultFlag) {

    logger.info(" Filtering Diagnostic Reports with {}", (resultFlag ? "results" : "no results"));
    Set<Resource> returnVal = new HashSet<>();

    if (res != null && resultFlag) {

      returnVal =
          res.stream()
              .map(dr -> (DiagnosticReport) (dr))
              .filter(r -> r.hasResult())
              .collect(Collectors.toSet());
    } else if (res != null) {

      returnVal =
          res.stream()
              .map(dr -> (DiagnosticReport) (dr))
              .filter(r -> !(r.hasResult()))
              .collect(Collectors.toSet());
    }

    return returnVal;
  }

  public static Set<Resource> filterPregnancyObservations(Set<Resource> resources) {

    logger.info("Filtering Pregnancy Observations");

    if (resources == null) {
      return Collections.emptySet();
    }

    return resources.stream()
        .filter(r -> r instanceof Observation)
        .map(r -> (Observation) r)
        .filter(obs -> obs.hasCode() && obs.getCode().hasCoding())
        .filter(
            obs ->
                obs.getCode().getCoding().stream()
                    .anyMatch(coding -> PREGNANCY_CODES.contains(coding.getCode())))
        .collect(Collectors.toSet());
  }

  public static Set<Resource> getSmokingStatusObservation(Set<Resource> res) {

    logger.info(" Getting Smoking Status observation ");
    Set<Resource> returnVal = new HashSet<>();
    if (res != null) {
      for (Resource r : res) {

        Observation obs = (Observation) (r);

        if (obs.hasCode()) {
          CodeableConcept cd = obs.getCode();

          if (hasCode(FhirGeneratorConstants.LOINC_CS_URL, SMOKING_STATUS_CODE, cd)) {

            returnVal.add(r);
            break;
          }
        }
      }
    }
    return returnVal;
  }

  public static Boolean hasCode(String csUrl, String code, CodeableConcept cd) {

    if (cd != null) {

      List<Coding> cds = cd.getCoding();

      for (Coding coding : cds) {

        if (hasCode(csUrl, code, coding)) return true;
      }
    }
    return false;
  }

  public static Boolean hasCode(String csUrl, String code, Coding coding) {

    if (coding != null) {

      if (coding.hasSystem()
          && coding.getSystem().contentEquals(csUrl)
          && coding.hasCode()
          && coding.getCode().contentEquals(code)) {
        return true;
      }
    }
    return false;
  }

  public static String getTextForCodeableConcepts(List<CodeableConcept> ccs) {

    String text = "No Information";
    Boolean found = false;
    String codingText = "Information: ";

    if (ccs != null && !ccs.isEmpty()) {

      for (CodeableConcept cc : ccs) {

        if (cc.hasText() && !found) {
          text = cc.getText();
          found = true;
        } else if (cc.hasText() && found) {
          text += ", " + cc.getText();
        } else if (!cc.hasText() && cc.hasCoding()) {
          codingText += getTextForCodings(cc.getCoding());
        }
      }

      // If no text was present, using Coding text.
      if (!found) {
        text = codingText;
      }
    }
    return text;
  }

  public static String getTextForCodings(List<Coding> ccs) {

    String text = "No Information";
    Boolean found = false;

    if (ccs != null && !ccs.isEmpty()) {
      for (Coding cc : ccs) {

        if (cc.hasDisplay() && !found) {
          text = cc.getDisplay();
          found = true;
        } else if (cc.hasDisplay() && found) {
          text += ", " + cc.getDisplay();
        }
      }
    }
    return text;
  }

  public static Set<Resource> filterPregancyObservationn(
      Set<Resource> res, List<Map<String, Object>> pregnancyCodes) {

    logger.info(" Getting observations for codes {}", pregnancyCodes);
    Set<Resource> returnVal = new HashSet<>();
    if (res != null) {
      for (Resource r : res) {

        Observation obs = (Observation) (r);

        if (obs.hasCode() && obs.getCode().hasCoding()) {
          for (Map<String, Object> pregnancyCodeMap : pregnancyCodes) {
            String system = (String) pregnancyCodeMap.get("system");
            String code = (String) pregnancyCodeMap.get("code");
            if (CdaFhirUtilities.isCodePresent(
                Collections.singletonList(obs.getCode()), code, system)) {
              returnVal.add(r);
            }
          }
        }
      }
    }
    return returnVal;
  }

  public static Set<Resource> filterSocialHistoryObservations(
      Set<Resource> resources, List<Map<String, String>> socialHistoryCodes) {

    logger.info("Getting social history observations");

    Set<Resource> filteredResources = new HashSet<>();

    if (resources != null) {

      for (Resource resource : resources) {

        if (!(resource instanceof Observation)) {
          continue;
        }

        Observation observation = (Observation) resource;

        if (observation.hasCode() && observation.getCode().hasCoding()) {

          for (Map<String, String> codeMap : socialHistoryCodes) {

            String system = codeMap.get("system");
            String code = codeMap.get("code");

            if (CdaFhirUtilities.isCodePresent(
                Collections.singletonList(observation.getCode()), code, system)) {

              filteredResources.add(resource);
              break;
            }
          }
        }
      }
    }

    return filteredResources;
  }

  public static Boolean isPregnancySection(Composition.SectionComponent sc) {

    if (sc == null || !sc.hasCode() || !sc.getCode().hasCoding()) {
      return false;
    }

    return sc.getCode().getCoding().stream()
        .anyMatch(
            coding ->
                "http://loinc.org".equalsIgnoreCase(coding.getSystem())
                    && "90767-5".equals(coding.getCode()));
  }

  public static Boolean isSocialHistorySection(Composition.SectionComponent sc) {

    if (sc == null || !sc.hasCode() || !sc.getCode().hasCoding()) {
      return false;
    }

    return sc.getCode().getCoding().stream()
        .anyMatch(
            coding ->
                "http://loinc.org".equalsIgnoreCase(coding.getSystem())
                    && "29762-2".equals(coding.getCode()));
  }
}
