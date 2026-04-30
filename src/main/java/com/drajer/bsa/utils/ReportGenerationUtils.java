package com.drajer.bsa.utils;

import com.drajer.cdafromr4.CdaFhirUtilities;
import com.drajer.fhirecr.FhirGeneratorConstants;
import java.util.*;
import java.util.stream.Collectors;
import org.hl7.fhir.dstu3.model.codesystems.ObservationCategory;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReportGenerationUtils {

  private static final Logger logger = LoggerFactory.getLogger(ReportGenerationUtils.class);

  private static final String SMOKING_STATUS_CODE = "72166-2";

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
}
