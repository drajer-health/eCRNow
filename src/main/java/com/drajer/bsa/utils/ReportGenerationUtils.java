package com.drajer.bsa.utils;

import com.drajer.fhirecr.FhirGeneratorConstants;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
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
}
