package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.resource.Condition;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import com.drajer.sof.model.Dstu2FhirData;
import java.util.*;
import java.util.stream.Collectors;

public class ObservationFactory {
  private static final String SNOMED_CODE_SYSTEM = "http://snomed.info/sct";
  private static final String LOINC_CODE_SYSTEM = "http://loinc.org";

  private static final Set<String> TRAVEL_HISTORY_SNOMED_CODES =
      Collections.unmodifiableSet(
          new HashSet<>(
              Arrays.asList(
                  "161085007",
                  "443846001",
                  "420008001",
                  "46521000175102",
                  "34831000175105",
                  "161086008")));

  private static final Set<String> OCCUPATION_SNOMED_CODES =
      Collections.unmodifiableSet(new HashSet<>(Arrays.asList("224362002", "364703007")));

  private static final Set<String> OCCUPATION_LOINC_CODES =
      Collections.unmodifiableSet(new HashSet<>(Arrays.asList("11295-3", "11341-5")));

  private static final Set<String> PREGNANCY_SNOMED_CODES =
      Collections.unmodifiableSet(new HashSet<>(Arrays.asList("77386006")));

  public static List<Observation> filterPregnancyObservations(List<Observation> obsList) {
    return obsList
        .stream()
        .filter(obs -> hasMatchingCode(obs.getCode(), SNOMED_CODE_SYSTEM, PREGNANCY_SNOMED_CODES))
        .collect(Collectors.toList());
  }

  public static List<Observation> filterTravelHistory(List<Observation> obsList) {
    return obsList
        .stream()
        .filter(
            obs -> hasMatchingCode(obs.getCode(), SNOMED_CODE_SYSTEM, TRAVEL_HISTORY_SNOMED_CODES))
        .collect(Collectors.toList());
  }

  public static List<Condition> filterPregnancyConditions(List<Condition> condList) {
    return condList
        .stream()
        .filter(cond -> hasMatchingCode(cond.getCode(), SNOMED_CODE_SYSTEM, PREGNANCY_SNOMED_CODES))
        .collect(Collectors.toList());
  }

  public static List<Observation> filterOccupationObservations(List<Observation> obsList) {
    return obsList
        .stream()
        .filter(
            obs ->
                hasMatchingCode(obs.getCode(), SNOMED_CODE_SYSTEM, OCCUPATION_SNOMED_CODES)
                    || hasMatchingCode(obs.getCode(), LOINC_CODE_SYSTEM, OCCUPATION_LOINC_CODES))
        .collect(Collectors.toList());
  }

  private static boolean hasMatchingCode(
      CodeableConceptDt codeableConcept, String system, Set<String> allowedCodes) {
    if (codeableConcept == null || codeableConcept.getCoding() == null) return false;
    return codeableConcept
        .getCoding()
        .stream()
        .anyMatch(c -> system.equals(c.getSystem()) && allowedCodes.contains(c.getCode()));
  }

  public static Dstu2FhirData applyAllFilters(
      Dstu2FhirData data, List<Observation> obsList, List<Condition> condList) {
    if (data != null) {

      if (obsList != null && !obsList.isEmpty()) {

        data.setPregnancyObs(filterPregnancyObservations(obsList));
        data.setTravelObs(filterTravelHistory(obsList));
        data.setOccupationObs(filterOccupationObservations(obsList));
      } else if (condList != null && !condList.isEmpty()) {

        data.setPregnancyConditions(filterPregnancyConditions(condList));
      }
      return data;
    }

    return data;
  }
}
