package com.drajer.ecrapp.util;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import ca.uhn.fhir.parser.IParser;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.service.PlanDefinitionProcessor;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.Instant;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.hibernate.ObjectDeletedException;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.UsageContext;
import org.hl7.fhir.r4.model.ValueSet;
import org.hl7.fhir.r4.model.ValueSet.ConceptSetComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionContainsComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.logging.LogLevel;
import org.springframework.stereotype.Service;

@Service
public class ApplicationUtils {

  @Autowired
  @Qualifier("jsonParser")
  IParser jsonParser;

  public ApplicationUtils() {}

  private static final Logger logger = LoggerFactory.getLogger(ApplicationUtils.class);

  public static List<CanonicalType> getValueSetListFromGrouper(String grouperId) {

    List<CanonicalType> valueSetIdList = null;

    for (ValueSet valueset : ValueSetSingleton.getInstance().getGrouperValueSets()) {

      logger.debug("Looking for grouper value set for {}", grouperId);

      if (valueset.getId().equalsIgnoreCase(grouperId)) {

        logger.debug("Found Grouper Value Set for {}", grouperId);

        if (valueset.getCompose() != null && valueset.getCompose().getInclude() != null) {

          logger.debug("Value Set is composed of other value sets ");
          List<ConceptSetComponent> csc = valueset.getCompose().getInclude();

          for (ConceptSetComponent conceptSetComponent : csc) {

            logger.debug("Adding Value Set Ids to the list ");

            valueSetIdList = conceptSetComponent.getValueSet();
          }

          if (valueSetIdList != null && !valueSetIdList.isEmpty()) {
            logger.debug("Value Set Id List Size = {}", valueSetIdList.size());
          } else {
            logger.debug("Value Set Id List is NULL");
          }
        }
        break;
      } else {
        logger.debug("Value Set Id {}  does not match grouper Id ", valueset.getId());
      }
    }
    return valueSetIdList;
  }

  public static ValueSet getValueSetGrouperFromId(String grouperId) {

    ValueSet valueSetGrouper = null;

    for (ValueSet valueset : ValueSetSingleton.getInstance().getGrouperValueSets()) {
      if (valueset.getId().equalsIgnoreCase(grouperId)) {
        logger.debug("Grouper Id {}", grouperId);
        valueSetGrouper = valueset;
        break;
      }
    }
    return valueSetGrouper;
  }

  public static Set<ValueSet> getValueSetByIds(List<CanonicalType> valueSetIdList) {

    Set<ValueSet> valueSets = new HashSet<>();

    if (Optional.ofNullable(valueSetIdList).isPresent()) {

      logger.debug("Value Set id List is not null");

      for (CanonicalType canonicalType : valueSetIdList) {

        for (ValueSet valueSet : ValueSetSingleton.getInstance().getValueSets()) {

          if (valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())
              || ((valueSet.getUrl() != null)
                  && (valueSet.getUrl().equalsIgnoreCase(canonicalType.getValueAsString())))) {
            valueSets.add(valueSet);
            break;
          }
        }

        for (ValueSet valueSet : ValueSetSingleton.getInstance().getCovidValueSets()) {

          if (valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())
              || ((valueSet.getUrl() != null)
                  && (valueSet.getUrl().equalsIgnoreCase(canonicalType.getValueAsString())))) {
            valueSets.add(valueSet);
            break;
          }
        }
      }
    }
    return valueSets;
  }

  public static Set<ValueSet> getCovidValueSetByIds(List<CanonicalType> valueSetIdList) {

    Set<ValueSet> valueSets = new HashSet<>();

    if (Optional.ofNullable(valueSetIdList).isPresent()) {

      for (CanonicalType canonicalType : valueSetIdList) {

        logger.debug("Checking Value set {}", canonicalType.getValueAsString());

        for (ValueSet valueSet : ValueSetSingleton.getInstance().getValueSets()) {

          if (valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())
              && isACovidValueSet(valueSet)) {
            logger.debug("Found a Covid Value Set for Grouper using Id {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          } else if ((valueSet.getUrl() != null)
              && (valueSet.getUrl().equalsIgnoreCase(canonicalType.getValueAsString()))
              && isACovidValueSet(valueSet)) {

            logger.debug("Urls Matched for a Covid Value Set {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          }
        }

        for (ValueSet valueSet : ValueSetSingleton.getInstance().getCovidValueSets()) {

          if (valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())
              && isACovidValueSet(valueSet)) {

            logger.debug("Found a Covid Value Set for Grouper using Id {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          } else if ((valueSet.getUrl() != null)
              && (valueSet.getUrl().equalsIgnoreCase(canonicalType.getValueAsString()))
              && isACovidValueSet(valueSet)) {
            logger.debug("Urls Matched for a Covid Value Set {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          }
        }
      }
    }

    return valueSets;
  }

  public static Set<String> convertValueSetsToString(Set<ValueSet> valuesets) {

    Set<String> retVal = new HashSet<>();
    ValueSetExpansionComponent valueSetExpansionComponent;
    List<ValueSetExpansionContainsComponent> valueSetExpansionContainsComponentList;

    if (valuesets != null && !valuesets.isEmpty()) {
      for (ValueSet vs : valuesets) {
        logger.debug("Value Set Id = {}", vs.getId());
        valueSetExpansionComponent = vs.getExpansion();
        valueSetExpansionContainsComponentList = valueSetExpansionComponent.getContains();
        for (ValueSetExpansionContainsComponent vscomp : valueSetExpansionContainsComponentList) {
          if (vscomp.getSystem() != null && vscomp.getCode() != null) {
            logger.debug(" Code = {}", vscomp.getCode());
            retVal.add(vscomp.getSystem() + "|" + vscomp.getCode());
          }
        }
      }
    }
    return retVal;
  }

  public static Set<String> convertCodeableConceptsToString(List<CodeableConceptDt> codes) {

    Set<String> retVal = new HashSet<>();

    if (codes != null && !codes.isEmpty()) {

      for (CodeableConceptDt cd : codes) {

        List<CodingDt> cds = cd.getCoding();

        for (CodingDt code : cds) {

          if (code.getSystem() != null && code.getCode() != null) {

            retVal.add(code.getSystem() + "|" + code.getCode());
          }
        }
      }
    }

    return retVal;
  }

  public static Set<String> convertR4CodeableConceptsToString(List<CodeableConcept> codes) {

    Set<String> retVal = new HashSet<>();

    if (codes != null && !codes.isEmpty()) {

      for (CodeableConcept cd : codes) {

        List<Coding> cds = cd.getCoding();

        for (Coding code : cds) {

          if (code.getSystem() != null && code.getCode() != null) {

            retVal.add(code.getSystem() + "|" + code.getCode());
          }
        }
      }
    }

    return retVal;
  }

  public static String getCodeAsStringForMatching(String code, String cs) {

    if (code != null && cs != null) {
      return cs + "|" + code;
    }

    return null;
  }

  public static Instant convertTimingScheduleToInstant(TimingSchedule ts, Date timeRef) {

    Instant t = null;

    Duration d = new Duration();

    if (ts.getDurationUnit() != null) {

      logger.debug("Found Duration for Timing Schedule");
      logger.debug(" Duration before conversion = {}", ts.getDuration());
      logger.debug(" Duration Unit = {}", ts.getDurationUnit());
      d.setValue(ts.getDuration());
      d.setUnit(ts.getDurationUnit().toCode());
      logger.debug(" Duration during conversion = {}", d.getValue());
      logger.debug(" Duration Unit = {}", d.getUnit());

    } else if (ts.getFrequencyPeriodUnit() != null) {

      logger.debug("Found Frequency for Timing Schedule ");
      d.setValue(ts.getFrequencyPeriod());
      d.setUnit(ts.getFrequencyPeriodUnit().toCode());
    } else {

      d.setValue(0);
      d.setUnit("s");
    }

    t = convertDurationToInstant(d);

    return t;
  }

  public static Instant convertDurationToInstant(Duration d) {

    Instant t = null;

    if (d != null) {

      if (d.getUnit().equalsIgnoreCase("a")) {

        Calendar c = Calendar.getInstance();
        c.add(Calendar.YEAR, d.getValue().intValue());
        t = c.toInstant();

      } else if (d.getUnit().equalsIgnoreCase("mo")) {

        Calendar c = Calendar.getInstance();
        c.add(Calendar.MONTH, d.getValue().intValue());
        t = c.toInstant();

      } else if (d.getUnit().equalsIgnoreCase("wk")) {

        Calendar c = Calendar.getInstance();
        c.add(Calendar.DAY_OF_MONTH, (d.getValue().intValue() * 7));
        t = c.toInstant();

      } else if (d.getUnit().equalsIgnoreCase("d")) {

        Calendar c = Calendar.getInstance();
        c.add(Calendar.DAY_OF_MONTH, d.getValue().intValue());
        t = c.toInstant();

      } else if (d.getUnit().equalsIgnoreCase("h")) {

        t = new Date().toInstant().plusSeconds(d.getValue().longValue() * 60 * 60);
      } else if (d.getUnit().equalsIgnoreCase("min")) {

        t = new Date().toInstant().plusSeconds(d.getValue().longValue() * 60);
      } else if (d.getUnit().equalsIgnoreCase("s")) {

        t = new Date().toInstant().plusSeconds(d.getValue().longValue());
      } else {

        t = new Date().toInstant().plusSeconds(d.getValue().longValue());
      }
    }

    return t;
  }

  public static void saveDataToFile(String data, String fileName) {

    try (DataOutputStream outStream =
        new DataOutputStream(new BufferedOutputStream(new FileOutputStream(fileName)))) {

      logger.debug("Writing data to file: {}", fileName);
      outStream.writeBytes(data);
    } catch (IOException e) {
      logger.debug("Unable to write data to file: {}", fileName, e);
    }
  }

  public static boolean isACovidValueSet(ValueSet v) {

    boolean retVal = false;

    if (v != null) {

      logger.debug("Checking Value Set Id {}", v.getId());

      if (v.getUseContextFirstRep() != null) {

        UsageContext uc = v.getUseContextFirstRep();

        if (uc.getValue() != null && (uc.getValue() instanceof CodeableConcept)) {

          CodeableConcept cc = (CodeableConcept) uc.getValue();

          if (cc.getCodingFirstRep() != null
              && (cc.getCodingFirstRep().getCode() != null
                  && cc.getCodingFirstRep()
                      .getCode()
                      .contentEquals(PlanDefinitionProcessor.COVID_SNOMED_USE_CONTEXT_CODE))) {
            logger.debug("Found COVID VALUE SET = {}", v.getId());
            retVal = true;
          }
        }
      }
    }

    return retVal;
  }

  public static boolean isAGrouperValueSet(ValueSet v) {

    boolean retVal = false;

    if (v != null) {

      if (v.getUseContextFirstRep() != null) {

        logger.debug("Found Use Context ");

        UsageContext uc = v.getUseContextFirstRep();

        if (uc.getValue() != null && (uc.getValue() instanceof Reference)) {

          logger.debug("Found Use Context Reference ");

          Reference rr = (Reference) uc.getValue();

          if (rr != null
              && rr.getReference() != null
              && (rr.getReference().contains(PlanDefinitionProcessor.GROUPER_VALUE_SET_REFERENCE_1)
                  || rr.getReference()
                      .contains(PlanDefinitionProcessor.GROUPER_VALUE_SET_REFERENCE_2))) {

            logger.debug("Found Grouper VALUE SET = {}", v.getId());
            retVal = true;
          }
        }
      }
    }

    return retVal;
  }

  public static PatientExecutionState getDetailStatus(LaunchDetails details) {

    ObjectMapper mapper = new ObjectMapper();
    PatientExecutionState state = null;

    try {

      state = mapper.readValue(details.getStatus(), PatientExecutionState.class);

    } catch (JsonProcessingException e1) {
      String msg = "Unable to read/write execution state";
      logger.error(msg, e1);
      throw new RuntimeException(msg, e1);
    }

    return state;
  }

  public Bundle readBundleFromFile(String filename) {

    logger.info("About to read File {}", filename);
    Bundle bundle = null;
    try (InputStream in = new FileInputStream(new File(filename))) {
      logger.info("Reading File ");

      bundle = jsonParser.parseResource(Bundle.class, in);
      logger.info("Completed Reading File");
    } catch (Exception e) {
      logger.error("Exception Reading File", e);
    }
    return bundle;
  }

  public static void handleException(Exception e, String expMsg, LogLevel loglevel) {

    if (e instanceof ObjectDeletedException) {
      throw (ObjectDeletedException) e;
    }

    if (loglevel == LogLevel.INFO) {
      logger.info(expMsg, e);
    } else if (loglevel == LogLevel.WARN) {
      logger.warn(expMsg, e);
    } else if (loglevel == LogLevel.ERROR) {
      logger.error(expMsg, e);
    } else if (loglevel == LogLevel.DEBUG) {
      logger.debug(expMsg, e);
    }
  }

  public static boolean isSetContainsValueSet(Set<ValueSet> valueSets, ValueSet valueSet) {

    if (valueSets != null && valueSet != null) {
      for (ValueSet vs : valueSets) {
        if (vs.getId().equalsIgnoreCase(valueSet.getId())) {
          return true;
        }
      }
    }
    return false;
  }
}
