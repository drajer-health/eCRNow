package com.drajer.ecrapp.util;

import ca.uhn.fhir.context.FhirContext;
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
import java.util.TimeZone;
import org.apache.commons.text.StringEscapeUtils;
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

  public static final String EXCEPTION_READING_FILE = "Exception Reading File";

  @Autowired
  @Qualifier("jsonParser")
  IParser jsonParser;

  public ApplicationUtils() {}

  private static final Logger logger = LoggerFactory.getLogger(ApplicationUtils.class);

  public static List<CanonicalType> getValueSetListFromGrouper(String grouperId) {

    List<CanonicalType> valueSetIdList = null;

    for (ValueSet valueset : ValueSetSingleton.getInstance().getGrouperValueSets()) {

      logger.debug("Looking for grouper value set for {}", grouperId);

      if (valueset.getUrl() != null && valueset.getUrl().equals(grouperId)) {

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
        logger.debug(
            "Value Set Id {}  does not match grouper Id : {}", valueset.getId(), grouperId);
      }
    }
    return valueSetIdList;
  }

  public static ValueSet getValueSetGrouperFromId(String grouperId) {

    ValueSet valueSetGrouper = null;

    for (ValueSet valueset : ValueSetSingleton.getInstance().getGrouperValueSets()) {
      if (valueset.getUrl() != null && valueset.getUrl().equals(grouperId)) {
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

        for (ValueSet valueSet : ValueSetSingleton.getInstance().getEmergentValueSets()) {

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

  public static Set<ValueSet> getEmergentValueSetByIds(List<CanonicalType> valueSetIdList) {

    Set<ValueSet> valueSets = new HashSet<>();

    if (Optional.ofNullable(valueSetIdList).isPresent()) {

      for (CanonicalType canonicalType : valueSetIdList) {

        logger.debug("Checking Value set {}", canonicalType.getValueAsString());

        for (ValueSet valueSet : ValueSetSingleton.getInstance().getValueSets()) {

          if (valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())
              && isAEmergentValueSet(valueSet)) {
            logger.debug("Found a Emergent Value Set for Grouper using Id {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          } else if ((valueSet.getUrl() != null)
              && (valueSet.getUrl().equalsIgnoreCase(canonicalType.getValueAsString()))
              && isAEmergentValueSet(valueSet)) {

            logger.debug("Urls Matched for a Emergent Value Set {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          }
        }

        for (ValueSet valueSet : ValueSetSingleton.getInstance().getEmergentValueSets()) {

          if (valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())
              && isAEmergentValueSet(valueSet)) {

            logger.debug("Found a Emergent Value Set for Grouper using Id {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          } else if ((valueSet.getUrl() != null)
              && (valueSet.getUrl().equalsIgnoreCase(canonicalType.getValueAsString()))
              && isAEmergentValueSet(valueSet)) {
            logger.debug("Urls Matched for a Emergent Value Set {}", valueSet.getId());
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
    logger.info("Time Ref in convertTimingScheduleToInstant:{}", timeRef);

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

  public static Instant getInstantForOffHours(
      Duration d,
      Integer offhourStart,
      Integer offhoursStartMin,
      Integer offhourEnd,
      Integer offhoursEndMin,
      String tz) {
    logger.info("OffHours Duration ={}", d);

    Instant t = null;

    logger.info(
        " OffHour Parameters: Start = {}, End = {} , Tz = {} ", offhourStart, offhourEnd, tz);

    t =
        calculateNewTimeForTimer(
            offhourStart,
            offhoursStartMin,
            offhourEnd,
            offhoursEndMin,
            tz,
            d,
            (new Date().toInstant()));

    return t;
  }

  public static Instant calculateNewTimeForTimer(
      Integer lowHours,
      Integer lowMin,
      Integer highHours,
      Integer highMin,
      String tz,
      Duration d,
      Instant currentTime) {

    if (lowHours != null && lowMin != null && highHours != null && highMin != null && tz != null) {

      int totalHours = 0;

      logger.debug(
          " LowHours: {}, LowMin: {}, HighHours: {}, HighMin: {}",
          lowHours,
          lowMin,
          highHours,
          highMin);

      if (lowHours < highHours) {
        totalHours = highHours - lowHours;
      } else {
        totalHours = (24 - lowHours) + highHours;
      }

      if (lowMin != 0) lowMin = 60 - lowMin;
      else lowMin = 0;

      int totalOffHourMin = (totalHours * 60) - lowMin + highMin;
      int totalBusyHourMin = (24 * 60) - totalOffHourMin;

      logger.info(
          " Total off Hour Min = {}, Total Busy Hour Min {}", totalOffHourMin, totalBusyHourMin);

      Calendar offHoursStartTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
      offHoursStartTime.set(Calendar.HOUR_OF_DAY, lowHours);
      offHoursStartTime.set(Calendar.MINUTE, lowMin);

      Calendar offHoursEndTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));

      if (highHours < lowHours) {

        // The off hours start today at low hours and end tomorrow at high hours
        offHoursEndTime.add(Calendar.DATE, 1);
        offHoursEndTime.set(Calendar.HOUR_OF_DAY, highHours);
        offHoursEndTime.set(Calendar.MINUTE, highMin);
      } else {
        offHoursEndTime.set(Calendar.HOUR_OF_DAY, highHours);
        offHoursEndTime.set(Calendar.MINUTE, highMin);
      }

      logger.info(
          " offHoursStart: {}, offHoursEnd {}, currentTime {}",
          offHoursStartTime.getTime().toInstant().toString(),
          offHoursEndTime.getTime().toInstant().toString(),
          currentTime.toString());

      Calendar busyHourStart = null;
      Boolean isBefore = false;

      // If the current time when the timer expired is between the offhours start and end,
      // then return the current time.
      if ((currentTime.isAfter(offHoursStartTime.getTime().toInstant())
              || currentTime.equals(offHoursStartTime.getTime().toInstant()))
          && (currentTime.isBefore(offHoursEndTime.getTime().toInstant())
              || currentTime.equals(offHoursEndTime.getTime().toInstant()))) {

        logger.info(" Returning the same instant for the next day ");
        Calendar retVal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        retVal.add(Calendar.DATE, 1);

        return retVal.getTime().toInstant();
      } else {

        if (currentTime.isBefore(offHoursStartTime.getTime().toInstant())) {

          busyHourStart = offHoursEndTime;
          busyHourStart.add(Calendar.DATE, -1);
          isBefore = true;

        } else {

          busyHourStart = offHoursEndTime;
        }
      }

      long offsetMinutes =
          java.time.Duration.between(busyHourStart.getTime().toInstant(), currentTime)
              .abs()
              .toMinutes();

      // Calculate the new Instant
      int newTimeOffset = ((int) offsetMinutes * totalOffHourMin) / totalBusyHourMin;

      logger.info(
          " Current Time: {} , busyHourStart: {} , OffsetMinutes: {}, newTimeOffset {}",
          currentTime.toString(),
          busyHourStart.getTime().toInstant().toString(),
          offsetMinutes,
          newTimeOffset);

      Calendar newStart = null;
      if (isBefore) {

        newStart = offHoursStartTime;
        logger.debug(" Adding Minutes to New Start Time = {}", newStart.getTime().toInstant());
        newStart.add(Calendar.MINUTE, newTimeOffset);

      } else {

        newStart = offHoursStartTime;
        logger.debug(" Adding Day to New Start Time = {}", newStart.getTime().toInstant());
        newStart.add(Calendar.DATE, 1);
        logger.debug(
            " Adding Minutes to New Start Time + 1 day = {}", newStart.getTime().toInstant());
        newStart.add(Calendar.MINUTE, newTimeOffset);

        logger.debug(" New Start Time = {}", newStart);
      }

      return newStart.getTime().toInstant();
    }

    return null;
  }

  public static Instant convertDurationToInstant(Duration d) {

    Instant t = null;
    String unit = "";

    if (d != null) {

      if (d.getCode() != null) {
        unit = d.getCode();
      } else if (d.getUnit() != null) {
        unit = d.getUnit();
      }

      if (unit.equalsIgnoreCase("a")) {

        Calendar c = Calendar.getInstance();
        c.add(Calendar.YEAR, d.getValue().intValue());
        t = c.toInstant();

      } else if (unit.equalsIgnoreCase("mo")) {

        Calendar c = Calendar.getInstance();
        c.add(Calendar.MONTH, d.getValue().intValue());
        t = c.toInstant();

      } else if (unit.equalsIgnoreCase("wk")) {

        Calendar c = Calendar.getInstance();
        c.add(Calendar.DAY_OF_MONTH, (d.getValue().intValue() * 7));
        t = c.toInstant();

      } else if (unit.equalsIgnoreCase("d")) {

        Calendar c = Calendar.getInstance();
        c.add(Calendar.DAY_OF_MONTH, d.getValue().intValue());
        t = c.toInstant();

      } else if (unit.equalsIgnoreCase("h")) {

        t = new Date().toInstant().plusSeconds(d.getValue().longValue() * 60 * 60);
      } else if (unit.equalsIgnoreCase("min")) {

        t = new Date().toInstant().plusSeconds(d.getValue().longValue() * 60);
      } else if (unit.equalsIgnoreCase("s")) {

        t = new Date().toInstant().plusSeconds(d.getValue().longValue());
      } else if (d.getValue() != null) {

        t = new Date().toInstant().plusSeconds(d.getValue().longValue());
      } else {
        t = null;
      }
    }

    return t;
  }

  public static void saveDataToFile(String data, String fileName) {

    try (DataOutputStream outStream =
        new DataOutputStream(new BufferedOutputStream(new FileOutputStream(fileName)))) {

      logger.debug("Writing data to file: {}", StringEscapeUtils.escapeJava(fileName));
      outStream.writeBytes(data);
    } catch (IOException e) {
      logger.debug("Unable to write data to file: {}", StringEscapeUtils.escapeJava(fileName), e);
    }
  }

  public static boolean isAEmergentValueSet(ValueSet v) {

    boolean retVal = false;

    if (v != null && v.hasUseContext() && v.getUseContext() != null) {

      logger.debug("Checking Value Set Id {}", StringEscapeUtils.escapeJava(v.getId()));

      List<UsageContext> ucs = v.getUseContext();

      for (UsageContext uc : ucs) {

        if (uc.hasValue() && uc.getValue() != null && (uc.getValue() instanceof CodeableConcept)) {

          CodeableConcept cc = (CodeableConcept) uc.getValue();

          if (cc.getCodingFirstRep() != null
              && (cc.getCodingFirstRep().getCode() != null
                  && (cc.getCodingFirstRep()
                          .getCode()
                          .contentEquals(PlanDefinitionProcessor.COVID_SNOMED_USE_CONTEXT_CODE)
                      || cc.getCodingFirstRep()
                          .getCode()
                          .contentEquals(PlanDefinitionProcessor.EMERGENT_USE_CONTEXT_CODE)))) {
            logger.debug("Found EMERGENT VALUE SET = {}", v.getId());
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
      logger.debug("Value Set:{}", v);

      if (v.getUseContextFirstRep() != null) {

        logger.debug("Found Use Context ");

        UsageContext uc = v.getUseContextFirstRep();

        if (uc.getValue() instanceof Reference) {

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
      logger.error(EXCEPTION_READING_FILE, e);
    }
    return bundle;
  }

  public ca.uhn.fhir.model.dstu2.resource.Bundle readDstu2BundleFromFile(String filename) {

    logger.info("About to read File {}", filename);
    ca.uhn.fhir.model.dstu2.resource.Bundle bundle = null;

    FhirContext dstu2Context = FhirContext.forDstu2();

    try (InputStream in = new FileInputStream(new File(filename))) {
      logger.info("Reading File ");
      bundle =
          dstu2Context
              .newJsonParser()
              .parseResource(ca.uhn.fhir.model.dstu2.resource.Bundle.class, in);
      logger.info("Completed Reading File");
    } catch (Exception e) {
      logger.error("Exception Reading File ", e);
    }
    return bundle;
  }

  public ca.uhn.fhir.model.dstu2.resource.Bundle readDstu2BundleFromString(String data) {

    logger.info("About to read String ");
    ca.uhn.fhir.model.dstu2.resource.Bundle bundle = null;
    FhirContext dstu2Context = FhirContext.forDstu2();
    bundle =
        dstu2Context
            .newJsonParser()
            .parseResource(ca.uhn.fhir.model.dstu2.resource.Bundle.class, data);

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
