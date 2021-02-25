package com.drajer.eca.model;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.cdafromdstu2.Dstu2CdaEicrGenerator;
import com.drajer.cdafromr4.CdaEicrGeneratorFromR4;
import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.sof.utils.FhirContextInitializer;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.apache.commons.collections4.SetUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Encounter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EcaUtils {

  private EcaUtils() {
    throw new IllegalStateException("Utility class");
  }

  private static Logger logger = LoggerFactory.getLogger(EcaUtils.class);

  public static boolean matchTriggerCodesForDSTU2(
      List<ActionData> codePaths,
      Dstu2FhirData data,
      PatientExecutionState state,
      LaunchDetails details) {

    logger.info(" Start Matching Trigger Codes ");
    boolean matchfound = false;
    state.getMatchTriggerStatus().setTriggerMatchStatus(false);

    for (ActionData ad : codePaths) {

      logger.info(" Need to match Trigger Codes for : {}", ad.getPath());

      List<CodeableConceptDt> ptCodes = data.getCodesForExpression(ad.getPath());

      if (ptCodes != null && !ptCodes.isEmpty()) {

        logger.info(" Found total {} {}  for Patient", ptCodes.size(), ad.getPath());

        Set<String> codesToMatch = ApplicationUtils.convertCodeableConceptsToString(ptCodes);
        matchfound = matchTriggerCodes(details, ad, codesToMatch, state);
      }
    }

    logger.info(" End Matching Trigger Codes ");
    return matchfound;
  }

  public static boolean matchTriggerCodesForR4(
      List<ActionData> codePaths,
      R4FhirData data,
      PatientExecutionState state,
      LaunchDetails details) {

    logger.info(" Start Matching Trigger Codes ");
    boolean matchfound = false;
    state.getMatchTriggerStatus().setTriggerMatchStatus(false);

    for (ActionData ad : codePaths) {

      logger.info(" Need to match Trigger Codes for :{} ", ad.getPath());

      List<CodeableConcept> ptCodes = data.getR4CodesForExpression(ad.getPath());

      if (ptCodes != null && !ptCodes.isEmpty()) {

        logger.info(" Found total {} {} for Patient.", ptCodes.size(), ad.getPath());

        Set<String> codesToMatch = ApplicationUtils.convertR4CodeableConceptsToString(ptCodes);
        matchfound = matchTriggerCodes(details, ad, codesToMatch, state);
      }
    }

    logger.info(" End Matching Trigger Codes ");
    return matchfound;
  }

  public static boolean matchTriggerCodes(
      LaunchDetails details, ActionData ad, Set<String> codesToMatch, PatientExecutionState state) {
    boolean matchfound = false;
    Set<String> codesToMatchAgainst = null;

    if (details.getIsCovid()) {

      codesToMatchAgainst =
          ValueSetSingleton.getInstance().getCovidValueSetsAsStringForGrouper(ad.getPath());
      logger.info(
          " Total # of {} Codes in Trigger Code Value Set for matching for COVID-19",
          codesToMatchAgainst.size());
    } else {

      codesToMatchAgainst =
          ValueSetSingleton.getInstance().getValueSetsAsStringForGrouper(ad.getPath());
      logger.info(
          " Total # of {} Codes in Trigger Code Value Set for matching for Full EICR ",
          codesToMatchAgainst.size());
    }

    Set<String> intersection = SetUtils.intersection(codesToMatch, codesToMatchAgainst);

    if (intersection != null && !intersection.isEmpty()) {

      logger.info(" Number of Matched Codes = {}", intersection.size());

      state.getMatchTriggerStatus().setTriggerMatchStatus(true);
      matchfound = true;

      String valueSet = "2.16.840.1.113762.1.4.1146.1123";
      String valuesetVersion = "1";

      state
          .getMatchTriggerStatus()
          .addMatchedCodes(intersection, valueSet, ad.getPath(), valuesetVersion);

    } else {

      logger.info(" No Matched codes found for : {}", ad.getPath());
    }
    return matchfound;
  }

  public static Eicr createEicr(LaunchDetails details) {

    Eicr ecr = new Eicr();

    if (ActionRepo.getInstance().getLoadingQueryService() != null) {

      logger.info(" Getting necessary data from Loading Queries ");
      FhirData data =
          ActionRepo.getInstance()
              .getLoadingQueryService()
              .getData(details, details.getStartDate(), details.getEndDate());

      String eICR = null;

      if (data instanceof Dstu2FhirData) {

        logger.info("Creating eICR based on FHIR DSTU2 ");
        Dstu2FhirData dstu2Data = (Dstu2FhirData) data;
        eICR = Dstu2CdaEicrGenerator.convertDstu2FhirBundletoCdaEicr(dstu2Data, details, ecr);

      } else if (data instanceof R4FhirData) {

        logger.info("Creating eICR based on FHIR R4 ");
        R4FhirData r4Data = (R4FhirData) data;

        eICR = CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(r4Data, details, ecr);

      } else {

        String msg = "No Fhir Data retrieved to CREATE EICR.";
        logger.error(msg);

        throw new RuntimeException(msg);
      }

      if (eICR != null && !eICR.isEmpty()) {
        // Create the object for persistence.
        ecr.setEicrData(eICR);
        ActionRepo.getInstance().getEicrRRService().saveOrUpdate(ecr);
      } else {
        String msg = "No Fhir Data retrieved to CREATE EICR.";
        logger.error(msg);
        throw new RuntimeException(msg);
      }

    } else {

      String msg =
          "System Startup Issue, Spring Injection not functioning properly, loading service is null.";
      logger.error(msg);

      throw new RuntimeException(msg);
    }

    return ecr;
  }

  public static void updateDetailStatus(LaunchDetails details, PatientExecutionState state) {
    ObjectMapper mapper = new ObjectMapper();

    try {

      details.setStatus(mapper.writeValueAsString(state));

    } catch (JsonProcessingException e) {

      String msg = "Unable to update execution state";
      logger.error(msg, e);
      throw new RuntimeException(msg, e);
    }
  }

  public static PatientExecutionState recheckTriggerCodes(
      LaunchDetails details, WorkflowEvent launchType) {

    Set<AbstractAction> acts =
        ActionRepo.getInstance().getActions().get(EcrActionTypes.MATCH_TRIGGER);
    for (AbstractAction act : acts) {
      act.execute(details, launchType);
      ActionRepo.getInstance().getLaunchService().saveOrUpdate(details);
    }

    PatientExecutionState newState = null;

    newState = ApplicationUtils.getDetailStatus(details);

    return newState;
  }

  public static boolean hasNewTriggerCodeMatches(
      PatientExecutionState oldState, PatientExecutionState newState) {

    boolean retVal = false;

    if (!oldState.getMatchTriggerStatus().getTriggerMatchStatus()
        && newState.getMatchTriggerStatus().getTriggerMatchStatus()) {

      logger.info(
          " No Previously Matched trigger codes, since there is a match now returning true ");
      retVal = true;
    } else if (oldState.getMatchTriggerStatus().getTriggerMatchStatus()
        && newState.getMatchTriggerStatus().getTriggerMatchStatus()) {

      logger.info(" Comparing old Trigger Code Matches with New Trigger Code Matches ");
      List<MatchedTriggerCodes> mtc = oldState.getMatchTriggerStatus().getMatchedCodes();

      Set<String> oldCodes = new HashSet<String>();
      for (MatchedTriggerCodes m : mtc) {

        Optional.ofNullable(m.getMatchedCodes()).ifPresent(oldCodes::addAll);
        Optional.ofNullable(m.getMatchedValues()).ifPresent(oldCodes::addAll);
      }

      List<MatchedTriggerCodes> mtcNew = newState.getMatchTriggerStatus().getMatchedCodes();

      Set<String> newCodes = new HashSet<String>();
      for (MatchedTriggerCodes mn : mtcNew) {

        Optional.ofNullable(mn.getMatchedCodes()).ifPresent(newCodes::addAll);
        Optional.ofNullable(mn.getMatchedValues()).ifPresent(newCodes::addAll);
      }

      if (!oldCodes.containsAll(newCodes)) {

        logger.info(" Found new trigger codes and hence have to account for creating EICR ");
        retVal = true;
      } else {
        logger.info(
            " Trigger Codes are exactly the same as previous, so no need to perform further processing ");
      }

    } else {

      logger.info(" No matches found in the new check , so return false ");
    }

    return retVal;
  }

  public static boolean checkEncounterClose(LaunchDetails details) {

    boolean retVal = false;

    if (details != null
        && details.getEncounterId() != null
        && !details.getEncounterId().isEmpty()) {

      // Valid Encounter Id
      FhirContextInitializer ci = new FhirContextInitializer();
      FhirContext ctx = ci.getFhirContext(details.getFhirVersion());

      IGenericClient client =
          ci.createClient(ctx, details.getEhrServerURL(), details.getAccessToken());

      Encounter enc =
          (Encounter)
              ci.getResouceById(details, client, ctx, "Encounter", details.getEncounterId());

      if (enc != null) {
        logger.info(" Found Encounter for checking encounter closure ");

        if (enc.getPeriod() != null && enc.getPeriod().getEnd() != null) {
          logger.info(
              " Encounter has an end date so it is considered closed {}", enc.getPeriod().getEnd());
          retVal = true;
        }
      }
    }

    return retVal;
  }
}
