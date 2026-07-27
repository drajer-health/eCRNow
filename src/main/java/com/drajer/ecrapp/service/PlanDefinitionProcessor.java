package com.drajer.ecrapp.service;

import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.eca.model.AbstractAction;
import com.drajer.eca.model.ActionData;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.CQLExpressionCondition;
import com.drajer.eca.model.CloseOutEicrAction;
import com.drajer.eca.model.CreateEicrAction;
import com.drajer.eca.model.CreateEicrAfterRecheckAction;
import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.MatchTriggerAction;
import com.drajer.eca.model.PeriodicUpdateEicrAction;
import com.drajer.eca.model.RelatedAction;
import com.drajer.eca.model.ReportabilityResponseAction;
import com.drajer.eca.model.SubmitEicrAction;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.eca.model.ValidateEicrAction;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.ersd.service.ValueSetService;
import jakarta.annotation.PostConstruct;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DataRequirement.DataRequirementCodeFilterComponent;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.Enumerations.FHIRAllTypes;
import org.hl7.fhir.r4.model.Library;
import org.hl7.fhir.r4.model.PlanDefinition;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionComponent;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionConditionComponent;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionRelatedActionComponent;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.Timing;
import org.hl7.fhir.r4.model.Timing.TimingRepeatComponent;
import org.hl7.fhir.r4.model.TriggerDefinition;
import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.hl7.fhir.r4.model.ValueSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class PlanDefinitionProcessor {

  public static final String COVID_SNOMED_USE_CONTEXT_CODE = "840539006";
  public static final String COVID_SNOMED_USE_CONTEXT_SYSTEM = "http://snomed.info/sct";
  public static final String EMERGENT_USE_CONTEXT_CODE = "emergent";
  public static final String EMERGENT_USE_CONTEXT_SYSTEM =
      "http://hl7.org/fhir/us/ecr/CodeSystem/us-ph-usage-context";

  public static final String GROUPER_VALUE_SET_REFERENCE_1 = "plandefinition-ersd-instance";
  public static final String GROUPER_VALUE_SET_REFERENCE_2 = "plandefinition-ersd-skeleton";

  public static final String ERSD_BUNDLE_ID_STRING = "rctc";

  @Autowired
  @Qualifier("esrdGenericClient")
  private IGenericClient esrdClient;

  @Autowired
  @Qualifier("jsonParser")
  IParser jsonParser;

  @Autowired
  @Qualifier("valueSetServiceImpl")
  ValueSetService valueSetService;

  @Value("${ersd.file.location:default.json}")
  String ersdFileLocation;

  private final Logger logger = LoggerFactory.getLogger(PlanDefinitionProcessor.class);

  /** Helper class to hold collections of bundle resources. */
  private static class BundleResourceCollections {
    Set<ValueSet> emergentValuesets = new HashSet<>();
    Set<ValueSet> valuesets = new HashSet<>();
    Set<ValueSet> grouperValueSets = new HashSet<>();
  }

  @PostConstruct
  public void initializeClientMethods() {
    processResourceBundle();
  }

  public void processResourceBundle() {
    logger.info(" Reading ERSD Bundle File ");
    Bundle ersdBundle = readErsdBundleFromFile();

    if (ersdBundle == null) {
      return;
    }

    if (ersdBundle.getEntry() != null) {
      logger.info(" Bundle has been created with Entries : {}", ersdBundle.getEntry().size());
    }

    Bundle actualErsdBundle = extractInnerErsdBundle(ersdBundle);
    List<BundleEntryComponent> bundleEntries = getBundleEntries(actualErsdBundle, ersdBundle);

    BundleResourceCollections collections = processValueSetsAndLibraries(bundleEntries);
    Map<EventTypes.EcrActionTypes, Set<AbstractAction>> acts = new HashMap<>();
    processPlanDefinitionAndActions(bundleEntries, collections, acts);

    if (acts != null) {
      ActionRepo.getInstance().setActions(acts);
      ActionRepo.getInstance().setupTriggerBasedActions();
    }
  }

  private Bundle extractInnerErsdBundle(Bundle ersdBundle) {
    if (ersdBundle.getType() != Bundle.BundleType.SEARCHSET) {
      return null;
    }

    logger.info("Found a Bundle from a search result, containing the actual ERSD Bundle");
    List<BundleEntryComponent> innerBundle = ersdBundle.getEntry();

    for (BundleEntryComponent bundleEntry : innerBundle) {
      if (Optional.ofNullable(bundleEntry).isPresent()
          && bundleEntry.getResource().getResourceType().equals(ResourceType.Bundle)) {
        logger.debug(" Found a bundle within a bundle ");
        Bundle ib = (Bundle) bundleEntry.getResource();

        if (ib.getType() == Bundle.BundleType.COLLECTION
            && ib.getId().contains(ERSD_BUNDLE_ID_STRING)) {
          logger.info(" Found the bundle which is the actual ERSD Bundle file ");
          return ib;
        }
      }
    }
    return null;
  }

  private List<BundleEntryComponent> getBundleEntries(Bundle actualErsdBundle, Bundle ersdBundle) {
    if (actualErsdBundle != null) {
      logger.info(" Inner ERSD Bundle Found from where we need to extract the plan definition");
      return actualErsdBundle.getEntry();
    }
    logger.info(" Bundle read from configuration is a valid bundle to extract the plan definition");
    return ersdBundle.getEntry();
  }

  private BundleResourceCollections processValueSetsAndLibraries(
      List<BundleEntryComponent> bundleEntries) {
    BundleResourceCollections collections = new BundleResourceCollections();

    for (BundleEntryComponent bundleEntry : bundleEntries) {
      if (Optional.ofNullable(bundleEntry).isPresent()) {
        ResourceType resourceType = bundleEntry.getResource().getResourceType();

        if (resourceType.equals(ResourceType.ValueSet)) {
          processValueSet((ValueSet) bundleEntry.getResource(), collections);
        } else if (resourceType.equals(ResourceType.Library)) {
          processLibrary((Library) bundleEntry.getResource());
        }
      }
    }

    ValueSetSingleton.getInstance().setEmergentValueSets(collections.emergentValuesets);
    ValueSetSingleton.getInstance().setValueSets(collections.valuesets);
    ValueSetSingleton.getInstance().setGrouperValueSets(collections.grouperValueSets);

    return collections;
  }

  private void processValueSet(ValueSet valueSet, BundleResourceCollections collections) {
    logger.debug(" Found Value set");

    if (ApplicationUtils.isAEmergentValueSet(valueSet)) {
      logger.debug(" Found a Emergent Value Set {}", valueSet.getId());
      collections.emergentValuesets.add(valueSet);
      collections.valuesets.add(valueSet);
    } else if (ApplicationUtils.isAGrouperValueSet(valueSet)) {
      logger.debug(" Found a Grouper Value Set {}", valueSet.getId());
      collections.grouperValueSets.add(valueSet);
    } else {
      logger.debug(" Found a Regular Value Set {}", valueSet.getId());
      collections.valuesets.add(valueSet);
    }
  }

  private void processLibrary(Library lib) {
    logger.debug(" Found the Library ");
    if (lib.getId().contains("rctc")) {
      logger.debug(" Adding Rctc Version to the Action Repo {}", lib.getVersion());
      ActionRepo.getInstance().setRctcVersion(lib.getVersion());
    }
  }

  private void processPlanDefinitionAndActions(
      List<BundleEntryComponent> bundleEntries,
      BundleResourceCollections collections,
      Map<EventTypes.EcrActionTypes, Set<AbstractAction>> acts) {
    for (BundleEntryComponent bundleEntry : bundleEntries) {
      if (Optional.ofNullable(bundleEntry).isPresent()) {
        if (bundleEntry.getResource().getResourceType().equals(ResourceType.PlanDefinition)) {
          PlanDefinition planDefinition = (PlanDefinition) bundleEntry.getResource();
          processPlanDefinitionActions(planDefinition, acts);
        }
      }
    }
  }

  private void processPlanDefinitionActions(
      PlanDefinition planDefinition, Map<EventTypes.EcrActionTypes, Set<AbstractAction>> acts) {
    List<PlanDefinitionActionComponent> actions = planDefinition.getAction();

    logger.info(" Found Plan Definition ");
    if (actions == null || actions.isEmpty()) {
      return;
    }

    for (PlanDefinitionActionComponent action : actions) {
      processActionByType(action, acts);
    }
  }

  private void processActionByType(
      PlanDefinitionActionComponent action,
      Map<EventTypes.EcrActionTypes, Set<AbstractAction>> acts) {
    String actionId = action.getId();

    if ("match-trigger".equals(actionId)) {
      logger.info(" Identified Match Trigger EICR Action ");
      MatchTriggerAction mta = new MatchTriggerAction();
      populateActionData(mta, acts, action, EcrActionTypes.MATCH_TRIGGER);
      processMatchTriggerAction(action);
    } else if ("create-eicr".equals(actionId)) {
      logger.info(" Identified Create EICR Action ");
      CreateEicrAction mta = new CreateEicrAction();
      populateActionData(mta, acts, action, EcrActionTypes.CREATE_EICR);
    } else if ("periodic-update-eicr".equals(actionId)) {
      logger.info(" Identified Periodic Update EICR Action ");
      PeriodicUpdateEicrAction mta = new PeriodicUpdateEicrAction();
      populateActionData(mta, acts, action, EcrActionTypes.PERIODIC_UPDATE_EICR);
    } else if ("create-eicr-after-recheck".equals(actionId)) {
      logger.info(" Identified Create EICR After Recheck Action ");
      CreateEicrAfterRecheckAction cra = new CreateEicrAfterRecheckAction();
      populateActionData(cra, acts, action, EcrActionTypes.CREATE_EICR_AFTER_RECHECK);
    } else if ("close-out-eicr".equals(actionId)) {
      logger.info(" Identified Close Out EICR Action ");
      CloseOutEicrAction mta = new CloseOutEicrAction();
      populateActionData(mta, acts, action, EcrActionTypes.CLOSE_OUT_EICR);
    } else if ("validate-eicr".equals(actionId)) {
      logger.info(" Identified Validate EICR Action ");
      ValidateEicrAction mta = new ValidateEicrAction();
      populateActionData(mta, acts, action, EcrActionTypes.VALIDATE_EICR);
    } else if ("route-and-send-eicr".equals(actionId)) {
      logger.info(" Identified Submit EICR Action ");
      SubmitEicrAction mta = new SubmitEicrAction();
      populateActionData(mta, acts, action, EcrActionTypes.SUBMIT_EICR);
      populateRRCheckAction(acts, mta);
    }
  }

  private void processMatchTriggerAction(PlanDefinitionActionComponent action) {
    List<TriggerDefinition> triggerDefinitionsList = action.getTrigger();

    if (triggerDefinitionsList != null && !triggerDefinitionsList.isEmpty()) {
      logger.info(" Number of Trigger Definitions {}", triggerDefinitionsList.size());

      for (TriggerDefinition triggerDefinition : triggerDefinitionsList) {
        valueSetService.createPlanDefinitionAction(triggerDefinition);
      }
    }
  }

  private void populateRRCheckAction(
      Map<EcrActionTypes, Set<AbstractAction>> acts, AbstractAction relatedAction) {

    ReportabilityResponseAction act = new ReportabilityResponseAction();

    act.setActionId(java.util.UUID.randomUUID().toString());

    RelatedAction ra = new RelatedAction();
    ra.setRelationship(PlanDefinition.ActionRelationshipType.AFTER);
    ra.setRelatedAction(relatedAction);

    Duration d = new Duration();
    d.setValue(300);
    d.setUnit("s");
    ra.setDuration(new Duration());

    act.addRelatedAction(ra);

    if (acts.containsKey(EcrActionTypes.RR_CHECK)) {

      acts.get(EcrActionTypes.RR_CHECK).add(act);

      logger.info(" Map contained  RR CHECK so added to map resulting in size {}", acts.size());
    } else {
      Set<AbstractAction> aa = new HashSet<>();
      aa.add(act);
      acts.put(EcrActionTypes.RR_CHECK, aa);

      logger.info(
          " Map did not contain RR CHECK so added to map resulting in size {}", acts.size());
    }
  }

  private Bundle readErsdBundleFromFile() {

    logger.debug("About to read ERSD File {}", ersdFileLocation);
    Bundle bundle = null;
    try (InputStream in = new FileInputStream(new File(ersdFileLocation))) {
      logger.debug("Reading ERSD File ");

      bundle = jsonParser.parseResource(Bundle.class, in);
      logger.debug("Completed Reading ERSD File");
    } catch (Exception e) {
      logger.error("Exception Reading ERSD File", e);
    }
    return bundle;
  }

  private void populateActionData(
      AbstractAction act,
      Map<EventTypes.EcrActionTypes, Set<AbstractAction>> acts,
      PlanDefinitionActionComponent action,
      EventTypes.EcrActionTypes type) {

    act.setActionId(action.getId());

    if (action.hasTrigger()) processTriggerDefinitions(action.getTrigger(), act);

    if (action.hasCondition()) processConditions(action.getCondition(), act);

    if (action.hasRelatedAction()) processRelatedActions(action.getRelatedAction(), act, acts);

    if (action.hasTimingTiming()) {

      logger.info(" Found Timing Element for Action {} ", act.getActionId());
      TimingSchedule ts = getTimingSchedule(action.getTimingTiming(), TriggerType.DATACHANGED);
      if (ts != null) {
        act.addTimingData(ts);
        ts.print();
      }
    }

    if (acts != null) {
      if (acts.containsKey(type)) {
        acts.get(type).add(act);
        logger.info(" Map contained {}, so added to map resulting in size {}", type, acts.size());
      } else {
        Set<AbstractAction> aa = new HashSet<>();
        aa.add(act);
        acts.put(type, aa);
        logger.info(
            " Map did not contain {}, so added to map resulting in size {}", type, acts.size());
      }
    }
  }

  private void processTriggerDefinitions(List<TriggerDefinition> tdlist, AbstractAction act) {
    if (tdlist == null || tdlist.isEmpty()) {
      return;
    }

    for (TriggerDefinition triggerDefinition : tdlist) {
      if (isDataTrigger(triggerDefinition)) {
        processDataTrigger(triggerDefinition, act);
      } else if (triggerDefinition.getType() == TriggerType.PERIODIC) {
        processPeriodicTrigger(triggerDefinition, act);
      }
    }
  }

  private boolean isDataTrigger(TriggerDefinition triggerDefinition) {
    return triggerDefinition.getType() != TriggerType.NAMEDEVENT
        && triggerDefinition.getType() != TriggerType.PERIODIC
        && triggerDefinition.hasData();
  }

  private void processDataTrigger(TriggerDefinition triggerDefinition, AbstractAction act) {
    logger.info(" Identified Data Trigger for Act {}", act.getActionId());
    List<DataRequirement> dataRequirements = triggerDefinition.getData();

    for (DataRequirement dataReq : dataRequirements) {
      ActionData actionData = createActionDataFromDataRequirement(triggerDefinition, dataReq);
      act.addActionData(actionData);
    }
  }

  private ActionData createActionDataFromDataRequirement(
      TriggerDefinition triggerDefinition, DataRequirement dataReq) {
    ActionData ad = new ActionData();
    ad.setTriggerType(triggerDefinition.getType());
    ad.setFhirDataType(FHIRAllTypes.valueOf(dataReq.getType().toUpperCase()));

    if (dataReq.hasProfile()) {
      ad.setProfiles(dataReq.getProfile());
    }

    if (dataReq.hasCodeFilter()) {
      extractCodeFilterData(dataReq.getCodeFilterFirstRep(), dataReq.getType(), ad);
    }

    return ad;
  }

  private void extractCodeFilterData(
      DataRequirementCodeFilterComponent codeFilter, String dataType, ActionData ad) {
    if (codeFilter.hasPath()) {
      ad.setPath(dataType + "." + codeFilter.getPath());
      logger.info(" Evaluation Path = {}", ad.getPath());
    }

    if (codeFilter.hasValueSet()) {
      ad.setValueSet(codeFilter.getValueSetElement());
    }
  }

  private void processPeriodicTrigger(TriggerDefinition triggerDefinition, AbstractAction act) {
    if (!triggerDefinition.hasTimingTiming()) {
      return;
    }

    Timing timing = triggerDefinition.getTimingTiming();
    if (timing == null || !timing.hasRepeat()) {
      return;
    }

    TimingSchedule ts = getTimingSchedule(timing, triggerDefinition.getType());
    if (ts != null) {
      act.addTimingData(ts);
    }
  }

  private TimingSchedule getTimingSchedule(Timing t, TriggerType type) {

    if (t != null && t.hasRepeat()) {

      TimingRepeatComponent rc = t.getRepeat();

      // Create Timing Data
      TimingSchedule ts = new TimingSchedule();

      ts.setTriggerType(type);

      ts.setNumOfRepeat(rc.getCount());
      ts.setMaxRepeat(rc.getCountMax());
      ts.setFrequency(rc.getFrequency());
      ts.setFrequencyMax(rc.getFrequencyMax());

      ts.setFrequencyPeriod(rc.getPeriod());
      ts.setFrequencyPeriodUnit(rc.getPeriodUnitElement().getValue());
      ts.setDuration(rc.getDuration());
      ts.setDurationUnit(rc.getDurationUnit());

      logger.info(
          "Found Timing Element with Frequency Period {} {} AND Duration {} {}",
          rc.getPeriod(),
          rc.getPeriodUnitElement().getValueAsString(),
          rc.getDuration(),
          rc.getDurationUnit());

      return ts;
    }

    return null;
  }

  private void processConditions(
      List<PlanDefinitionActionConditionComponent> condlist, AbstractAction act) {

    if (condlist != null && !condlist.isEmpty()) {

      for (PlanDefinitionActionConditionComponent cond : condlist) {

        if (cond.hasKind() && cond.hasExpression()) {

          CQLExpressionCondition cd = new CQLExpressionCondition();
          cd.setConditionType(cond.getKind());
          cd.setExpression(cond.getExpression().getExpression());

          act.addCondition(cd);
        }
      }
    }
  }

  private void processRelatedActions(
      List<PlanDefinitionActionRelatedActionComponent> rdlist,
      AbstractAction act,
      Map<EventTypes.EcrActionTypes, Set<AbstractAction>> acts) {

    if (rdlist != null && !rdlist.isEmpty()) {

      for (PlanDefinitionActionRelatedActionComponent rc : rdlist) {

        RelatedAction ra = new RelatedAction();
        ra.setRelationship(rc.getRelationship());

        if (rc.hasOffsetDuration()) ra.setDuration(rc.getOffsetDuration());

        AbstractAction a = getActionById(rc.getActionId(), acts);

        if (a != null) {
          ra.setRelatedAction(a);
        }

        act.addRelatedAction(ra);
      }
    }
  }

  private AbstractAction getActionById(
      String actId, Map<EventTypes.EcrActionTypes, Set<AbstractAction>> acts) {

    if (acts != null && acts.size() > 0) {

      for (Map.Entry<EventTypes.EcrActionTypes, Set<AbstractAction>> ent : acts.entrySet()) {

        Set<AbstractAction> aa = ent.getValue();

        if (aa != null && !aa.isEmpty()) {

          for (AbstractAction a : aa) {

            if (a.getActionId().equalsIgnoreCase(actId)) return a;
          }
        }
      }
    }

    return null;
  }
}
