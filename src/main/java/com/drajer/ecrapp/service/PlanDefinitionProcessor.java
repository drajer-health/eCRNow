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
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import javax.annotation.PostConstruct;
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

  @Value("${ersd.file.location}")
  String ersdFileLocation;

  @Value("${ersd.covid19}")
  Boolean covid;

  private final Logger logger = LoggerFactory.getLogger(PlanDefinitionProcessor.class);

  @PostConstruct
  public void initializeClientMethods() {
    processResourceBundle();
  }

  public void processResourceBundle() {

    // Reading Bundle with Id 506 from ersd server.
    // Bundle esrdBundle =
    // esrdClient.read().resource(Bundle.class).withId("506").execute();

    logger.info(" Reading ERSD Bundle File ");
    Bundle ersdBundle = readErsdBundleFromFile();
    Bundle actualErsdBundle = null;

    if (ersdBundle != null) {

      if (logger.isInfoEnabled()) {
        logger.info(
            " Bundle has been created with Entries : "
                + ((ersdBundle.getEntry() != null) ? ersdBundle.getEntry().size() : " zero "));
      }

      // Check to see if this is a searchset bundle.
      if (ersdBundle.getType() == Bundle.BundleType.SEARCHSET) {

        // Check if there is a bundle of type collection and use it.
        // Typically it will be the first one.

        logger.info("Found a Bundle from a search result, containing the actual ERSD Bundle");

        List<BundleEntryComponent> innerBundle = ersdBundle.getEntry();

        for (BundleEntryComponent bundleEntry : innerBundle) {

          if (Optional.ofNullable(bundleEntry).isPresent()
              && bundleEntry.getResource().getResourceType().equals(ResourceType.Bundle)) {

            logger.info(" Found a bundle within a bundle ");

            Bundle ib = (Bundle) (bundleEntry.getResource());

            if (ib.getType() == Bundle.BundleType.COLLECTION
                && ib.getId().contains(ERSD_BUNDLE_ID_STRING)) {

              logger.info(" Found the bundle which is the actual ERSD Bundle file ");
              actualErsdBundle = ib;
              break;
            }
          }
        }
      }

      List<BundleEntryComponent> bundleEntries = null;

      if (actualErsdBundle != null) {
        logger.info(" Innder ERSD Bundle Found from where we need to extract the plan definition");
        bundleEntries = actualErsdBundle.getEntry();
      } else {
        logger.info(
            " Bundle read from configuration is a valid bundle to extarct the plan definition");
        bundleEntries = ersdBundle.getEntry();
      }

      ValueSet valueSet = null;
      PlanDefinition planDefinition = null;
      List<PlanDefinitionActionComponent> actions = null;
      List<TriggerDefinition> triggerDefinitionsList = null;
      Set<ValueSet> covidValuesets = new HashSet<>();
      Set<ValueSet> valuesets = new HashSet<>();
      Set<ValueSet> grouperValueSets = new HashSet<>();
      Map<EventTypes.EcrActionTypes, Set<AbstractAction>> acts = new HashMap<>();

      for (BundleEntryComponent bundleEntry : bundleEntries) {

        logger.info(" Processing Bundle Entries ");

        if (Optional.ofNullable(bundleEntry).isPresent()) {

          logger.info(
              " Bundle Entries present and is of type "
                  + bundleEntry.getResource().getResourceType());

          if (bundleEntry.getResource().getResourceType().equals(ResourceType.ValueSet)) {

            logger.info(" Found Value set");

            valueSet = (ValueSet) bundleEntry.getResource();

            if (ApplicationUtils.isACovidValueSet(valueSet)) {

              logger.info(" Found a COVID Value Set " + valueSet.getId());

              valueSetService.createValueSet(valueSet);
              covidValuesets.add(valueSet);
              valuesets.add(valueSet);
            } else if (ApplicationUtils.isAGrouperValueSet(valueSet)) {

              logger.info(" Found a Grouper Value Set " + valueSet.getId());
              valueSetService.createValueSetGrouper(valueSet);
              grouperValueSets.add(valueSet);

            } else {
              logger.info(" Found a Regular Value Set " + valueSet.getId());
              valueSetService.createValueSet(valueSet);
              valuesets.add(valueSet);
            }

          } else if (bundleEntry.getResource().getResourceType().equals(ResourceType.Library)) {
            logger.info(" Found the Library ");
            Library lib = (Library) bundleEntry.getResource();

            if (lib.getId().contains("rctc")) {

              logger.info(" Adding Rctc Version to the Action Repo" + lib.getVersion());
              ActionRepo.getInstance().setRctcVersion(lib.getVersion());
            }
          }
        }
      }

      ValueSetSingleton.getInstance().setCovidValueSets(covidValuesets);
      ValueSetSingleton.getInstance().setValueSets(valuesets);
      ValueSetSingleton.getInstance().setGrouperValueSets(grouperValueSets);

      for (BundleEntryComponent bundleEntry : bundleEntries) {

        if (Optional.ofNullable(bundleEntry).isPresent()) {

          if (bundleEntry.getResource().getResourceType().equals(ResourceType.PlanDefinition)) {
            planDefinition = (PlanDefinition) bundleEntry.getResource();
            actions = planDefinition.getAction();

            logger.info(" Found Plan Definition ");
            if (actions != null && !actions.isEmpty()) {

              for (PlanDefinitionActionComponent action : actions) {

                if (action.getId().equals("match-trigger")) {

                  logger.info(" Identified Match Trigger EICR Action ");

                  MatchTriggerAction mta = new MatchTriggerAction();

                  populateActionData(mta, acts, action, EcrActionTypes.MATCH_TRIGGER);

                  triggerDefinitionsList = action.getTrigger();

                  if (triggerDefinitionsList != null && !triggerDefinitionsList.isEmpty()) {

                    logger.info(" Number of Trigger Definitions {}", triggerDefinitionsList.size());

                    for (TriggerDefinition triggerDefinition : triggerDefinitionsList) {

                      valueSetService.createPlanDefinitionAction(triggerDefinition);
                    }
                  }

                } else if (action.getId().equals("create-eicr")) {

                  logger.info(" Identified Create EICR Action ");

                  CreateEicrAction mta = new CreateEicrAction();

                  populateActionData(mta, acts, action, EcrActionTypes.CREATE_EICR);

                } else if (action.getId().equals("periodic-update-eicr")) {

                  logger.info(" Identified Periodic Update EICR Action ");

                  PeriodicUpdateEicrAction mta = new PeriodicUpdateEicrAction();

                  populateActionData(mta, acts, action, EcrActionTypes.PERIODIC_UPDATE_EICR);

                } else if (action.getId().equals("create-eicr-after-recheck")) {

                  logger.info(" Identified Create EICR After Recheck Action ");

                  CreateEicrAfterRecheckAction cra = new CreateEicrAfterRecheckAction();

                  populateActionData(cra, acts, action, EcrActionTypes.CREATE_EICR_AFTER_RECHECK);

                } else if (action.getId().equals("close-out-eicr")) {

                  logger.info(" Identified Close Out EICR Action ");

                  CloseOutEicrAction mta = new CloseOutEicrAction();

                  populateActionData(mta, acts, action, EcrActionTypes.CLOSE_OUT_EICR);

                } else if (action.getId().equals("validate-eicr")) {

                  logger.info(" Identified Validate EICR Action ");

                  ValidateEicrAction mta = new ValidateEicrAction();

                  populateActionData(mta, acts, action, EcrActionTypes.VALIDATE_EICR);

                } else if (action.getId().equals("route-and-send-eicr")) {

                  logger.info(" Identified Submit EICR Action ");

                  SubmitEicrAction mta = new SubmitEicrAction();

                  populateActionData(mta, acts, action, EcrActionTypes.SUBMIT_EICR);

                  populateRRCheckAction(acts, mta);
                }
              }
            }
          }
        }
      }

      if (acts != null) {
        ActionRepo.getInstance().setActions(acts);

        ActionRepo.getInstance().setupTriggerBasedActions();
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

    logger.info("About to read ERSD File {}", ersdFileLocation);
    Bundle bundle = null;
    try (InputStream in = new FileInputStream(new File(ersdFileLocation))) {
      logger.info("Reading ERSD File ");

      bundle = jsonParser.parseResource(Bundle.class, in);
      logger.info("Completed Reading ERSD File");
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

        if (logger.isInfoEnabled()) {
          logger.info(
              " Map contained {}, so added to map resulting in size {}",
              type.toString(),
              acts.size());
        }
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

    if (tdlist != null && !tdlist.isEmpty()) {

      for (TriggerDefinition triggerDefinition : tdlist) {

        if (triggerDefinition.getType() != TriggerType.NAMEDEVENT
            && triggerDefinition.getType() != TriggerType.PERIODIC
            && triggerDefinition.hasData()) {

          logger.info(" Identified Data Trigger for Act {}", act.getActionId());

          List<DataRequirement> dr = triggerDefinition.getData();

          for (DataRequirement d : dr) {

            // Create ActionData object
            ActionData ad = new ActionData();

            ad.setTriggerType(triggerDefinition.getType());

            ad.setFhirDataType(FHIRAllTypes.valueOf(d.getType().toUpperCase()));

            if (d.hasProfile()) ad.setProfiles(d.getProfile());

            if (d.hasCodeFilter()) {

              DataRequirementCodeFilterComponent cf = d.getCodeFilterFirstRep();

              if (cf.hasPath()) {
                ad.setPath(d.getType() + "." + cf.getPath());
                logger.info(" Evaluation Path = " + ad.getPath());
              }

              if (cf.hasValueSet()) ad.setValueSet(cf.getValueSetElement());
            }

            act.addActionData(ad);
          }

        } else if (triggerDefinition.getType() == TriggerType.PERIODIC) {

          if (triggerDefinition.hasTimingTiming()) {

            Timing t = triggerDefinition.getTimingTiming();

            if (t.hasRepeat()) {

              TimingSchedule ts = getTimingSchedule(t, triggerDefinition.getType());

              if (ts != null) {
                act.addTimingData(ts);
              }
            }

          } else {

            // Not handling the others for eCR
          }

        } else {

          // Ignore other types for eCR
        }
      }
    }
  }

  private TimingSchedule getTimingSchedule(Timing t, TriggerType type) {

    if (t != null && t.hasRepeat()) {

      logger.info(" Found Timing Element ");

      TimingRepeatComponent rc = t.getRepeat();

      // Create Timing Data
      TimingSchedule ts = new TimingSchedule();

      ts.setTriggerType(type);

      ts.setNumOfRepeat(rc.getCount());
      ts.setMaxRepeat(rc.getCountMax());
      ts.setFrequency(rc.getFrequency());
      ts.setFrequencyMax(rc.getFrequencyMax());

      logger.info(" Period Freq = " + rc.getPeriod());
      ts.setFrequencyPeriod(rc.getPeriod());

      logger.info(" Period Freq Unit = " + rc.getPeriodUnitElement().getValueAsString());
      ts.setFrequencyPeriodUnit(rc.getPeriodUnitElement().getValue());

      logger.info(" Duration = " + rc.getDuration());
      ts.setDuration(rc.getDuration());

      logger.info(" Duration Unit = " + rc.getDurationUnit());
      ts.setDurationUnit(rc.getDurationUnit());

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
