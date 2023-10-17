package com.drajer.ersd.temp;

import ca.uhn.fhir.parser.IParser;
import java.io.InputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import javax.annotation.PostConstruct;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DataRequirement.DataRequirementCodeFilterComponent;
import org.hl7.fhir.r4.model.PlanDefinition;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionComponent;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.TriggerDefinition;
import org.hl7.fhir.r4.model.UsageContext;
import org.hl7.fhir.r4.model.ValueSet;
import org.hl7.fhir.r4.model.ValueSet.ConceptSetComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class ResourceBundleSingleton {

  private static final Logger LOGGER = LoggerFactory.getLogger(ResourceBundleSingleton.class);
  private static final String ERSD_BUNDLE_ID_STRING = "rctc";
  private static final String COVID_SNOMED_USE_CONTEXT_CODE = "840539006";
  private static final String EMERGENT_USE_CONTEXT_CODE = "emergent";
  private static final String GROUPER_VALUE_SET_REFERENCE_1 = "plandefinition-ersd-instance";
  private static final String GROUPER_VALUE_SET_REFERENCE_2 = "plandefinition-ersd-skeleton";

  @Autowired
  @Qualifier("jsonParser")
  IParser jsonParser;

  @Value("${ersd.temp.file.location}")
  String ersdTempFileLocation;

  @Value("${ersd.temp.file.load:false}")
  boolean ersdTempFileLoad;

  @PostConstruct
  public void initializeClientMethods() {
    if (ersdTempFileLoad) {
      processResourceBundle();
    }
  }

  private Bundle readErsdBundleFromFile() {

    LOGGER.info("About to read ERSD File {}", ersdTempFileLocation);
    Bundle bundle = null;

    try (InputStream in = this.getClass().getResourceAsStream(ersdTempFileLocation)) {
      bundle = jsonParser.parseResource(Bundle.class, in);
    } catch (Exception e) {
      LOGGER.error("Exception Reading ERSD File", e);
    }

    return bundle;
  }

  private static boolean isAEmergentValueSet(ValueSet valueSet) {

    boolean retVal = false;

    if (valueSet != null && valueSet.getUseContext() != null) {

      List<UsageContext> ucs = valueSet.getUseContext();

      for (UsageContext uc : ucs) {

        if (uc.getValue() instanceof CodeableConcept) {

          CodeableConcept cc = (CodeableConcept) uc.getValue();

          if (cc.getCodingFirstRep() != null
              && (cc.getCodingFirstRep().getCode() != null
                  && (cc.getCodingFirstRep().getCode().contentEquals(COVID_SNOMED_USE_CONTEXT_CODE)
                      || cc.getCodingFirstRep()
                          .getCode()
                          .contentEquals(EMERGENT_USE_CONTEXT_CODE)))) {
            LOGGER.debug("Found EMERGENT VALUE SET = {}", valueSet.getId());
            retVal = true;
          }
        }
      }
    }

    return retVal;
  }

  private static boolean isAGrouperValueSet(ValueSet valueSet) {

    boolean retVal = false;

    if (valueSet != null && valueSet.getUseContextFirstRep() != null) {

      UsageContext uc = valueSet.getUseContextFirstRep();

      if (uc.getValue() instanceof Reference) {

        Reference rr = (Reference) uc.getValue();

        if (rr != null
            && rr.getReference() != null
            && (rr.getReference().contains(GROUPER_VALUE_SET_REFERENCE_1)
                || rr.getReference().contains(GROUPER_VALUE_SET_REFERENCE_2))) {
          LOGGER.debug("Found Grouper VALUE SET = {}", valueSet.getId());
          retVal = true;
        }
      }
    }

    return retVal;
  }

  private List<CanonicalType> getValueSetListFromGrouper(String grouperId) {

    List<CanonicalType> valueSetIdList = null;

    for (ValueSet valueSet : ValueSetSingletonTemp.getInstance().getGrouperValueSets()) {

      LOGGER.debug("Looking for grouper value set for {}", grouperId);
      if (valueSet.getUrl() != null && valueSet.getUrl().equals(grouperId)) {

        LOGGER.debug("Found Grouper Value Set for {}", grouperId);
        if (valueSet.getCompose() != null && valueSet.getCompose().getInclude() != null) {

          LOGGER.debug("Value Set is composed of other value sets ");
          List<ConceptSetComponent> csc = valueSet.getCompose().getInclude();

          for (ConceptSetComponent conceptSetComponent : csc) {
            LOGGER.debug("Adding Value Set Ids to the list ");
            valueSetIdList = conceptSetComponent.getValueSet();
          }

          if (valueSetIdList != null && !valueSetIdList.isEmpty()) {
            LOGGER.debug("Value Set Id List Size = {}", valueSetIdList.size());
          } else {
            LOGGER.debug("Value Set Id List is NULL");
          }
        }

        break;
      } else {
        LOGGER.debug(
            "Value Set Id {}  does not match grouper Id : {}", valueSet.getId(), grouperId);
      }
    }

    return valueSetIdList;
  }

  private ValueSet getValueSetGrouperFromId(String grouperId) {

    ValueSet valueSetGrouper = null;

    for (ValueSet valueset : ValueSetSingletonTemp.getInstance().getGrouperValueSets()) {
      if (valueset.getUrl() != null && valueset.getUrl().equals(grouperId)) {
        LOGGER.debug("Grouper Id {}", grouperId);
        valueSetGrouper = valueset;
        break;
      }
    }

    return valueSetGrouper;
  }

  private Set<ValueSet> getValueSetByIds(List<CanonicalType> valueSetIdList) {

    Set<ValueSet> valueSets = new HashSet<>();

    if (Optional.ofNullable(valueSetIdList).isPresent()) {

      LOGGER.debug("Value Set id List is not null");

      for (CanonicalType canonicalType : valueSetIdList) {

        for (ValueSet valueSet : ValueSetSingletonTemp.getInstance().getValueSets()) {

          if (valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())
              || ((valueSet.getUrl() != null)
                  && (valueSet.getUrl().equalsIgnoreCase(canonicalType.getValueAsString())))) {
            valueSets.add(valueSet);
            break;
          }
        }

        for (ValueSet valueSet : ValueSetSingletonTemp.getInstance().getEmergentValueSets()) {

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

  private boolean isSetContainsValueSet(Set<ValueSet> valueSets, ValueSet valueSet) {

    if (valueSets != null && valueSet != null) {
      for (ValueSet vs : valueSets) {
        if (vs.getId().equalsIgnoreCase(valueSet.getId())) {
          return true;
        }
      }
    }
    return false;
  }

  private static Set<ValueSet> getEmergentValueSetByIds(List<CanonicalType> valueSetIdList) {

    Set<ValueSet> valueSets = new HashSet<>();

    if (Optional.ofNullable(valueSetIdList).isPresent()) {

      for (CanonicalType canonicalType : valueSetIdList) {

        for (ValueSet valueSet : ValueSetSingletonTemp.getInstance().getValueSets()) {

          if (valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())
              && isAEmergentValueSet(valueSet)) {

            LOGGER.debug("Found a Emergent Value Set for Grouper using Id {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          } else if ((valueSet.getUrl() != null)
              && (valueSet.getUrl().equalsIgnoreCase(canonicalType.getValueAsString()))
              && isAEmergentValueSet(valueSet)) {

            LOGGER.debug("Urls Matched for a Emergent Value Set {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          }
        }

        for (ValueSet valueSet : ValueSetSingletonTemp.getInstance().getEmergentValueSets()) {

          if (valueSet.getId().equalsIgnoreCase(canonicalType.getValueAsString())
              && isAEmergentValueSet(valueSet)) {

            LOGGER.debug("Found a Emergent Value Set for Grouper using Id {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          } else if ((valueSet.getUrl() != null)
              && (valueSet.getUrl().equalsIgnoreCase(canonicalType.getValueAsString()))
              && isAEmergentValueSet(valueSet)) {
            LOGGER.debug("Urls Matched for a Emergent Value Set {}", valueSet.getId());
            valueSets.add(valueSet);
            break;
          }
        }
      }
    }

    return valueSets;
  }

  private void createPlanDefinitionAction(TriggerDefinition triggerDefinition) {

    List<DataRequirement> datareqs = triggerDefinition.getData();

    Set<ValueSet> grouperToValueSets = new HashSet<>();
    Set<ValueSet> grouperToEmergentValueSets = new HashSet<>();

    for (DataRequirement d : datareqs) {

      DataRequirementCodeFilterComponent codeFilter = d.getCodeFilterFirstRep();

      LOGGER.debug(" Getting Value Set List for Grouper {}", codeFilter.getValueSet());

      List<CanonicalType> valueSetIdList = getValueSetListFromGrouper(codeFilter.getValueSet());

      LOGGER.debug(
          " Size of valueSetIdList = {}",
          ((valueSetIdList == null) ? "Null" : valueSetIdList.size()));

      grouperToValueSets = getValueSetByIds(valueSetIdList);

      LOGGER.debug(" Size of Value Sets for Grouper : {}", grouperToValueSets.size());

      grouperToEmergentValueSets = getEmergentValueSetByIds(valueSetIdList);

      LOGGER.debug(
          " Size of Emergent Value Sets for Grouper : {}", grouperToEmergentValueSets.size());
    }

    DataRequirement dataRequirement = triggerDefinition.getDataFirstRep();
    DataRequirementCodeFilterComponent codeFilter = dataRequirement.getCodeFilterFirstRep();

    List<CanonicalType> valueSetIdList = getValueSetListFromGrouper(codeFilter.getValueSet());
    Set<ValueSet> valueSets = getValueSetByIds(valueSetIdList);

    ValueSet valueSetGrouper = getValueSetGrouperFromId(codeFilter.getValueSet());

    String path = dataRequirement.getType() + "." + codeFilter.getPath();
    LOGGER.debug(
        " Trigger Path to Grouper Map {} , Grouper {}",
        path,
        valueSetGrouper == null ? "NULL" : valueSetGrouper.getId());

    ValueSetSingletonTemp.getInstance().getTriggerPathToValueSetsMap().put(path, valueSets);

    if (ValueSetSingletonTemp.getInstance().getTriggerPathToGrouperMap().containsKey(path)) {
      LOGGER.debug(" Found Path in Grouper Map for {}", path);
      if (Boolean.FALSE.equals(
          isSetContainsValueSet(
              ValueSetSingletonTemp.getInstance().getTriggerPathToGrouperMap().get(path),
              valueSetGrouper))) {
        ValueSetSingletonTemp.getInstance()
            .getTriggerPathToGrouperMap()
            .get(path)
            .add(valueSetGrouper);
      }
    } else {
      LOGGER.debug(" Did not Find Path in Grouper Map for {}", path);
      Set<ValueSet> vs = new HashSet<>();
      vs.add(valueSetGrouper);
      ValueSetSingletonTemp.getInstance().getTriggerPathToGrouperMap().put(path, vs);
    }

    if (valueSetGrouper != null) {

      LOGGER.debug(" Adding Grouper Id {} to map", codeFilter.getValueSet());
      ValueSetSingletonTemp.getInstance()
          .addGrouperToValueSetMap(valueSetGrouper.getId(), grouperToValueSets);
      ValueSetSingletonTemp.getInstance()
          .addGrouperToEmergentValueSetMap(valueSetGrouper.getId(), grouperToEmergentValueSets);
    }
  }

  public void processResourceBundle() {
    LOGGER.info(" Reading ERSD Bundle File ");
    Bundle ersdBundle = readErsdBundleFromFile();
    Bundle actualErsdBundle = null;

    if (ersdBundle != null) {

      if (ersdBundle.getEntry() != null) {

        LOGGER.info(" Bundle has been created with Entries : {}", ersdBundle.getEntry().size());
      }

      // Check to see if this is a searchset bundle.
      if (ersdBundle.getType() == Bundle.BundleType.SEARCHSET) {

        // Check if there is a bundle of type collection and use it.
        // Typically it will be the first one.

        LOGGER.info("Found a Bundle from a search result, containing the actual ERSD Bundle");

        List<BundleEntryComponent> innerBundle = ersdBundle.getEntry();

        for (BundleEntryComponent bundleEntry : innerBundle) {

          if (Optional.ofNullable(bundleEntry).isPresent()
              && bundleEntry.getResource().getResourceType().equals(ResourceType.Bundle)) {

            Bundle ib = (Bundle) (bundleEntry.getResource());

            if (ib.getType() == Bundle.BundleType.COLLECTION
                && ib.getId().contains(ERSD_BUNDLE_ID_STRING)) {

              LOGGER.info(" Found the bundle which is the actual ERSD Bundle file ");
              actualErsdBundle = ib;
              break;
            }
          }
        }
      }

      List<BundleEntryComponent> bundleEntries = null;

      if (actualErsdBundle != null) {
        LOGGER.info(" Inner ERSD Bundle Found from where we need to extract the plan definition");
        bundleEntries = actualErsdBundle.getEntry();
      } else {
        LOGGER.info(
            " Bundle read from configuration is a valid bundle to extract the plan definition");
        bundleEntries = ersdBundle.getEntry();
      }

      PlanDefinition planDefinition = null;
      List<PlanDefinitionActionComponent> actions = null;
      List<TriggerDefinition> triggerDefinitionsList = null;
      ValueSet valueSet = null;
      Set<ValueSet> emergentValuesets = new HashSet<>();
      Set<ValueSet> valuesets = new HashSet<>();
      Set<ValueSet> grouperValueSets = new HashSet<>();

      for (BundleEntryComponent bundleEntry : bundleEntries) {

        if (Optional.ofNullable(bundleEntry).isPresent()) {

          LOGGER.debug(
              " Bundle Entries present and is of type {}",
              bundleEntry.getResource().getResourceType());

          if (bundleEntry.getResource().getResourceType().equals(ResourceType.ValueSet)) {

            valueSet = (ValueSet) bundleEntry.getResource();

            if (ResourceBundleSingleton.isAEmergentValueSet(valueSet)) {
              LOGGER.debug(" Found a Emergent Value Set {}", valueSet.getId());
              emergentValuesets.add(valueSet);
              valuesets.add(valueSet);
            } else if (ResourceBundleSingleton.isAGrouperValueSet(valueSet)) {
              LOGGER.debug(" Found a Grouper Value Set {}", valueSet.getId());
              grouperValueSets.add(valueSet);
            } else if (valueSet != null) {
              LOGGER.debug(" Found a Regular Value Set {}", valueSet.getId());
              valuesets.add(valueSet);
            }
          }
        }
      }

      ValueSetSingletonTemp.getInstance().setEmergentValueSets(emergentValuesets);
      ValueSetSingletonTemp.getInstance().setValueSets(valuesets);
      ValueSetSingletonTemp.getInstance().setGrouperValueSets(grouperValueSets);

      for (BundleEntryComponent bundleEntry : bundleEntries) {

        if (Optional.ofNullable(bundleEntry).isPresent()
            && bundleEntry.getResource().getResourceType().equals(ResourceType.PlanDefinition)) {

          planDefinition = (PlanDefinition) bundleEntry.getResource();
          actions = planDefinition.getAction();

          LOGGER.info(" Found Plan Definition ");
          if (actions != null && !actions.isEmpty()) {
            for (PlanDefinitionActionComponent action : actions) {
              if (action.getId().equals("match-trigger")) {

                LOGGER.info(" Identified Match Trigger EICR Action ");

                triggerDefinitionsList = action.getTrigger();

                if (triggerDefinitionsList != null && !triggerDefinitionsList.isEmpty()) {

                  LOGGER.info(" Number of Trigger Definitions {}", triggerDefinitionsList.size());

                  for (TriggerDefinition triggerDefinition : triggerDefinitionsList) {

                    createPlanDefinitionAction(triggerDefinition);
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
