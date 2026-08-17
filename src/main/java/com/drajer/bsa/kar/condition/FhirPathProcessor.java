package com.drajer.bsa.kar.condition;

import com.drajer.bsa.cache.KarResolvedVariableCache;
import com.drajer.bsa.cache.KarVariableClassifier;
import com.drajer.bsa.cache.ResolvedVariables;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.eca.model.MatchedTriggerCodes;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.DataRequirement.DataRequirementCodeFilterComponent;
import org.hl7.fhir.r4.model.Parameters.ParametersParameterComponent;
import org.javatuples.Pair;
import org.opencds.cqf.fhir.cr.cpg.r4.R4CqlExecutionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FhirPathProcessor implements BsaConditionProcessor {

  private final Logger logger = LoggerFactory.getLogger(FhirPathProcessor.class);
  public static final String PARAM = "return";
  public static final String CPG_PARAM_DEFINITION =
      "http://hl7.org/fhir/uv/cpg/StructureDefinition/cpg-parameterDefinition";
  private static final String FHIR_PATH_LANGUAGE = "text/fhirpath";

  private Supplier<R4CqlExecutionService> evaluatorFactory;
  private final ThreadLocal<R4CqlExecutionService> evaluatorThreadLocal = new ThreadLocal<>();

  /**
   * The application-local cache of resolved STATIC KAR plan variables. This is set explicitly by
   * KarParserImpl when the condition is created since FhirPathProcessor instances are plain objects
   * (not Spring beans) and cannot be autowired directly. May be null (e.g. in unit tests), in which
   * case static variables are always resolved inline as a fallback.
   */
  private KarResolvedVariableCache karVariableCache;

  public void setKarVariableCache(KarResolvedVariableCache karVariableCache) {
    this.karVariableCache = karVariableCache;
  }

  @Override
  public Boolean evaluateExpression(
      BsaCondition cond, BsaAction act, KarProcessingData kd, EhrQueryService ehrService) {

    Parameters params = kd.getParametersByActionId(act.getActionId());
    if (params == null) {

      params = resolveInputParameters(act.getInputData(), kd, act);
    }

    logger.info(" Parameters size before resolving variables = {}", params.getParameter().size());

    String logicExpression = cond.getLogicExpression().getExpression();
    logger.debug("Logic Expression to be evaluated: {}", logicExpression);
    logger.debug(
        "Evaluating condition for action: {} with expression: {}",
        act.getActionId(),
        logicExpression);

    long varStart = System.nanoTime();
    resolveVariables(cond, params, kd, act, ehrService);
    logger.info(
        " Variable resolution for action {} took {} ms", act.getActionId(), elapsedMs(varStart));

    logger.info(" Parameters size after resolving variables = {}", params.getParameter().size());

    long evalStart = System.nanoTime();
    Parameters result =
        getEvaluator()
            .evaluate(
                null, logicExpression, params, null, null, null, null, null, null, null, null);
    logger.info(
        " FHIRPath condition evaluation for action {} took {} ms",
        act.getActionId(),
        elapsedMs(evalStart));
    ParametersParameterComponent ppc = result.getParameter(PARAM);

    if (ppc == null) {
      logger.error(
          " Null Value returned from FHIR Path Expression Evaluator : So condition not met");
      return false;
    } else {
      if (!(ppc.getValue() instanceof BooleanType)) {
        logger.error(
            " Not BooleanType Value returned from FHIR Path Expression Evaluator in "
                + cond.getLogicExpression().getExpression());
        throw new RuntimeException("Unexpected FHIR Path Expression return type");
      }
    }

    BooleanType value = (BooleanType) ppc.getValue();

    if (value != null && value.getValue() != null) {
      logger.info(" Result from CQL FHIR Path Evaluation {}", value);
      return value.getValue();
    } else {
      logger.error(
          " Null Value returned from FHIR Path Expression Evaluator : So condition not met");
      return false;
    }
  }

  public void resolveVariables(
      BsaCondition cond,
      Parameters params,
      KarProcessingData kd,
      BsaAction act,
      EhrQueryService ehrService) {

    if (!(cond instanceof BsaFhirPathCondition fhirPathCondition)) {
      logger.info(" Not a FhirPath Condition, so ignored ");
      return;
    }

    logger.info(" Found a FhirPath Condition for action  {}", act.getActionId());

    long start = System.nanoTime();

    // If a prior action for this same patient/KAR has already resolved the merged set of plan
    // variables (STATIC + CONTEXT), reuse it directly with zero evaluator/context calls.
    Parameters alreadyResolved = kd.getResolvedPlanVariables();

    if (alreadyResolved != null && !alreadyResolved.isEmpty()) {

      logger.info(" Reusing previously resolved plan variables for this patient/KAR ");
      alreadyResolved.getParameter().forEach(params::addParameter);
      logger.info(
          " resolveVariables reused {} previously resolved variable(s) for action {} in {} ms",
          alreadyResolved.getParameter().size(),
          act.getActionId(),
          elapsedMs(start));
      return;
    }

    List<Expression> expressions = fhirPathCondition.getVariables();
    Parameters resolvedParams = new Parameters();
    String karId = kd.getKar() != null ? kd.getKar().getVersionUniqueId() : null;

    if (expressions == null || expressions.isEmpty()) {

      logger.info(" No Plan Definition Variables to resolve ");

    } else {

      for (Expression exp : expressions) {

        if (!exp.hasLanguage() || !FHIR_PATH_LANGUAGE.contentEquals(exp.getLanguage())) {
          logger.info(" Ignoring non FhirPath Expression ");
          continue;
        }

        long varStart = System.nanoTime();

        ParametersParameterComponent paramComponent = resolveVariable(exp, karId, kd, ehrService);

        logger.info(
            " Resolved plan variable {} for action {} in {} ms",
            exp.getName(),
            act.getActionId(),
            elapsedMs(varStart));

        resolvedParams.addParameter(paramComponent.copy());
        params.addParameter(paramComponent);
      }
    }

    // Cache the merged result so subsequent actions for this same patient/KAR do not need to
    // resolve any of these variables again.
    kd.setResolvedPlanVariables(resolvedParams);

    logger.info(
        " resolveVariables freshly resolved {} variable(s) for action {} in {} ms",
        resolvedParams.getParameter().size(),
        act.getActionId(),
        elapsedMs(start));
  }

  /**
   * Resolves a single plan variable using one of three paths: (1) CONTEXT variables are
   * patient/encounter specific, so they are substituted directly and never cached; (2) STATIC
   * variables already present in the KAR resolved variable cache are reused as-is with zero
   * evaluator calls; (3) STATIC variables missing from the cache are resolved inline via the
   * evaluator, and the result is written back into the cache so no later action or patient ever
   * pays this cost again for the same KAR.
   */
  private ParametersParameterComponent resolveVariable(
      Expression exp, String karId, KarProcessingData kd, EhrQueryService ehrService) {

    ParametersParameterComponent paramComponent = new ParametersParameterComponent();
    paramComponent.setName("%" + exp.getName());

    if (KarVariableClassifier.isContextVariable(exp)) {
      paramComponent.setValue(resolveContextVariable(exp, kd, ehrService));
      return paramComponent;
    }

    Optional<Type> cachedValue = getCachedStaticVariable(karId, exp.getName());

    if (cachedValue.isPresent()) {

      logger.debug(
          " Resolved static plan variable {} from KAR resolved variable cache", exp.getName());
      paramComponent.setValue(cachedValue.get().copy());
      return paramComponent;
    }

    if (karVariableCache != null) {
      karVariableCache.recordFallback();
    }

    logger.warn(
        " Cache miss for static plan variable {} of expression {}, resolving inline and caching"
            + " the result for future reuse",
        exp.getName(),
        exp.getExpression());

    Type value = resolveInline(exp, ehrService, kd);
    paramComponent.setValue(value);

    cacheResolvedStaticVariable(karId, exp.getName(), value);

    return paramComponent;
  }

  /**
   * Resolves a CONTEXT (patient/encounter specific) variable. The two well-known simple
   * substitutions are built directly with no evaluator call; any other {{context.*}} expression
   * still needs the evaluator, but - unlike STATIC variables - its result must never be written to
   * the shared KAR cache since the value differs per patient.
   */
  private Type resolveContextVariable(
      Expression exp, KarProcessingData kd, EhrQueryService ehrService) {

    if (KarVariableClassifier.isContextDateVariable(exp.getName())) {

      DateTimeType value =
          new DateTimeType(resolveContextVariables(exp.getExpression(), ehrService, kd));
      logger.info(" Adding Resolved Parameter {} with value {}", exp.getName(), value);
      return value;
    }

    if (KarVariableClassifier.isContextCodeVariable(exp.getName())) {
      return new CodeType(resolveContextVariables(exp.getExpression(), ehrService, kd));
    }

    return resolveInline(exp, ehrService, kd);
  }

  /** Resolves a variable's expression via the evaluator, after substituting any context params. */
  private Type resolveInline(Expression exp, EhrQueryService ehrService, KarProcessingData kd) {

    logger.info(" Expression before resolution {}", exp.getExpression());

    String expr = resolveContextVariables(exp.getExpression(), ehrService, kd);

    logger.info(" Expression after resolution {}", expr);

    long evalStart = System.nanoTime();
    Parameters variableResult =
        getEvaluator().evaluate(null, expr, null, null, null, null, null, null, null, null, null);
    logger.info(
        " Inline evaluator call for plan variable {} took {} ms",
        exp.getName(),
        elapsedMs(evalStart));

    if (variableResult.getParameter(PARAM) == null) {
      logger.error(
          " No parameter returned from FHIR Path Expression Evaluator for variable {} in expression"
              + " {}, so value is set to null",
          exp.getName(),
          exp.getExpression());
      return null;
    }

    Type value = variableResult.getParameter(PARAM).getValue();
    logger.info(" Adding Resolved Parameter {} with value {}", exp.getName(), value);
    return value;
  }

  /**
   * Returns the STATIC variable previously resolved and cached for this KAR, if the cache is wired
   * up and the KAR/variable are known. Never throws - a missing cache, KAR, or variable simply
   * results in the variable falling back to inline evaluation.
   */
  private Optional<Type> getCachedStaticVariable(String karId, String variableName) {

    if (karVariableCache == null || karId == null) {
      return Optional.empty();
    }

    return karVariableCache.get(karId).flatMap(rv -> rv.getVariable(variableName));
  }

  /**
   * Backfills the KAR resolved variable cache with a STATIC variable resolved inline due to a cache
   * miss, so every subsequent patient/action for this KAR reuses it instead of paying the evaluator
   * cost again. This is a read-modify-write over the existing cache entry (if any); a lost update
   * under concurrent misses for the same KAR simply means that one fallback is retried later, which
   * is safe.
   */
  private void cacheResolvedStaticVariable(String karId, String variableName, Type value) {

    if (karVariableCache == null || karId == null || value == null) {
      return;
    }

    Map<String, Type> merged =
        karVariableCache
            .get(karId)
            .map(rv -> new HashMap<>(rv.getVariables()))
            .orElseGet(HashMap::new);

    merged.put(variableName, value);
    karVariableCache.put(karId, new ResolvedVariables(merged));
  }

  private static long elapsedMs(long startNanos) {
    return (System.nanoTime() - startNanos) / 1_000_000;
  }

  public String resolveContextVariables(
      String exp, EhrQueryService ehrService, KarProcessingData kd) {

    return ehrService.substituteContextParams(kd, exp, true);
  }

  public Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> filterResources(
      DataRequirement dr, KarProcessingData kd) {

    CheckTriggerCodeStatus ctc = new CheckTriggerCodeStatus();
    Map<String, Set<Resource>> resources = new HashMap<>();
    Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> retVal = new Pair<>(ctc, resources);

    logger.info(" Getting Resources by Type {}", dr.getType());

    HashSet<Resource> candidates = new HashSet<>();
    Set<Resource> inputCandidates = kd.getResourcesByType(dr.getType());
    if (inputCandidates != null) {
      candidates.addAll(inputCandidates);
    }

    for (Map.Entry<String, HashMap<String, Resource>> entry : kd.getActionOutputData().entrySet()) {
      for (Map.Entry<String, Resource> innerEntry : entry.getValue().entrySet()) {
        if (innerEntry.getValue().fhirType().equals(dr.getType())) {
          candidates.add(innerEntry.getValue());
        }
      }
    }

    filterByCode(dr, kd, ctc, candidates, resources);

    return retVal;
  }

  public void filterByCode(
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Set<Resource> candidates,
      Map<String, Set<Resource>> resources) {

    if (candidates != null) {

      for (Resource res : candidates) {

        if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Condition) {

          logger.debug(" Found Condition Resource {}", res.getId());
          Condition cond = (Condition) res;
          CodeableConcept cc = cond.getCode();

          filterByCode(dr, cc, kd, ctc, resources, res, false);

        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Observation) {

          logger.debug(" Found Observation Resource {}", res.getId());
          Observation obs = (Observation) res;
          CodeableConcept cc = obs.getCode();

          filterByCode(dr, cc, kd, ctc, resources, res, false);

          if (obs.getValue() instanceof CodeableConcept && obs.getValueCodeableConcept() != null) {
            CodeableConcept ccv = obs.getValueCodeableConcept();
            filterByCode(dr, ccv, kd, ctc, resources, res, false);
          }
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.ServiceRequest) {

          logger.debug(" Found ServiceRequest Resource {}", res.getId());
          ServiceRequest sr = (ServiceRequest) res;
          CodeableConcept cc = sr.getCode();

          filterByCode(dr, cc, kd, ctc, resources, res, false);
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.DiagnosticReport) {

          logger.debug(" Found DiagnosticReport Resource {}", res.getId());
          DiagnosticReport d = (DiagnosticReport) res;
          CodeableConcept cc = d.getCode();

          filterByCode(dr, cc, kd, ctc, resources, res, false);
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.MedicationRequest) {

          logger.debug(" Found MedicationRequest Resource {}", res.getId());
          MedicationRequest mr = (MedicationRequest) res;
          Type med = mr.getMedication();

          if (med instanceof CodeableConcept) {
            CodeableConcept cc = (CodeableConcept) med;
            filterByCode(dr, cc, kd, ctc, resources, res, false);
          } else if (med instanceof Reference) {
            Reference medRef = (Reference) med;
            String medId =
                medRef.hasReferenceElement() ? medRef.getReferenceElement().getIdPart() : null;
            if (medId != null && !medId.isEmpty()) {
              Resource medication = kd.getResourceById(medId, ResourceType.Medication);
              if (medication != null && !medication.isEmpty()) {
                Medication m = (Medication) medication;
                if (m.hasCode()) {
                  filterByCode(dr, m.getCode(), kd, ctc, resources, res, false);
                }
              }
            }
          } else {
            logger.info(" To be done, to navigate the Med Hiearachy to get the code ");
          }
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.MedicationStatement) {

          logger.debug(" Found MedicationStatement Resource {}", res.getId());
          MedicationStatement mr = (MedicationStatement) res;
          Type med = mr.getMedication();

          if (med instanceof CodeableConcept) {
            CodeableConcept cc = (CodeableConcept) med;
            filterByCode(dr, cc, kd, ctc, resources, res, false);
          } else if (med instanceof Reference) {
            Reference medRef = (Reference) med;
            String medId =
                medRef.hasReferenceElement() ? medRef.getReferenceElement().getIdPart() : null;
            if (medId != null && !medId.isEmpty()) {
              Resource medication = kd.getResourceById(medId, ResourceType.Medication);
              if (medication != null && !medication.isEmpty()) {
                Medication m = (Medication) medication;
                if (m.hasCode()) {
                  filterByCode(dr, m.getCode(), kd, ctc, resources, res, false);
                }
              }
            }
          } else {
            logger.info(" To be done, to navigate the Med Hiearachy to get the code ");
          }

        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.MedicationAdministration) {

          logger.debug(" Found MedicationAdministration Resource {}", res.getId());
          MedicationAdministration mr = (MedicationAdministration) res;
          Type med = mr.getMedication();
          if (med instanceof CodeableConcept) {
            CodeableConcept cc = (CodeableConcept) med;
            filterByCode(dr, cc, kd, ctc, resources, res, false);
          } else if (med instanceof Reference) {
            Reference medRef = (Reference) med;
            String medId =
                medRef.hasReferenceElement() ? medRef.getReferenceElement().getIdPart() : null;
            if (medId != null && !medId.isEmpty()) {
              Resource medication = kd.getResourceById(medId, ResourceType.Medication);
              if (medication != null && !medication.isEmpty()) {
                Medication m = (Medication) medication;
                if (m.hasCode()) {
                  filterByCode(dr, m.getCode(), kd, ctc, resources, res, false);
                }
              }
            }
          } else {
            logger.info(" To be done, to navigate the Med Hiearachy to get the code ");
          }
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Procedure) {

          logger.debug(" Found Procedure Resource {}", res.getId());
          Procedure pr = (Procedure) res;

          CodeableConcept cc = pr.getCode();
          filterByCode(dr, cc, kd, ctc, resources, res, false);

        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Immunization) {

          logger.debug(" Found Immunization Resource {}", res.getId());
          Immunization immz = (Immunization) res;

          CodeableConcept cc = immz.getVaccineCode();
          filterByCode(dr, cc, kd, ctc, resources, res, false);
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Encounter) {

          logger.debug(" Found Encounter Resource {}", res.getId());
          Encounter enc = (Encounter) res;

          CodeableConcept cc = enc.getReasonCodeFirstRep();
          filterByCode(dr, cc, kd, ctc, resources, res, false);
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.MeasureReport) {
          if (resources.get(res.fhirType()) != null) {
            resources.get(res.fhirType()).add(res);
          } else {
            Set<Resource> resources2 = new HashSet<>();
            resources2.add(res);
            resources.put(res.fhirType(), resources2);
          }
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.ValueSet) {
          if (resources.get(res.fhirType()) != null) {
            resources.get(res.fhirType()).add(res);
          } else {
            Set<Resource> resources2 = new HashSet<>();
            resources2.add(res);
            resources.put(res.fhirType(), resources2);
          }
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.CodeSystem) {
          if (resources.get(res.fhirType()) != null) {
            resources.get(res.fhirType()).add(res);
          } else {
            Set<Resource> resources2 = new HashSet<>();
            resources2.add(res);
            resources.put(res.fhirType(), resources2);
          }
        }
      }
    }
  }

  public void filterByCode(
      DataRequirement dr,
      CodeableConcept cc,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> res,
      Resource resourceMatched,
      Boolean valElem) {

    logger.debug("valElem:{}", valElem);

    List<DataRequirementCodeFilterComponent> drcfs = dr.getCodeFilter();
    Boolean notFound = false;
    if (drcfs != null) {

      for (DataRequirementCodeFilterComponent drcf : drcfs) {

        if ((drcf.getPath().toLowerCase().contains("code")
                || drcf.getPath().contains("reasonCode")
                || drcf.getPath().contains("value")
                || drcf.getPath().equals("medication")
                || drcf.getPath().equals("vaccineCode"))
            && drcf.getValueSet() != null) {

          Resource vsr = getValueSet(kd, drcf.getValueSet());

          if (vsr != null) {
            logger.debug(" Found Value Set {} to compare codes.", vsr.getId());

            ValueSet vs = (ValueSet) vsr;
            String matchPath = dr.getType() + "." + drcf.getPath();

            Pair<Boolean, MatchedTriggerCodes> retInfo =
                BsaServiceUtils.isCodeableConceptPresentInValueSet(vs, cc, matchPath, false);

            if (retInfo != null) {

              logger.info(
                  " Found a match for the code, adding resource {}", resourceMatched.getId());
              ctc.setTriggerMatchStatus(retInfo.getValue0());
              ctc.addMatchedTriggerCodes(retInfo.getValue1());
              if (res.get(dr.getId()) != null) {
                res.get(dr.getId()).add(resourceMatched);
              } else {
                Set<Resource> resources = new HashSet<>();
                resources.add(resourceMatched);

                // what if it already exists, it gets over written
                res.put(dr.getId(), resources);
              }
            } else {
              logger.debug(" No match found for path {}", matchPath);
              // Set the trigger match status to be false
              notFound = true;
              // Also clear the resources that were added if possible..
            }
          } else {
            logger.error(" Value Set not found for id {}", drcf.getValueSet());
          }
        } else {

          logger.error(" Value Set and Code not present for code filter component");
        }
      } // for all data requirements
    } else {
      logger.error(" Code Filter Component list is null, cannot proceed with finding matches ");
    }
  }

  private Parameters resolveInputParameters(
      List<DataRequirement> dataRequirements, KarProcessingData kd, BsaAction act) {
    if (dataRequirements == null || dataRequirements.isEmpty()) {
      return null;
    }

    Parameters params = new Parameters();

    for (DataRequirement req : dataRequirements) {

      if (req.hasCodeFilter()) {

        String name = req.getId();
        String fhirType = req.getType();
        String limit = req.hasLimit() ? Integer.toString(req.getLimit()) : "*";

        Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> resources =
            filterResources(req, kd);

        if (resources == null || resources.getValue1() == null || resources.getValue1().isEmpty()) {
          ParametersParameterComponent parameter =
              new ParametersParameterComponent().setName("%" + String.format("%s", name));
          parameter.addExtension(
              CPG_PARAM_DEFINITION,
              new ParameterDefinition().setMax(limit).setName("%" + name).setType(fhirType));
          params.addParameter(parameter);
        } else {
          for (Entry<String, Set<Resource>> entry : resources.getValue1().entrySet()) {
            if (entry.getKey().equals(fhirType)) {
              for (Resource resource : entry.getValue()) {
                ParametersParameterComponent parameter =
                    new ParametersParameterComponent().setName("%" + String.format("%s", name));
                parameter.addExtension(
                    CPG_PARAM_DEFINITION,
                    new ParameterDefinition().setMax(limit).setName("%" + name).setType(fhirType));
                parameter.setResource(resource);
                params.addParameter(parameter);
              }
            }
          }
        }
      } else {

        logger.info(" Data Requirement does not have Code Filter ");
        String name = req.getId();
        String fhirType = req.getType();
        String limit = req.hasLimit() ? Integer.toString(req.getLimit()) : "*";

        Set<Resource> resources = kd.getDataForId(req.getId(), act.getRelatedDataId(req.getId()));

        if (resources != null) {
          for (Resource res : resources) {

            ParametersParameterComponent parameter =
                new ParametersParameterComponent().setName("%" + String.format("%s", name));
            parameter.addExtension(
                CPG_PARAM_DEFINITION,
                new ParameterDefinition().setMax(limit).setName("%" + name).setType(fhirType));
            parameter.setResource(res);
            params.addParameter(parameter);
          }
        } else {
          ParametersParameterComponent parameter =
              new ParametersParameterComponent().setName("%" + String.format("%s", name));
          parameter.addExtension(
              CPG_PARAM_DEFINITION,
              new ParameterDefinition().setMax(limit).setName("%" + name).setType(fhirType));
          params.addParameter(parameter);
        }
      }
    }
    return params;
  }

  public Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> applyCodeFilter(
      DataRequirement dr, KarProcessingData kd, BsaAction action) {

    CheckTriggerCodeStatus ctc = new CheckTriggerCodeStatus();
    Map<String, Set<Resource>> resources = new HashMap<>();
    Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> retVal = new Pair<>(ctc, resources);

    logger.info(" Getting Resources by Data Requirement Id: {}", dr.getId());

    Set<Resource> candidates = kd.getDataForId(dr.getId(), action.getRelatedDataId(dr.getId()));

    filterByCode(dr, kd, ctc, candidates, resources);

    return retVal;
  }

  @Override
  public Boolean evaluateExpression(
      BsaCondition cond, Parameters params, EhrQueryService ehrService) {

    Parameters result =
        (Parameters)
            newEvaluator()
                .evaluate(
                    null,
                    cond.getLogicExpression().getExpression(),
                    params,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null);
    ParametersParameterComponent ppc = result.getParameter(PARAM);

    if (ppc == null) {
      logger.error(
          " Null Value returned from FHIR Path Expression Evaluator : So condition not met");
      return false;
    } else {
      if (!(ppc.getValue() instanceof BooleanType)) {
        logger.error(
            " Not BooleanType Value returned from FHIR Path Expression Evaluator in "
                + cond.getLogicExpression().getExpression());
        throw new RuntimeException("Unexpected FHIR Path Expression return type");
      }
    }

    BooleanType value = (BooleanType) ppc.getValue();

    if (value != null) {
      return value.getValue();
    } else {

      logger.error(
          " Null Value returned from FHIR Path Expression Evaluator : So condition not met");
      return false;
    }
  }

  public void setExpressionEvaluatorFactory(Supplier<R4CqlExecutionService> evaluatorFactory) {
    this.evaluatorFactory = evaluatorFactory;
  }

  R4CqlExecutionService newEvaluator() {
    R4CqlExecutionService ev = evaluatorFactory.get();
    logger.info("Evaluator instance: " + System.identityHashCode(ev));
    return ev;
  }

  public Resource getValueSet(KarProcessingData kd, String url) {
    if (StringUtils.isBlank(url)) {
      return null;
    }
    Resource res = kd.getKar().getDependentResource(ResourceType.ValueSet, url);

    if (res == null) {
      res = kd.getKar().getDependentResource(ResourceType.ValueSet, normalizeCanonicalUrl(url));
    }
    return res;
  }

  public String normalizeCanonicalUrl(String url) {
    try {
      if (StringUtils.isBlank(url)) {
        return null;
      }

      int pipeIndex = url.indexOf('|');
      return (pipeIndex >= 0) ? url.substring(0, pipeIndex) : url;

    } catch (Exception e) {
      logger.warn("Failed to normalize canonical URL: {}", url, e);

      return url;
    }
  }

  /**
   * Returns the evaluator associated with the current thread. A new evaluator is created only when
   * the current thread does not already have one.
   */
  R4CqlExecutionService getEvaluator() {
    R4CqlExecutionService evaluator = evaluatorThreadLocal.get();

    if (evaluator == null) {
      if (evaluatorFactory == null) {
        throw new IllegalStateException("Expression evaluator factory has not been configured");
      }

      evaluator = evaluatorFactory.get();
      evaluatorThreadLocal.set(evaluator);

      logger.info(
          "Created evaluator instance: {} for thread: {}",
          System.identityHashCode(evaluator),
          Thread.currentThread().getName());
    } else {
      logger.debug(
          "Reusing evaluator instance: {} for thread: {}",
          System.identityHashCode(evaluator),
          Thread.currentThread().getName());
    }

    return evaluator;
  }

  /**
   * Clears the evaluator associated with the current thread. This should be called in a finally
   * block when one KAR/job has completed, especially when the application uses a thread pool.
   */
  public void clearEvaluator() {
    R4CqlExecutionService evaluator = evaluatorThreadLocal.get();

    if (evaluator != null) {
      logger.info(
          "Removing evaluator instance: {} from thread: {}",
          System.identityHashCode(evaluator),
          Thread.currentThread().getName());
    }

    evaluatorThreadLocal.remove();
  }
}
