package com.drajer.bsa.kar.condition;

import com.drajer.bsa.cache.KarResolvedVariableCache;
import com.drajer.bsa.cache.KarVariableClassifier;
import com.drajer.bsa.cache.ResolvedVariables;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
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

    resolveVariables(cond, params, kd, act, ehrService);

    logger.info(" Parameters size after resolving variables = {}", params.getParameter().size());

    Parameters result =
        (Parameters)
            getEvaluator()
                .evaluate(
                    null, logicExpression, params, null, null, null, null, null, null, null, null);
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

    if (cond instanceof BsaFhirPathCondition) {

      logger.info(" Found a FhirPath Condition for action  {}", act.getActionId());

      // If a prior action for this same patient/KAR has already resolved the merged set of plan
      // variables (STATIC + CONTEXT), reuse it directly with zero evaluator/context calls.
      Parameters alreadyResolved = kd.getResolvedPlanVariables();

      if (alreadyResolved != null && !alreadyResolved.isEmpty()) {

        logger.info(" Reusing previously resolved plan variables for this patient/KAR ");
        alreadyResolved.getParameter().forEach(params::addParameter);
        return;
      }

      // Resolve conditions that are present at the PlanDefinition level.
      List<Expression> expressions = ((BsaFhirPathCondition) cond).getVariables();

      Parameters resolvedParams = new Parameters();

      if (expressions != null && !expressions.isEmpty()) {

        // Lazily fetched, at most once, and only if a STATIC variable is actually encountered.
        boolean staticVariablesFetched = false;
        Optional<ResolvedVariables> staticVariables = Optional.empty();

        for (Expression exp : expressions) {

          if (exp.hasLanguage() && exp.getLanguage().contentEquals("text/fhirpath")) {

            ParametersParameterComponent paramComponent = new ParametersParameterComponent();
            paramComponent.setName("%" + exp.getName());

            if (KarVariableClassifier.isContextDateVariable(exp.getName())) {

              String expr = resolveContextVariables(exp.getExpression(), ehrService, kd);
              DateTimeType value = new DateTimeType(expr);
              paramComponent.setValue(value);

              logger.info(" Adding Resolved Parameter {} with value {}", exp.getName(), value);

            } else if (KarVariableClassifier.isContextCodeVariable(exp.getName())) {

              String expr = resolveContextVariables(exp.getExpression(), ehrService, kd);
              CodeType val = new CodeType(expr);
              paramComponent.setValue(val);

            } else {

              if (!staticVariablesFetched) {
                staticVariables = getCachedStaticVariables(kd);
                staticVariablesFetched = true;
              }

              Optional<Type> cachedValue =
                  staticVariables.flatMap(rv -> rv.getVariable(exp.getName()));

              if (cachedValue.isPresent()) {

                paramComponent.setValue(cachedValue.get().copy());

                logger.debug(
                    " Resolved static plan variable {} from KAR resolved variable cache",
                    exp.getName());

              } else {

                if (karVariableCache != null) {
                  karVariableCache.recordFallback();
                }

                logger.warn(
                    " Cache miss for static plan variable {} of expression {}, falling back to inline evaluation",
                    exp.getName(),
                    exp.getExpression());

                resolveStaticVariableInline(exp, paramComponent, ehrService, kd);
              }
            }

            resolvedParams.addParameter(paramComponent.copy());
            params.addParameter(paramComponent);

          } else {
            logger.info(" Ignoring non FhirPath Expression ");
          }
        }

      } else {
        logger.info(" No Plan Definition Variables to resolve ");
      }

      // Cache the merged result so subsequent actions for this same patient/KAR do not need to
      // resolve any of these variables again.
      kd.setResolvedPlanVariables(resolvedParams);

    } else {

      logger.info(" Not a FhirPath Condition, so ignored ");
    }
  }

  /**
   * Resolves a STATIC variable inline via the evaluator. This is only used as a fallback when the
   * KAR resolved variable cache has not been initialized yet or does not contain the variable, and
   * matches the behavior used before the cache existed.
   */
  private void resolveStaticVariableInline(
      Expression exp,
      ParametersParameterComponent paramComponent,
      EhrQueryService ehrService,
      KarProcessingData kd) {

    logger.info(" Expression before resolution {}", exp.getExpression());

    String expr = resolveContextVariables(exp.getExpression(), ehrService, kd);

    logger.info(" Expression after resolution {}", expr);

    Parameters variableResult =
        (Parameters)
            getEvaluator()
                .evaluate(null, expr, null, null, null, null, null, null, null, null, null);

    if (variableResult.getParameter(PARAM) == null) {
      logger.error(
          " No parameter returned from FHIR Path Expression Evaluator for variable {} in expression {}, so value is set to null",
          exp.getName(),
          exp.getExpression());
      paramComponent.setValue((Type) null);
    } else {
      Type value = variableResult.getParameter(PARAM).getValue();
      paramComponent.setValue(value);

      logger.info(" Adding Resolved Parameter {} with value {}", exp.getName(), value);
    }
  }

  /**
   * Returns the STATIC variables previously resolved and cached for this patient's KAR, if the
   * cache is wired up and the KAR is known. Never throws - a missing cache or KAR simply results in
   * every STATIC variable falling back to inline evaluation.
   */
  private Optional<ResolvedVariables> getCachedStaticVariables(KarProcessingData kd) {

    if (karVariableCache == null) {
      return Optional.empty();
    }

    KnowledgeArtifact kar = kd.getKar();

    if (kar == null) {
      return Optional.empty();
    }

    return karVariableCache.get(kar.getVersionUniqueId());
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
