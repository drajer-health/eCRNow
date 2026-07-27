package com.drajer.bsa.kar.condition;

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
  private static final String NULL_FHIR_PATH_EVALUATION =
      " Null Value returned from FHIR Path Expression Evaluator : So condition not met";
  private static final String MED_HIERARCHY_TODO =
      " To be done, to navigate the Med Hiearachy to get the code ";

  private Supplier<R4CqlExecutionService> evaluatorFactory;

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
            newEvaluator()
                .evaluate(
                    null, logicExpression, params, null, null, null, null, null, null, null, null);
    ParametersParameterComponent ppc = result.getParameter(PARAM);

    if (ppc == null) {
      logger.error(NULL_FHIR_PATH_EVALUATION);
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
      logger.error(NULL_FHIR_PATH_EVALUATION);
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

      // Resolve conditions that are present at the PlanDefinition level.
      List<Expression> expressions = ((BsaFhirPathCondition) cond).getVariables();

      if (expressions != null && !expressions.isEmpty()) {

        for (Expression exp : expressions) {

          if (exp.hasLanguage() && exp.getLanguage().contentEquals("text/fhirpath")) {

            ParametersParameterComponent paramComponent = new ParametersParameterComponent();

            logger.info(" Expression before resolution {}", exp.getExpression());

            String expr = resolveContextVariables(exp.getExpression(), ehrService, kd);

            logger.info(" Expression after resolution {}", expr);

            Parameters variableResult =
                (Parameters)
                    newEvaluator()
                        .evaluate(null, expr, null, null, null, null, null, null, null, null, null);

            if (exp.getName().contentEquals("encounterStartDate")
                || exp.getName().contentEquals("encounterEndDate")
                || exp.getName().contentEquals("lastReportSubmissionDate")) {

              DateTimeType value = new DateTimeType(expr);

              paramComponent.setName("%" + exp.getName());
              paramComponent.setValue(value);

              logger.info(" Adding Resolved Parameter {} with value {}", exp.getName(), value);

            } else if (exp.getName().contentEquals("encounterClass")) {

              CodeType val = new CodeType(expr);
              paramComponent.setName("%" + exp.getName());
              paramComponent.setValue(val);

            } else {

              // TODO: Fix how this should be treated in the case the getParameter(PARAM) is null

              if (variableResult.getParameter(PARAM) == null) {
                logger.error(
                    " No parameter returned from FHIR Path Expression Evaluator for variable {} in expression {}, so value is set to null",
                    exp.getName(),
                    exp.getExpression());
                paramComponent.setName("%" + exp.getName());
                paramComponent.setValue((Type) null);
                params.addParameter(paramComponent);
              } else {
                Type value = variableResult.getParameter(PARAM).getValue();
                paramComponent.setName("%" + exp.getName());
                paramComponent.setValue(value);

                logger.info(" Adding Resolved Parameter {} with value {}", exp.getName(), value);
              }
            }

            params.addParameter(paramComponent);
          } else {
            logger.info(" Ignoring non FhirPath Expression ");
          }
        }

      } else {
        logger.info(" No Plan Definition Variables to resolve ");
      }

      // Try to resolve any other context variables that are present at the expression level and are
      // not resolved.

    } else {

      logger.info(" Not a FhirPath Condition, so ignored ");
    }
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
        if (res.getResourceType().toString().contentEquals(dr.getType())) {
          handleResourceByType(res, dr, kd, ctc, resources);
        }
      }
    }
  }

  private void handleResourceByType(
      Resource res,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    switch (res.getResourceType()) {
      case Condition:
        handleCondition((Condition) res, dr, kd, ctc, resources);
        break;
      case Observation:
        handleObservation((Observation) res, dr, kd, ctc, resources);
        break;
      case ServiceRequest:
        handleServiceRequest((ServiceRequest) res, dr, kd, ctc, resources);
        break;
      case DiagnosticReport:
        handleDiagnosticReport((DiagnosticReport) res, dr, kd, ctc, resources);
        break;
      case MedicationRequest:
        handleMedicationRequest((MedicationRequest) res, dr, kd, ctc, resources);
        break;
      case MedicationStatement:
        handleMedicationStatement((MedicationStatement) res, dr, kd, ctc, resources);
        break;
      case MedicationAdministration:
        handleMedicationAdministration((MedicationAdministration) res, dr, kd, ctc, resources);
        break;
      case Procedure:
        handleProcedure((Procedure) res, dr, kd, ctc, resources);
        break;
      case Immunization:
        handleImmunization((Immunization) res, dr, kd, ctc, resources);
        break;
      case Encounter:
        handleEncounter((Encounter) res, dr, kd, ctc, resources);
        break;
      case MeasureReport:
      case ValueSet:
      case CodeSystem:
        handleCodeBasedResources(res, resources);
        break;
      default:
        logger.debug("Unsupported resource type: {}", res.getResourceType());
    }
  }

  private void handleCondition(
      Condition cond,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    logger.debug(" Found Condition Resource {}", cond.getId());
    CodeableConcept cc = cond.getCode();
    filterByCode(dr, cc, kd, ctc, resources, cond, false);
  }

  private void handleObservation(
      Observation obs,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    logger.debug(" Found Observation Resource {}", obs.getId());
    CodeableConcept cc = obs.getCode();
    filterByCode(dr, cc, kd, ctc, resources, obs, false);

    if (obs.getValue() instanceof CodeableConcept && obs.getValueCodeableConcept() != null) {
      CodeableConcept ccv = obs.getValueCodeableConcept();
      filterByCode(dr, ccv, kd, ctc, resources, obs, false);
    }
  }

  private void handleServiceRequest(
      ServiceRequest sr,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    logger.debug(" Found ServiceRequest Resource {}", sr.getId());
    CodeableConcept cc = sr.getCode();
    filterByCode(dr, cc, kd, ctc, resources, sr, false);
  }

  private void handleDiagnosticReport(
      DiagnosticReport d,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    logger.debug(" Found DiagnosticReport Resource {}", d.getId());
    CodeableConcept cc = d.getCode();
    filterByCode(dr, cc, kd, ctc, resources, d, false);
  }

  private void handleMedicationRequest(
      MedicationRequest mr,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    logger.debug(" Found MedicationRequest Resource {}", mr.getId());
    processMedicationReference(mr.getMedication(), dr, kd, ctc, resources, mr);
  }

  private void handleMedicationStatement(
      MedicationStatement mr,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    logger.debug(" Found MedicationStatement Resource {}", mr.getId());
    processMedicationReference(mr.getMedication(), dr, kd, ctc, resources, mr);
  }

  private void handleMedicationAdministration(
      MedicationAdministration mr,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    logger.debug(" Found MedicationAdministration Resource {}", mr.getId());
    processMedicationReference(mr.getMedication(), dr, kd, ctc, resources, mr);
  }

  private void processMedicationReference(
      Type med,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources,
      Resource medicationResource) {

    if (med instanceof CodeableConcept) {
      CodeableConcept cc = (CodeableConcept) med;
      filterByCode(dr, cc, kd, ctc, resources, medicationResource, false);
    } else if (med instanceof Reference) {
      processMedicationReferenceType((Reference) med, dr, kd, ctc, resources, medicationResource);
    } else {
      logger.info(" To be done, to navigate the Med Hierarchy to get the code ");
    }
  }

  private void processMedicationReferenceType(
      Reference medRef,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources,
      Resource medicationResource) {

    String medId = medRef.hasReferenceElement() ? medRef.getReferenceElement().getIdPart() : null;
    if (medId != null && !medId.isEmpty()) {
      Resource medication = kd.getResourceById(medId, ResourceType.Medication);
      if (medication != null && !medication.isEmpty()) {
        Medication m = (Medication) medication;
        if (m.hasCode()) {
          filterByCode(dr, m.getCode(), kd, ctc, resources, medicationResource, false);
        }
      }
    }
  }

  private void handleProcedure(
      Procedure pr,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    logger.debug(" Found Procedure Resource {}", pr.getId());
    CodeableConcept cc = pr.getCode();
    filterByCode(dr, cc, kd, ctc, resources, pr, false);
  }

  private void handleImmunization(
      Immunization immz,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    logger.debug(" Found Immunization Resource {}", immz.getId());
    CodeableConcept cc = immz.getVaccineCode();
    filterByCode(dr, cc, kd, ctc, resources, immz, false);
  }

  private void handleEncounter(
      Encounter enc,
      DataRequirement dr,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> resources) {

    logger.debug(" Found Encounter Resource {}", enc.getId());
    CodeableConcept cc = enc.getReasonCodeFirstRep();
    filterByCode(dr, cc, kd, ctc, resources, enc, false);
  }

  private void handleCodeBasedResources(Resource res, Map<String, Set<Resource>> resources) {

    addResourceToMap(res, resources);
  }

  private void addResourceToMap(Resource res, Map<String, Set<Resource>> resources) {
    String resourceType = res.fhirType();
    if (resources.containsKey(resourceType)) {
      resources.get(resourceType).add(res);
    } else {
      Set<Resource> resourceSet = new HashSet<>();
      resourceSet.add(res);
      resources.put(resourceType, resourceSet);
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

  /**
   * Create a parameters component with extension.
   *
   * @param name the parameter name
   * @param limit the limit value
   * @param fhirType the FHIR type
   * @return the parameters component
   */
  private ParametersParameterComponent createParameterWithExtension(
      String name, String limit, String fhirType) {
    ParametersParameterComponent parameter =
        new ParametersParameterComponent().setName("%" + String.format("%s", name));
    parameter.addExtension(
        CPG_PARAM_DEFINITION,
        new ParameterDefinition().setMax(limit).setName("%" + name).setType(fhirType));
    return parameter;
  }

  /**
   * Process filtered resources with code filter.
   *
   * @param params the parameters to add to
   * @param req the data requirement
   * @param kd the KAR processing data
   * @param name the parameter name
   * @param fhirType the FHIR type
   * @param limit the limit value
   */
  private void processCodeFilteredResources(
      Parameters params,
      DataRequirement req,
      KarProcessingData kd,
      String name,
      String fhirType,
      String limit) {
    Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> resources = filterResources(req, kd);

    if (resources == null || resources.getValue1() == null || resources.getValue1().isEmpty()) {
      ParametersParameterComponent parameter = createParameterWithExtension(name, limit, fhirType);
      params.addParameter(parameter);
    } else {
      for (Entry<String, Set<Resource>> entry : resources.getValue1().entrySet()) {
        if (entry.getKey().equals(fhirType)) {
          for (Resource resource : entry.getValue()) {
            ParametersParameterComponent parameter =
                createParameterWithExtension(name, limit, fhirType);
            parameter.setResource(resource);
            params.addParameter(parameter);
          }
        }
      }
    }
  }

  /**
   * Process non-filtered resources.
   *
   * @param params the parameters to add to
   * @param req the data requirement
   * @param kd the KAR processing data
   * @param act the BSA action
   * @param name the parameter name
   * @param fhirType the FHIR type
   * @param limit the limit value
   */
  private void processNonFilteredResources(
      Parameters params,
      DataRequirement req,
      KarProcessingData kd,
      BsaAction act,
      String name,
      String fhirType,
      String limit) {
    logger.info(" Data Requirement does not have Code Filter ");
    Set<Resource> resources = kd.getDataForId(req.getId(), act.getRelatedDataId(req.getId()));

    if (resources != null) {
      for (Resource res : resources) {
        ParametersParameterComponent parameter =
            createParameterWithExtension(name, limit, fhirType);
        parameter.setResource(res);
        params.addParameter(parameter);
      }
    } else {
      ParametersParameterComponent parameter = createParameterWithExtension(name, limit, fhirType);
      params.addParameter(parameter);
    }
  }

  private Parameters resolveInputParameters(
      List<DataRequirement> dataRequirements, KarProcessingData kd, BsaAction act) {
    if (dataRequirements == null || dataRequirements.isEmpty()) {
      return null;
    }

    Parameters params = new Parameters();

    for (DataRequirement req : dataRequirements) {
      String name = req.getId();
      String fhirType = req.getType();
      String limit = req.hasLimit() ? Integer.toString(req.getLimit()) : "*";

      if (req.hasCodeFilter()) {
        processCodeFilteredResources(params, req, kd, name, fhirType, limit);
      } else {
        processNonFilteredResources(params, req, kd, act, name, fhirType, limit);
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
      logger.error(NULL_FHIR_PATH_EVALUATION);
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

      logger.error(NULL_FHIR_PATH_EVALUATION);
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
}
