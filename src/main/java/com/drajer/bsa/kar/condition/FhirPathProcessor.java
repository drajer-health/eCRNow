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
import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DataRequirement.DataRequirementCodeFilterComponent;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Expression;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.ParameterDefinition;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Parameters.ParametersParameterComponent;
import org.hl7.fhir.r4.model.Procedure;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.Type;
import org.hl7.fhir.r4.model.ValueSet;
import org.javatuples.Pair;
import org.opencds.cqf.cql.evaluator.expression.ExpressionEvaluator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FhirPathProcessor implements BsaConditionProcessor {

  private final Logger logger = LoggerFactory.getLogger(FhirPathProcessor.class);
  public static final String PARAM = "return";
  public static final String CPG_PARAM_DEFINITION =
      "http://hl7.org/fhir/uv/cpg/StructureDefinition/cpg-parameterDefinition";

  ExpressionEvaluator expressionEvaluator;

  @Override
  public Boolean evaluateExpression(
      BsaCondition cond, BsaAction act, KarProcessingData kd, EhrQueryService ehrService) {

    Parameters params = kd.getParametersByActionId(act.getActionId());
    if (params == null) {

      params = resolveInputParameters(act.getInputData(), kd, act);
    }

    logger.info(" Parameters size before resolving variables = {}", params.getParameter().size());

    resolveVariables(cond, params, kd, act, ehrService);

    logger.info(" Parameters size after resolving variables = {}", params.getParameter().size());

    Parameters result =
        (Parameters)
            expressionEvaluator.evaluate(cond.getLogicExpression().getExpression(), params);
    BooleanType value = (BooleanType) result.getParameter(PARAM);

    if (value != null) {
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

      // Resolve conditions that are present at the PlanDefinition level.
      List<Expression> expressions = ((BsaFhirPathCondition) cond).getVariables();

      if (expressions != null && !expressions.isEmpty()) {

        for (Expression exp : expressions) {

          if (exp.hasLanguage() && exp.getLanguage().contentEquals("text/fhirpath")) {

            ParametersParameterComponent paramComponent = new ParametersParameterComponent();

            logger.info(" Expression before resolution {}", exp.getExpression());

            String expr = resolveContextVariables(exp.getExpression(), ehrService, kd);

            logger.info(" Expression after resolution {}", expr);

            Parameters variableResult = (Parameters) expressionEvaluator.evaluate(expr, null);

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

              Type value = variableResult.getParameter(PARAM);
              paramComponent.setName("%" + exp.getName());
              paramComponent.setValue(value);

              logger.info(" Adding Resolved Parameter {} with value {}", exp.getName(), value);
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

    if (drcfs != null) {

      for (DataRequirementCodeFilterComponent drcf : drcfs) {

        if ((drcf.getPath().toLowerCase().contains("code")
                || drcf.getPath().contains("reasonCode")
                || drcf.getPath().contains("value")
                || drcf.getPath().equals("medication"))
            && drcf.getValueSet() != null) {

          Resource vsr =
              kd.getKar().getDependentResource(ResourceType.ValueSet, drcf.getValueSet());

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
                res.put(dr.getId(), resources);
              }
            } else {
              logger.debug(" No match found for code ");
            }
          } else {
            logger.error(" Value Set not found for id {}", drcf.getValueSet());
          }
        } else {

          logger.error(" Value Set and Code not present for code filter component");
        }
      }
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

  public ExpressionEvaluator getExpressionEvaluator() {
    return expressionEvaluator;
  }

  public void setExpressionEvaluator(ExpressionEvaluator expressionEvaluator) {
    this.expressionEvaluator = expressionEvaluator;
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
            expressionEvaluator.evaluate(cond.getLogicExpression().getExpression(), params);
    BooleanType value = (BooleanType) result.getParameter(PARAM);

    if (value != null) {
      return value.getValue();
    } else {

      logger.error(
          " Null Value returned from FHIR Path Expression Evaluator : So condition not met");
      return false;
    }
  }
}
