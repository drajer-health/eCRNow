package com.drajer.bsa.kar.condition;

import ca.uhn.fhir.fhirpath.IFhirPath;
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
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DataRequirement.DataRequirementCodeFilterComponent;
import org.hl7.fhir.r4.model.Encounter;
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

  IFhirPath fhirPathProcessor;
  ExpressionEvaluator expressionEvaluator;

  @Override
  public Boolean evaluateExpression(BsaCondition cond, BsaAction act, KarProcessingData kd) {
    Parameters params = resolveInputParameters(act.getInputData(), kd);
    Parameters result =
        (Parameters)
            expressionEvaluator.evaluate(cond.getLogicExpression().getExpression(), params);
    BooleanType value = (BooleanType) result.getParameter("return");

    return value.getValue();
  }

  public Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> filterResources(
      DataRequirement dr, KarProcessingData kd) {

    // This will have to be changed once we plugin a real FhirPath Engine.
    CheckTriggerCodeStatus ctc = new CheckTriggerCodeStatus();
    Map<String, Set<Resource>> resources = new HashMap<String, Set<Resource>>();
    Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> retVal =
        new Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>>(ctc, resources);

    logger.info(" Getting Resources by Type {}", dr.getType());

    HashSet<Resource> candidates = new HashSet<>();
    Set<Resource> inputCandidates = kd.getResourcesByType(dr.getType());
    if (inputCandidates != null) {
      candidates.addAll(inputCandidates);
    }

    // TODO: Should this be alligned based on action id or datarequirements name or something?
    for (Map.Entry<String, HashMap<String, Resource>> entry : kd.getActionOutputData().entrySet()) {
      for (Map.Entry<String, Resource> innerEntry : entry.getValue().entrySet()) {
        if (innerEntry.getValue().fhirType().equals(dr.getType())) {
          candidates.add(innerEntry.getValue());
        }
      }
    }

    if (candidates != null) {

      for (Resource res : candidates) {

        if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Condition) {

          logger.info(" Found Condition Resource {}", res.getId());
          Condition cond = (Condition) res;
          CodeableConcept cc = cond.getCode();

          filterByCode(dr, cc, kd, ctc, resources, res, false);

        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Observation) {

          logger.info(" Found Observation Resource {}", res.getId());
          Observation obs = (Observation) res;
          CodeableConcept cc = obs.getCode();

          filterByCode(dr, cc, kd, ctc, resources, res, false);

          if (obs.getValue() != null
              && obs.getValue() instanceof CodeableConcept
              && obs.getValueCodeableConcept() != null) {
            CodeableConcept ccv = obs.getValueCodeableConcept();
            filterByCode(dr, ccv, kd, ctc, resources, res, false);
          }
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.ServiceRequest) {

          logger.info(" Found ServiceRequest Resource {}", res.getId());
          ServiceRequest sr = (ServiceRequest) res;
          CodeableConcept cc = sr.getCode();

          filterByCode(dr, cc, kd, ctc, resources, res, false);
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.MedicationRequest) {

          logger.info(" Found MedicationRequest Resource {}", res.getId());
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

          logger.info(" Found MedicationAdministration Resource {}", res.getId());
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

          logger.info(" Found Procedure Resource {}", res.getId());
          Procedure pr = (Procedure) res;

          CodeableConcept cc = pr.getCode();
          filterByCode(dr, cc, kd, ctc, resources, res, false);

        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Immunization) {

          logger.info(" Found Immunization Resource {}", res.getId());
          Immunization immz = (Immunization) res;

          CodeableConcept cc = immz.getVaccineCode();
          filterByCode(dr, cc, kd, ctc, resources, res, false);
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Encounter) {

          logger.info(" Found Encounter Resource {}", res.getId());
          Encounter enc = (Encounter) res;

          CodeableConcept cc = enc.getReasonCodeFirstRep();
          filterByCode(dr, cc, kd, ctc, resources, res, false);
        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.MeasureReport) {
          if (resources.get(res.fhirType()) != null) {
            resources.get(res.fhirType()).add(res);
          } else {
            Set<Resource> resources2 = new HashSet<Resource>();
            resources2.add(res);
            resources.put(res.fhirType(), resources2);
          }
        }
      }
    }

    return retVal;
  }

  public void filterByCode(
      DataRequirement dr,
      CodeableConcept cc,
      KarProcessingData kd,
      CheckTriggerCodeStatus ctc,
      Map<String, Set<Resource>> res,
      Resource resourceMatched,
      Boolean valElem) {

    List<DataRequirementCodeFilterComponent> drcfs = dr.getCodeFilter();

    if (drcfs != null) {

      for (DataRequirementCodeFilterComponent drcf : drcfs) {

        if ((drcf.getPath().toLowerCase().contains("code")
                || drcf.getPath().contains("value")
                || drcf.getPath().equals("medication"))
            && drcf.getValueSet() != null) {

          Resource vsr =
              kd.getKar().getDependentResource(ResourceType.ValueSet, drcf.getValueSet());

          if (vsr != null) {
            logger.info(" Found Value Set {} to compare codes.", vsr.getId());

            ValueSet vs = (ValueSet) vsr;
            String matchPath = dr.getType() + "." + drcf.getPath();

            Pair<Boolean, MatchedTriggerCodes> retInfo =
                BsaServiceUtils.isCodeableConceptPresentInValueSet(vs, cc, matchPath, false);

            if (retInfo != null) {

              logger.info(
                  " Found a match for the code, adding resource {}", resourceMatched.getId());
              ctc.setTriggerMatchStatus(retInfo.getValue0());
              ctc.addMatchedTriggerCodes(retInfo.getValue1());
              if (res.get(resourceMatched.fhirType()) != null) {
                res.get(resourceMatched.fhirType()).add(resourceMatched);
              } else {
                Set<Resource> resources = new HashSet<Resource>();
                resources.add(resourceMatched);
                res.put(resourceMatched.fhirType(), resources);
              }
            } else {
              logger.info(" No match found for code ");
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
      List<DataRequirement> dataRequirements, KarProcessingData kd) {
    if (dataRequirements == null || dataRequirements.isEmpty()) {
      return null;
    }
    Parameters params = new Parameters();
    for (DataRequirement req : dataRequirements) {
      String name = req.getId();
      String fhirType = req.getType();
      String limit = req.hasLimit() ? Integer.toString(req.getLimit()) : "*";
      Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> resources = filterResources(req, kd);
      if (resources == null || resources.getValue1() == null || resources.getValue1().isEmpty()) {
        ParametersParameterComponent parameter =
            new ParametersParameterComponent().setName("%" + String.format("%s", name));
        parameter.addExtension(
            "http://hl7.org/fhir/uv/cpg/StructureDefinition/cpg-parameterDefinition",
            new ParameterDefinition().setMax(limit).setName("%" + name).setType(fhirType));
        params.addParameter(parameter);
      } else {
        for (Entry<String, Set<Resource>> entry : resources.getValue1().entrySet()) {
          if (entry.getKey().equals(fhirType)) {
            for (Resource resource : entry.getValue()) {
              ParametersParameterComponent parameter =
                  new ParametersParameterComponent().setName("%" + String.format("%s", name));
              parameter.addExtension(
                  "http://hl7.org/fhir/uv/cpg/StructureDefinition/cpg-parameterDefinition",
                  new ParameterDefinition().setMax(limit).setName("%" + name).setType(fhirType));
              parameter.setResource(resource);
              params.addParameter(parameter);
            }
          }
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
}
