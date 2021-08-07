package com.drajer.bsa.kar.condition;

import ca.uhn.fhir.fhirpath.IFhirPath;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.eca.model.MatchedTriggerCodes;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DataRequirement.DataRequirementCodeFilterComponent;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Procedure;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.Type;
import org.hl7.fhir.r4.model.ValueSet;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FhirPathProcessor implements BsaConditionProcessor {

  private final Logger logger = LoggerFactory.getLogger(FhirPathProcessor.class);

  IFhirPath fhirPathProcessor;

  @Override
  public Boolean evaluateExpression(BsaCondition cond, BsaAction act, KarProcessingData kd) {

    //  fhirPathProcessor.e
    return true;
  }

  public Pair<CheckTriggerCodeStatus, Set<Resource>> filterResources(
      DataRequirement dr, KarProcessingData kd) {

    // This will have to be changed once we plugin a real FhirPath Engine.
    CheckTriggerCodeStatus ctc = new CheckTriggerCodeStatus();
    Set<Resource> resources = new HashSet<Resource>();
    Pair<CheckTriggerCodeStatus, Set<Resource>> retVal =
        new Pair<CheckTriggerCodeStatus, Set<Resource>>(ctc, resources);

    logger.info(" Getting Resources by Type {}", dr.getType());

    Set<Resource> candidates = kd.getResourcesByType(dr.getType());

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
      Set<Resource> res,
      Resource resourceMatched,
      Boolean valElem) {

    List<DataRequirementCodeFilterComponent> drcfs = dr.getCodeFilter();

    if (drcfs != null) {

      for (DataRequirementCodeFilterComponent drcf : drcfs) {

        if ((drcf.getPath().equals("code") || drcf.getPath().equals("value"))
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
              res.add(resourceMatched);
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
}
