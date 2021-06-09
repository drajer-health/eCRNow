package com.drajer.bsa.kar.condition;

import ca.uhn.fhir.fhirpath.IFhirPath;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.BsaServiceUtils;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DataRequirement.DataRequirementCodeFilterComponent;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ValueSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

// import ca.uhn.fhir.fhirpath.IFhirPath;

public class FhirPathProcessor implements BsaConditionProcessor {

  private final Logger logger = LoggerFactory.getLogger(FhirPathProcessor.class);

  IFhirPath fhirPathProcessor;

  @Override
  public Boolean evaluateExpression(BsaCondition cond, BsaAction act, KarProcessingData kd) {

    //  fhirPathProcessor.e
    return true;
  }

  public Set<Resource> filterResources(DataRequirement dr, KarProcessingData kd) {

    // This will have to be changed once we plugin a real FhirPath Engine.
    Set<Resource> resources = new HashSet<Resource>();

    logger.info(" Getting Resources by Type {}", dr.getType());

    Set<Resource> candidates = kd.getResourcesByType(dr.getType());

    if (candidates != null) {

      for (Resource res : candidates) {

        if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Condition) {

          logger.info(" Found Condition Resource {}", res.getId());
          Condition c = (Condition) res;

          List<DataRequirementCodeFilterComponent> drcfs = dr.getCodeFilter();

          if (drcfs != null) {

            for (DataRequirementCodeFilterComponent drcf : drcfs) {

              if (drcf.getPath().equals("code") && drcf.getValueSet() != null) {

                Resource vsr =
                    kd.getKar().getDependentResource(ResourceType.ValueSet, drcf.getValueSet());

                if (vsr != null) {

                  logger.info(" Found Value Set {} to compare codes.", vsr.getId());

                  ValueSet vs = (ValueSet) vsr;
                  if (BsaServiceUtils.isCodeableConceptPresentInValueSet(vs, c.getCode())) {

                    logger.info(" Found a match for the code, adding resource {}", c.getId());
                    resources.add(c);
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
            logger.error(
                " Code Filter Component list is null, cannot proceed with finding matches ");
          }

        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Observation) {

        }
      }
    }

    return resources;
  }
}
