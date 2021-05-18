package com.drajer.bsa.kar.condition;

import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.BsaServiceUtils;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DataRequirement.DataRequirementCodeFilterComponent;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ValueSet;

public class FhirPathProcessor implements BsaConditionProcessor {

  @Override
  public Boolean evaluateExpression(BsaCondition cond, BsaAction act, KarProcessingData kd) {

    return true;
  }

  public Set<Resource> filterResources(DataRequirement dr, KarProcessingData kd) {

    // This will have to be changed once we plugin a real FhirPath Engine.
    Set<Resource> resources = null;

    List<DataRequirementCodeFilterComponent> drcs = dr.getCodeFilter();

    Set<Resource> candidates = kd.getResourcesByType(dr.getType());

    if (candidates != null) {

      for (Resource res : candidates) {

        if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Condition) {

          Condition c = (Condition) res;

          List<DataRequirementCodeFilterComponent> drcfs = dr.getCodeFilter();

          if (drcfs != null) {

            for (DataRequirementCodeFilterComponent drcf : drcfs) {

              if (drcf.getPath().equals("code") && drcf.getValueSet() != null) {

                Resource vsr =
                    kd.getKar().getDependentResource(ResourceType.ValueSet, drcf.getValueSet());

                if (vsr != null) {

                  ValueSet vs = (ValueSet) vsr;
                  if (BsaServiceUtils.isCodeableConceptPresentInValueSet(vs, c.getCode())) {

                    candidates.add(c);
                  }
                }
              }
            }
          }

        } else if (res.getResourceType().toString().contentEquals(dr.getType())
            && res.getResourceType() == ResourceType.Observation) {

        }
      }
    }

    return resources;
  }
}
