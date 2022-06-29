package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EcrMeasureReportCreator extends EcrReportCreator {

  private final Logger logger = LoggerFactory.getLogger(EcrMeasureReportCreator.class);

  @Override
  public Resource createReport(
      KarProcessingData kd, EhrQueryService ehrService, String id, String profile, BsaAction act) {

    Resource res = super.createReport(kd, ehrService, id, profile, act);

    if (res instanceof Bundle) {

      Bundle bund = (Bundle) res;

      List<String> ids = kd.getKar().getOuputVariableIdsForResourceType(ResourceType.MeasureReport);

      for (String identifier : ids) {

        Set<Resource> resources = kd.getOutputDataById(identifier);

        if (resources != null && !resources.isEmpty()) {

          for (Resource r : resources) {
            bund.addEntry(new BundleEntryComponent().setResource(r));
          }
        }
      }
    }

    return res;
  }
}
