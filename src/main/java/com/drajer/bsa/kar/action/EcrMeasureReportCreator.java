package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;

import java.time.Instant;
import java.util.Date;
import java.util.Set;
import java.util.UUID;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EcrMeasureReportCreator extends EcrReportCreator {

  private final Logger logger = LoggerFactory.getLogger(EcrMeasureReportCreator.class);

  @Override
  public Resource createReport(
          KarProcessingData kd, EhrQueryService ehrService, Set<Resource> inputData, String id, String profile, BsaAction act) {
    logger.info("Ecr Measure Report Creator is executing");

    Resource res = super.createReport(kd, ehrService, id, profile, act);

    if (res instanceof Bundle bund) {

      Bundle measureReportBundle = createMeasureReportBundle(inputData);

      bund.addEntry(new BundleEntryComponent().setResource(measureReportBundle));
    }

    return res;
  }

  private Bundle createMeasureReportBundle(Set<Resource> resources) {
    Bundle bundle = new Bundle();

    bundle.setId(UUID.randomUUID().toString());
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.setTimestamp(Date.from(Instant.now()));

    for (Resource r: resources) {
      bundle.addEntry(new BundleEntryComponent().setResource(r));
    }

    return bundle;
  }
}
