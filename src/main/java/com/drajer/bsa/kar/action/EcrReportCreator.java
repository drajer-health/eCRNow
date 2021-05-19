package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.KarProcessingData;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Resource;

public class EcrReportCreator extends ReportCreator {

  @Override
  public Resource createReport(
      KarProcessingData kd, EhrQueryService ehrService, String id, String profile) {

    // Create the report as needed by the Ecr FHIR IG
    Bundle returnBundle = new Bundle();

    return null;
  }
}
