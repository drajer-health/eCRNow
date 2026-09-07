package com.drajer.sof.service;

import ca.uhn.fhir.context.FhirVersionEnum;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.Date;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.logging.LogLevel;
import org.springframework.stereotype.Component;

@Component
public class TriggerQueryService implements AbstractQueryService {

  private final TriggerQueryR4Bundle generateR4Bundles;

  @Autowired
  public TriggerQueryService(TriggerQueryR4Bundle generateR4Bundles) {
    this.generateR4Bundles = generateR4Bundles;
  }

  @Override
  public FhirData getData(LaunchDetails launchDetails, Date start, Date end) {

    if (launchDetails.getFhirVersion().equalsIgnoreCase(FhirVersionEnum.R4.toString())) {

      R4FhirData r4FhirData = new R4FhirData();
      try {
        org.hl7.fhir.r4.model.Bundle bundle =
            generateR4Bundles.createR4Bundle(launchDetails, r4FhirData, start, end);
        r4FhirData.setData(bundle);
      } catch (Exception e) {
        ApplicationUtils.handleException(e, "Error in Generating the R4 Bundle", LogLevel.ERROR);
      }
      return r4FhirData;
    }
    return null;
  }
}
