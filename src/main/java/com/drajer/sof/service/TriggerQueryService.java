package com.drajer.sof.service;

import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.Date;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TriggerQueryService implements AbstractQueryService {

  @Autowired TriggerQueryDstu2Bundle generateDstu2Bundles;

  @Autowired TriggerQueryR4Bundle generateR4Bundles;

  private final Logger logger = LoggerFactory.getLogger(TriggerQueryService.class);

  @Override
  public FhirData getData(LaunchDetails launchDetails, Date start, Date end) {

    // Access the Token Details and pull the data based on service
    // Based on the version of EMR, create either Dstu2FhirData or STU3 or R4.
    // Data to be pulled.
    if (launchDetails.getFhirVersion().equalsIgnoreCase(FhirVersionEnum.DSTU2.toString())) {
      Dstu2FhirData dstu2FhirData = new Dstu2FhirData();
      Bundle bundle = new Bundle();
      try {
        bundle = generateDstu2Bundles.createDSTU2Bundle(launchDetails, dstu2FhirData, start, end);
      } catch (Exception e) {
        logger.error("Error in Generating the DSTU2 Bundle", e);
      }
      dstu2FhirData.setData(bundle);
      if (logger.isInfoEnabled()) {
        logger.info("Bundle Entry Size====> {}", dstu2FhirData.getData().getEntry().size());
      }
      return dstu2FhirData;

    } else if (launchDetails.getFhirVersion().equalsIgnoreCase(FhirVersionEnum.R4.toString())) {
      R4FhirData r4FhirData = new R4FhirData();
      org.hl7.fhir.r4.model.Bundle bundle = new org.hl7.fhir.r4.model.Bundle();
      try {
        bundle = generateR4Bundles.createR4Bundle(launchDetails, r4FhirData, start, end);
      } catch (Exception e) {
        logger.error("Error in Generating the R4 Bundle", e);
      }
      r4FhirData.setData(bundle);
      return r4FhirData;
    }
    return null;
  }
}
