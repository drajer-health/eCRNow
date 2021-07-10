package com.drajer.sof.utils;

import com.drajer.cda.utils.FHIRURLResourceLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class FHIRContextURLBuilder {

  private static final Logger logger = LoggerFactory.getLogger(FHIRContextURLBuilder.class);
  private static String patientID = "<patient.id>";
  private static String ehrServerURL = "<ehrServerURL>";
  private static String category = "<category>";
  private static String resourceName = "<resource.name>";
  private static String system = "<system>";
  private static String code = "<code>";

  public String getFHRContextURL(String theKey, FhirContextValues contextValues) {
    String resourseURL = FHIRURLResourceLoader.getfhirURL(theKey);
    resourseURL = this.buildFHIRUrl(resourseURL, contextValues);
    logger.info("FHIR URL Builder resource key {} ::::: resource value {}", theKey, resourseURL);
    return resourseURL;
  }

  private String buildFHIRUrl(String theResourceUrl, FhirContextValues theContextValues) {
    String fhirUrl = theResourceUrl;
    if (theResourceUrl != null) {
      while (true) {
        if (!fhirUrl.contains("<")) {
          break;
        } else {
          String resource = fhirUrl.substring(fhirUrl.indexOf("<"), fhirUrl.indexOf(">") + 1);
          fhirUrl = this.getResourceValue(fhirUrl, theContextValues, resource, null);
        }
      }
    }
    return fhirUrl;
  }

  private String getResourceValue(
      String theFhirUrl,
      FhirContextValues theContextValues,
      String theResourceKey,
      String mandatory) {
    if (theResourceKey.equals(resourceName)) {
      theFhirUrl = theFhirUrl.replaceAll(theResourceKey, theContextValues.getResourceName());
    } else if (theResourceKey.equals(ehrServerURL)) {
      theFhirUrl = theFhirUrl.replaceAll(theResourceKey, theContextValues.getEhrServerURL());
    } else if (theResourceKey.equals(patientID)) {
      theFhirUrl = theFhirUrl.replaceAll(theResourceKey, theContextValues.getPatientID());
    } else if (theResourceKey.equals(category)) {
      theFhirUrl = theFhirUrl.replaceAll(theResourceKey, theContextValues.getCategory());
    } else if (theResourceKey.equals(system)) {
      theFhirUrl = theFhirUrl.replaceAll(theResourceKey, theContextValues.getSystem());
    } else if (theResourceKey.equals(code)) {
      theFhirUrl = theFhirUrl.replaceAll(theResourceKey, theContextValues.getCode());
    }
    return theFhirUrl;
  }
}
