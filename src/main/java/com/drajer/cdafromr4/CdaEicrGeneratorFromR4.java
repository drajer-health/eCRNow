package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import org.apache.commons.text.StringEscapeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaEicrGeneratorFromR4 {

  private static final Logger logger = LoggerFactory.getLogger(CdaEicrGeneratorFromR4.class);

  private CdaEicrGeneratorFromR4() {}

  public static String convertR4FhirBundletoCdaEicr(
      R4FhirData data, LaunchDetails details, Eicr ecr) {

    StringBuilder eICR = new StringBuilder();

    if (data != null) {

      logger.info("Preparing Jurisdiction data");
      data.prepareJurisdicationData();

      logger.debug("R4FhirData before eICR generation : {}", data);

      if (Boolean.TRUE.equals(data.hasRequiredDataForEicr())) {
        logger.info("Bundle has required data for Eicr");
        eICR.append(CdaHeaderGenerator.createCdaHeader(data, details, ecr));
        eICR.append(CdaBodyGenerator.generateCdaBody(data, details));
        eICR.append(CdaGeneratorUtils.getEndXMLHeaderForCdaDocument());

        logger.debug("Created new eICR {}", StringEscapeUtils.escapeXml11(eICR.toString()));

      } else {
        String msg = "Fhir Data not ready/missing to be used for creating a CDA Document";
        logger.error(msg);
        throw new RuntimeException(msg);
      }

    } else {

      logger.error("No Fhir Bundle Available to create CDA Documents");
    }

    return eICR.toString();
  }
}
