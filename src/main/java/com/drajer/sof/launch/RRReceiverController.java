package com.drajer.sof.launch;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.ecrapp.service.EicrRRService;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;

@RestController
public class RRReceiverController {

  public static final String ERROR_IN_PROCESSING_THE_REQUEST = "Error in Processing the request";
  private final Logger logger = LoggerFactory.getLogger(RRReceiverController.class);

  @Autowired EicrRRService rrReceieverService;

  @CrossOrigin
  @PostMapping(value = "/api/rrReceiver")
  public ResponseEntity<String> rrReceiver(
      @RequestHeader(name = "X-Request-ID") String xRequestIdHttpHeaderValue,
      @RequestHeader(name = "X-Correlation-ID", required = false)
          String xCorrelationIdHttpHeaderValue,
      @RequestBody ReportabilityResponse data,
      @RequestParam(name = "saveToEhr", required = false, defaultValue = "true") boolean saveToEhr,
      HttpServletRequest request,
      HttpServletResponse response) {
    try {

      logger.info(
          " Reportability Response received for X-Correlation-ID: {} with X-Request-ID: {}",
          StringEscapeUtils.escapeJava(xCorrelationIdHttpHeaderValue),
          StringEscapeUtils.escapeJava(xRequestIdHttpHeaderValue));

      if (data.getResponseType().contentEquals(Eicr.MDN_RESPONSE_TYPE)) {
        logger.info(" Received MDN instead of RR on the RR API ");

        if (StringUtils.isBlank(xCorrelationIdHttpHeaderValue)) {
          logger.error("X-Correlation-ID header is not present in MDN request.");
          throw new ResponseStatusException(
              HttpStatus.BAD_REQUEST, "X-Correlation-ID header is not present in MDN request.");
        }

        // For MDN, no other data will be present.
        rrReceieverService.handleFailureMdn(
            data, xCorrelationIdHttpHeaderValue, xRequestIdHttpHeaderValue);

      } else {
        logger.info(" Received RR as expected on the RR API. ");

        // Handle RR and optionally save to EHR.
        rrReceieverService.handleReportabilityResponse(data, xRequestIdHttpHeaderValue, saveToEhr);
      }

    } catch (IllegalArgumentException e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, e.getMessage());
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, e.getMessage());
    }

    return new ResponseEntity<>("Success", HttpStatus.OK);
  }

  @CrossOrigin
  @PostMapping(value = "/api/reSubmitRR")
  public ResponseEntity<String> reSubmitRR(
      @RequestParam(name = "eicrId", required = false) String eicrId,
      @RequestParam(name = "eicrDocId", required = false) String eicrDocId) {
    try {
      logger.info(
          "Received EicrId:: {}, EicrDocId:: {} in the request",
          StringEscapeUtils.escapeJava(eicrId),
          StringEscapeUtils.escapeJava(eicrDocId));
      Eicr eicr = null;
      if (eicrId != null) {
        eicr = rrReceieverService.getEicrById(Integer.parseInt(eicrId));
      } else if (eicrDocId != null) {
        eicr = rrReceieverService.getEicrByDocId(eicrDocId);
      } else {
        logger.error("EicrId and EicrDocId is not present in the request");
        throw new ResponseStatusException(
            HttpStatus.BAD_REQUEST, "EicrId or EicrDocId is not present in the request");
      }

      if (eicr != null) {
        ReportabilityResponse rr = new ReportabilityResponse();
        rr.setRrXml(eicr.getResponseData());
        rr.setResponseType(eicr.getResponseType());

        // Always save it to the EHR.
        rrReceieverService.handleReportabilityResponse(rr, eicr.getxRequestId(), true);
      } else {
        String errMsg =
            "Failed to resubmit the RR, Eicr row not found for EicrDocId "
                + StringEscapeUtils.escapeJava(eicrDocId)
                + " OR EicrId "
                + StringEscapeUtils.escapeJava(eicrId);
        logger.info(errMsg);
        throw new IllegalArgumentException(errMsg);
      }

    } catch (IllegalArgumentException e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, e.getMessage());
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, e.getMessage());
    }

    return new ResponseEntity<>("Success", HttpStatus.OK);
  }
}
