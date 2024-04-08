package com.drajer.bsa.controller;

import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.bsa.service.RrReceiver;
import com.drajer.ecrapp.model.ReportabilityResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;

@RestController
public class ReportabilityResponseController {

  private final Logger logger = LoggerFactory.getLogger(ReportabilityResponseController.class);

  @Autowired RrReceiver rrReceieverService;

  @Autowired PublicHealthMessagesDao phDao;

  @CrossOrigin
  @PostMapping(value = "/api/receiveReportabilityResponse")
  public ResponseEntity<String> receiveReportabilityResponse(
      @RequestHeader(name = "X-Request-ID") String xRequestIdHttpHeaderValue,
      @RequestHeader(name = "X-Correlation-ID", required = false)
          String xCorrelationIdHttpHeaderValue,
      @RequestBody ReportabilityResponse data,
      HttpServletRequest request,
      HttpServletResponse response) {
    try {

      logger.info(
          " Reportability Response received for X-Correlation-ID: {} with X-Request-ID: {}",
          StringEscapeUtils.escapeJava(xCorrelationIdHttpHeaderValue),
          StringEscapeUtils.escapeJava(xRequestIdHttpHeaderValue));

      if (data.getResponseType().contentEquals(ReportabilityResponse.MDN_RESPONSE_TYPE)) {

        logger.info(" Received MDN instead of RR on the receiveRR API ");

        if (StringUtils.isBlank(xCorrelationIdHttpHeaderValue)) {
          logger.error("X-Correlation-ID header is not present in MDN request.");
          throw new ResponseStatusException(
              HttpStatus.BAD_REQUEST, "X-Correlation-ID header is not present in MDN request.");
        }

        // For MDN, no other data will be present.
        rrReceieverService.handleFailureMdn(
            data, xCorrelationIdHttpHeaderValue, xRequestIdHttpHeaderValue);

      } else {
        logger.info(" Received RR as expected on the RR API ");

        // Handle RR and optionally save to EHR.
        rrReceieverService.handleReportabilityResponse(data, xRequestIdHttpHeaderValue);
      }

    } catch (IllegalArgumentException e) {
      logger.error("Error in processing the request ", e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, e.getMessage());
    } catch (Exception e) {
      logger.error("Error in processing the request ", e);
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, e.getMessage());
    }

    return new ResponseEntity<>("Success", HttpStatus.OK);
  }

  @CrossOrigin
  @PostMapping(value = "/api/reSubmitReportabilityResponse")
  public ResponseEntity<String> reSubmitReportabilityResponse(
      @RequestParam(name = "eicrId", required = false) String eicrId,
      @RequestParam(name = "eicrDocId", required = false) String eicrDocId) {
    try {
      logger.info(
          "Received EicrId:: {}, EicrDocId:: {} in the request",
          StringEscapeUtils.escapeJava(eicrId),
          StringEscapeUtils.escapeJava(eicrDocId));
      PublicHealthMessage phm = null;
      if (eicrId != null) {
        phm = phDao.getBySubmittedMessageId(eicrId);
      } else if (eicrDocId != null) {
        phm = phDao.getBySubmittedDataId(eicrDocId);
      } else {
        logger.error("EicrId and EicrDocId is not present in the request");
        throw new ResponseStatusException(
            HttpStatus.BAD_REQUEST, "EicrId or EicrDocId is not present in the request");
      }

      if (phm != null) {
        ReportabilityResponse rr = new ReportabilityResponse();
        rr.setRrXml(phm.getCdaResponseData());
        rr.setResponseType(phm.getResponseMessageType());

        // Always save it to the EHR.
        rrReceieverService.handleReportabilityResponse(rr, phm.getxRequestId());
      }

    } catch (IllegalArgumentException e) {
      logger.error("Error in processing the request", e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, e.getMessage());
    } catch (Exception e) {
      logger.error("Error in processing the request", e);
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, e.getMessage());
    }

    return new ResponseEntity<>("Success", HttpStatus.OK);
  }
}
