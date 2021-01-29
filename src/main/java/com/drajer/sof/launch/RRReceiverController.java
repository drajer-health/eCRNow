package com.drajer.sof.launch;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.ecrapp.service.EicrRRService;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;

@RestController
public class RRReceiverController {

  private final Logger logger = LoggerFactory.getLogger(RRReceiverController.class);

  @Autowired EicrRRService rrReceieverService;

  @CrossOrigin
  @RequestMapping(value = "/api/rrReceiver", method = RequestMethod.POST)
  public ResponseEntity<String> rrReceiver(
      @RequestHeader(name = "X-Request-ID") String xRequestIdHttpHeaderValue,
      @RequestHeader(name = "X-Correlation-ID", required = false)
          String xCorrelationIdHttpHeaderValue,
      @RequestBody ReportabilityResponse data,
      HttpServletRequest request,
      HttpServletResponse response) {
    try {

      logger.info(
          " X-Correlation-ID of {} and X-Request-ID of {} received on RR API ",
          xCorrelationIdHttpHeaderValue,
          xRequestIdHttpHeaderValue);

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
        logger.info(" Received RR as expected on the RR API ");

        // For MDN, no other data will be present.
        rrReceieverService.handleReportabilityResponse(data, xRequestIdHttpHeaderValue);
      }

    } catch (IllegalArgumentException e) {
      logger.error("Error in Processing the request", e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, e.getMessage());
    } catch (Exception e) {
      logger.error("Error in Processing the request", e);
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, e.getMessage());
    }

    return new ResponseEntity<>("Success", HttpStatus.OK);
  }
}
