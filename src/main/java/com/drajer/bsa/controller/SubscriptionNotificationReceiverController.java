package com.drajer.bsa.controller;

import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.server.exceptions.AuthenticationException;
import com.drajer.bsa.exceptions.InvalidLaunchContext;
import com.drajer.bsa.model.PatientLaunchContext;
import com.drajer.bsa.service.SubscriptionNotificationReceiver;
import com.drajer.bsa.utils.OperationOutcomeUtil;
import com.drajer.bsa.utils.StartupUtils;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.text.StringEscapeUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

/**
 * This controller is used to receive notifications to subscriptions that are subscribed to by the
 * BSA. The implementation here is a REST hook interface per the specification.
 *
 * @author nbashyam
 */
@RestController
public class SubscriptionNotificationReceiverController {

  private final Logger logger =
      LoggerFactory.getLogger(SubscriptionNotificationReceiverController.class);

  private static final String X_REQUEST_ID = "X-Request-ID";

  @Autowired SubscriptionNotificationReceiver subscriptionProcessor;

  @Autowired
  @Qualifier("jsonParser")
  IParser jsonParser;

  /**
   * This method is used to receive event-notifications from subscriptions.
   *
   * @param hsDetails The HealthcareSettings details passed as part of the Request Body.
   * @return This returns the HTTP Response Entity containing the JSON representation of the
   *     HealthcareSetting when successful, else returns appropriate error. Upon success a HTTP
   *     Status code of 200 is sent back. The following HTTP Errors will be sent back - 400 (BAD
   *     Request) - When the request body is not a notification FHIR R4 Bundle. - 401 (UnAuthorized)
   *     - When the incoming request does not have the security token required by the authorization
   *     server
   */
  @CrossOrigin
  @PostMapping(value = "/api/receive-notification")
  public ResponseEntity<Object> processNotification(
      @RequestBody String notificationBundle,
      HttpServletRequest request,
      HttpServletResponse response,
      PatientLaunchContext launchContext)
      throws InvalidLaunchContext {

    if (StartupUtils.hasAppStarted()) {
      String requestId = StringEscapeUtils.escapeJava(request.getHeader(X_REQUEST_ID));

      if (requestId != null && !requestId.isEmpty()) {
        if (notificationBundle != null) {

          logger.info("Notification Bundle received, Start processing notification. ");

          Bundle bund = (Bundle) jsonParser.parseResource(notificationBundle);

          if (bund != null) {

            try {
              logger.info(" Successfully parsed incoming notification as bundle ");

              subscriptionProcessor.processNotification(bund, request, response, launchContext);

              logger.info(" Finished processing notification ");

              return ResponseEntity.status(HttpStatus.OK)
                  .body(
                      OperationOutcomeUtil.createSuccessOperationOutcome(
                          "Notification processed successfully"));

            } catch (AuthenticationException e) {
              return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                  .body(
                      OperationOutcomeUtil.createErrorOperationOutcome(
                          "Unable to process notification, Error: " + e.getMessage()));
            } catch (DataIntegrityViolationException e) {
              return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY)
                  .body(
                      OperationOutcomeUtil.createErrorOperationOutcome(
                          "Unable to process notification due to Unique Constraint Violation (This happens when notification is receied for the same patient and resource for the same FHIR server), Detailed Error: "
                              + e.getMessage()));
            } catch (Exception e) {

              return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY)
                  .body(
                      OperationOutcomeUtil.createErrorOperationOutcome(
                          "Unable to process notification, Error: " + e.getMessage()));
            }

          } else {

            logger.error(
                "Unable to parse Resource Param (Has to be a Notification FHIR Bundle), hence the notification processing cannot proceed.");

            return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY)
                .body(
                    OperationOutcomeUtil.createErrorOperationOutcome(
                        "Unable to parse Resource Param in request body (Has to be a Notification FHIR R4 Bundle), "
                            + "hence the notification processing cannot proceed."));
          }

        } else {

          logger.error(
              "Unable to parse Resource Param (Has to be a Notification FHIR Bundle), hence the notification processing cannot proceed.");

          return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY)
              .body(
                  OperationOutcomeUtil.createErrorOperationOutcome(
                      "Unable to parse Resource Param in request body (Has to be a Notification FHIR R4 Bundle), "
                          + "hence the notification processing cannot proceed."));
        }
      } else {
        logger.error("Unable to continue since the X-Request-ID header is missing");

        return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY)
            .body(
                OperationOutcomeUtil.createErrorOperationOutcome(
                    "Unable to process notification since the X-Request-ID HTTP Header is missing"));
      }
    } else {
      logger.error(" Unable to process notification due to application startup delay");
      return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY)
          .body(
              OperationOutcomeUtil.createErrorOperationOutcome(
                  "Unable to process notification since the app has not started yet, wait till "
                      + StartupUtils.getPatientLaunchInstanceTime().toString()
                      + " for the application to startup and then send notifications"));
    }
  }
}
