package com.drajer.bsa.controller;

import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.service.NotificationContextService;
import com.drajer.sof.model.NotificationContextData;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 *
 *
 * <h1>NotificationContextController</h1>
 *
 * @author Navaneetha
 * @since 2023-09-13
 */
@RestController
public class NotificationContextController {

  private final Logger logger = LoggerFactory.getLogger(NotificationContextController.class);

  @Autowired NotificationContextService notificationContextService;

  @CrossOrigin
  @DeleteMapping(value = "/api/notificationContext")
  public String deleteNotificationContext(
      @RequestBody NotificationContextData notificationContextData,
      HttpServletRequest request,
      HttpServletResponse response)
      throws IOException {
    List<NotificationContext> notificationContextDetails =
        notificationContextService.getNotificationContextData(
            notificationContextData.getId(), notificationContextData.getFhirServerBaseUrl(),
            notificationContextData.getNotificationResourceId(),
                notificationContextData.getPatientId());
    if (notificationContextDetails != null) {
      for (NotificationContext notificationContext : notificationContextDetails) {
        notificationContextService.delete(notificationContext);
        return "NotificationContext deleted successfully.";
      }
    }
    response.sendError(HttpServletResponse.SC_NOT_FOUND, "NotificationContext Not found");
    return "NotificationContext Not found";
  }

  @GetMapping("/api/notificationContext")
  public ResponseEntity<List<NotificationContext>> getNotificationContextData(
      @RequestParam(required = false) String fhirServerUrl,
      @RequestParam(required = false) String patientId,
      @RequestParam(required = false) String notificationResourceId) {

    List<NotificationContext> contexts =
        notificationContextService.getNotificationContextData(
            null, fhirServerUrl, notificationResourceId, patientId);

    if (contexts.isEmpty()) {
      return ResponseEntity.noContent().build();
    } else {
      return ResponseEntity.ok(contexts);
    }
  }

  @GetMapping("/api/notificationContexts")
  public ResponseEntity<List<NotificationContext>> getAllNotificationContextData(
      @RequestParam Map<String, String> searchParams) {

    List<NotificationContext> contexts =
        notificationContextService.getAllNotificationContextData(null, searchParams);
    if (contexts.isEmpty()) {
      return ResponseEntity.noContent().build();
    } else {
      return ResponseEntity.ok(contexts);
    }
  }

  @GetMapping("/api/getNotificationContextForReprocessing")
  public ResponseEntity<List<NotificationContext>> getNotificationContextForReprocessing(
      @RequestParam Map<String, String> searchParams) {

    List<NotificationContext> contexts =
        notificationContextService.getNotificationContextForReprocessing(null, searchParams);
    if (contexts.isEmpty()) {
      return ResponseEntity.noContent().build();
    } else {
      return ResponseEntity.ok(contexts);
    }
  }
}
