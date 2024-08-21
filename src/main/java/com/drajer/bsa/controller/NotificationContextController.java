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
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.RequestBody;
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
}
