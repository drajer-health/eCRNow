package com.drajer.bsa.controller;

import static org.junit.Assert.assertEquals;

import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.service.NotificationContextService;
import com.drajer.sof.model.NotificationContextData;
import com.drajer.test.util.TestUtils;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class NotificationContextControllerTest {

  @InjectMocks NotificationContextController notificationContextController;

  @Mock NotificationContextService notificationContextService;

  private NotificationContext notificationContext;

  private NotificationContextData notificationContextData;

  @Mock HttpServletRequest httpServletRequest;

  @Mock HttpServletResponse httpServletResponse;

  @Before
  public void setUp() {
    notificationContext =
        (NotificationContext)
            TestUtils.getResourceAsObject(
                "Bsa/NotificationContext/NotificationContext.json", NotificationContext.class);
    notificationContextData =
        (NotificationContextData)
            TestUtils.getResourceAsObject(
                "Bsa/NotificationContext/NotificationContextData.json",
                NotificationContextData.class);
  }

  @Test
  public void deleteNotificationContext() throws IOException {

    List<NotificationContext> notificationContextList = new ArrayList<>();
    notificationContextList.add(notificationContext);

    Mockito.lenient()
        .doReturn(notificationContextList)
        .when(notificationContextService)
        .getNotificationContextData(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());

    assertEquals(
        "NotificationContext deleted successfully.",
        notificationContextController.deleteNotificationContext(
            notificationContextData, httpServletRequest, httpServletResponse));
  }

  @Test
  public void deleteNotificationContext_notFound() throws IOException {

    Mockito.lenient()
        .doReturn(new ArrayList<>())
        .when(notificationContextService)
        .getNotificationContextData(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());

    String response =
        notificationContextController.deleteNotificationContext(
            notificationContextData, httpServletRequest, httpServletResponse);

    Mockito.verify(httpServletResponse)
        .sendError(HttpServletResponse.SC_NOT_FOUND, "NotificationContext Not found");

    assertEquals("NotificationContext Not found", response);
  }

  @Test
  public void getNotificationContextData_empty() {

    Mockito.when(
            notificationContextService.getNotificationContextData(
                Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ArrayList<>());

    assertEquals(
        204,
        notificationContextController
            .getNotificationContextData("url", "patient", "res")
            .getStatusCodeValue());
  }

  @Test
  public void getNotificationContextData_success() {

    List<NotificationContext> list = new ArrayList<>();
    list.add(notificationContext);

    Mockito.when(
            notificationContextService.getNotificationContextData(
                Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(list);

    assertEquals(
        200,
        notificationContextController
            .getNotificationContextData("url", "patient", "res")
            .getStatusCodeValue());
  }

  @Test
  public void getAllNotificationContextData_empty() {

    Mockito.when(
            notificationContextService.getAllNotificationContextData(
                Mockito.any(), Mockito.anyMap()))
        .thenReturn(new ArrayList<>());

    assertEquals(
        204,
        notificationContextController
            .getAllNotificationContextData(new java.util.HashMap<>())
            .getStatusCodeValue());
  }

  @Test
  public void getNotificationContextForReprocessing_empty() {

    Mockito.when(
            notificationContextService.getNotificationContextForReprocessing(
                Mockito.any(), Mockito.anyMap()))
        .thenReturn(new ArrayList<>());

    assertEquals(
        204,
        notificationContextController
            .getNotificationContextForReprocessing(new java.util.HashMap<>())
            .getStatusCodeValue());
  }

  @Test
  public void getNotificationContextForReprocessing_success() {

    List<NotificationContext> list = new ArrayList<>();
    list.add(notificationContext);

    Mockito.when(
            notificationContextService.getNotificationContextForReprocessing(
                Mockito.any(), Mockito.anyMap()))
        .thenReturn(list);

    assertEquals(
        200,
        notificationContextController
            .getNotificationContextForReprocessing(new java.util.HashMap<>())
            .getStatusCodeValue());
  }
}
