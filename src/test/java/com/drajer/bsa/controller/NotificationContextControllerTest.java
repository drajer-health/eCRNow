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
}
