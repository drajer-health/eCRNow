package com.drajer.bsa.service;

import static org.mockito.Mockito.mock;

import com.drajer.bsa.utils.SubscriptionUtils;
import com.drajer.ecrapp.util.ApplicationUtils;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.File;
import org.hl7.fhir.r4.model.Bundle;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
class SubscriptionUtilsTest {

  @Test
  public void getNotificationContextTest() {

    String resourceName = "Bsa/NotificationBundleEncounterClose.json";

    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(resourceName).getFile());
    String absolutePath = file.getAbsolutePath();

    ApplicationUtils ap = new ApplicationUtils();
    Bundle bund = ap.readBundleFromFile(absolutePath);

    HttpServletRequest request = mock(HttpServletRequest.class);
    HttpServletResponse response = mock(HttpServletResponse.class);

    SubscriptionUtils.getNotificationContext(bund, request, response);
  }
}
