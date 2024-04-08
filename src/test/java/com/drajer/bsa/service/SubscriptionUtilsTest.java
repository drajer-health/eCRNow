package com.drajer.bsa.service;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

import com.drajer.bsa.model.PatientLaunchContext;
import com.drajer.bsa.utils.SubscriptionUtils;
import com.drajer.ecrapp.util.ApplicationUtils;
import java.io.File;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.hl7.fhir.r4.model.Bundle;
import org.junit.jupiter.api.Test;

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
    PatientLaunchContext launchContext = new PatientLaunchContext();

    SubscriptionUtils.getNotificationContext(bund, request, response, false, launchContext);
  }
}
