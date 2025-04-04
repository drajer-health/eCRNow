package com.drajer.bsa.service;

import com.drajer.bsa.exceptions.InvalidLaunchContext;
import com.drajer.bsa.exceptions.InvalidNotification;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.PatientLaunchContext;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.hl7.fhir.r4.model.Bundle;

/**
 *
 *
 * <h1>SubscriptionNotificationReceiver Interface</h1>
 *
 * The SubscriptionNotificationReceiver Interface class defines the processNotification method for
 * implementation.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
public interface SubscriptionNotificationReceiver {

  /**
   * The method is used to process notifications received from EHR.
   *
   * @param notificationBundle The bundle containing the subscription topic and the full resource
   *     that resulted in the notification.
   * @return
   * @throws InvalidNotification
   */
  public List<KarProcessingData> processNotification(
      Bundle notificationBundle,
      HttpServletRequest request,
      HttpServletResponse response,
      PatientLaunchContext launchContext)
      throws InvalidLaunchContext, InvalidNotification;

  public List<KarProcessingData> processRelaunchNotification(
      Bundle notificationBundle,
      HttpServletRequest request,
      HttpServletResponse response,
      PatientLaunchContext launchContext,
      Boolean relaunch)
      throws InvalidLaunchContext, InvalidNotification;

  public List<KarProcessingData> reProcessNotification(
      Bundle notificationBundle,
      HttpServletRequest request,
      HttpServletResponse response,
      PatientLaunchContext launchContext,
      Boolean relaunch)
      throws InvalidLaunchContext, InvalidNotification;
}
