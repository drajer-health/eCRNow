package com.drajer.bsa.service;

import com.drajer.bsa.model.KarProcessingData;

/**
 *
 *
 * <h1>KarProcessor</h1>
 *
 * This interface declares methods to apply a (Knowledge Artifact) KAR to notifications received.
 *
 * @author nbashyam
 */
public interface KarProcessor {

  /**
   * The method that applies a KAR to a specific notification context.
   *
   * @param data The complete context required including the KAR to be applied for the notification.
   */
  public void applyKarForNotification(KarProcessingData data);
}
