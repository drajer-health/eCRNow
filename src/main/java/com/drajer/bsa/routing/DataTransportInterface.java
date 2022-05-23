package com.drajer.bsa.routing;

import org.json.JSONObject;

import com.drajer.bsa.model.KarProcessingData;

/*
 * <h1>DataTransportInterface</h1>
 *
 * The class provides a method to send data to the PHA via multiple transport options.
 * The options supported in the platform currently include Direct, REST Api and FHIR.
 * For XDR, we hand off the data and the implementers have to submit using their existing XDR channels.
 *
 */
public interface DataTransportInterface {

  /**
   * The method is used to send data to a PHA/TTP using Direct Transport. As a direct client, the
   * implementer has to connect to a HISP and then send the message via Direct.
   *
   * @param data - The KarProcessingData that contains the necessary data to process and send the
   *     information.
   */
  public abstract void sendEicrDataUsingDirect(KarProcessingData data);

  /**
   * The method is used to receive data from a PHA/TTP using Direct Transport. As a direct client,
   * the implementer has to connect to a HISP and then receive the message via Direct.
   *
   * @param data - The KarProcessingData that contains the necessary data to process and receive the
   *     information.
   */
  public abstract void receiveRrDataUsingDirect(KarProcessingData data);

  /**
   * The method is used to send data to a PHA/TTP via an intermediary using RESTful API . The
   * intermediary may communicate with PHA/TTP using Direct or XDR or other mechanisms.
   *
   * @param data - The KarProcessingData that contains the necessary data to process and send the
   *     information.
   */
  public abstract JSONObject sendEicrDataUsingRestfulApi(KarProcessingData data);
}
