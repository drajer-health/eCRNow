package com.drajer.test.util;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;

import com.github.tomakehurst.wiremock.WireMockServer;
import java.util.concurrent.CountDownLatch;

/** Singleton handler to Wiremock. */
public class WireMockHandle {
  private static WireMockHandle sharedObjects = new WireMockHandle();
  private CountDownLatch countDownLatch;

  private WireMockHandle() {}

  /**
   * Singleton accessor
   *
   * @return WireMockHandle instance
   */
  public static WireMockHandle getInstance() {
    return sharedObjects;
  }

  private WireMockServer wireMockServer;

  /**
   * Gets the wiremock server that will be used among all tests
   *
   * @return WireMockServer instance
   * @throws Throwable exception if fails to create wiremock server
   */
  public WireMockServer getWireMockServer(int portNumber) throws Throwable {
    if (wireMockServer == null) {
      // No-args constructor will start on port 8080, no HTTPS
      wireMockServer = new WireMockServer(wireMockConfig().port(portNumber));
      countDownLatch = new CountDownLatch(1);

      Runnable wiremockRunnable =
          () -> {
            wireMockServer.start();
            countDownLatch.countDown();
          };

      Thread thread = new Thread(wiremockRunnable);
      thread.start();
      countDownLatch.await();
    } else {
      if (wireMockServer.port() != portNumber) {
        throw new RuntimeException("wiremock is running on port " + portNumber + " instead");
      }
    }
    return wireMockServer;
  }
}
