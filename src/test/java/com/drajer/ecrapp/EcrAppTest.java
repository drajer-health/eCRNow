package com.drajer.ecrapp;

import com.drajer.ecrapp.config.SpringConfiguration;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpMethod;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.web.client.ResourceAccessException;
import org.springframework.web.client.RestTemplate;

@SpringBootTest(classes = EcrApp.class, webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@ContextConfiguration(classes = SpringConfiguration.class)
@ActiveProfiles("test")
public class EcrAppTest {

  @Autowired private RestTemplate restTemplate;
  protected static final String URL = "http://localhost:";

  @LocalServerPort protected int port;

  @Test
  public void test1() {
    Assertions.assertNotNull(restTemplate);
    try {
      restTemplate.exchange(
          createURLWithPort("api/test2?waitFor=9700"), HttpMethod.GET, null, String.class);
    } catch (ResourceAccessException e) {
      Assertions.assertFalse(e.getMessage().toLowerCase().contains("read timed out"));
    } catch (Exception e) {
      Assertions.fail();
    }
  }

  @Test
  public void testNegative1() {
    Assertions.assertNotNull(restTemplate);
    try {
      restTemplate.exchange(
          createURLWithPort("/api/test2?waitFor=10000"), HttpMethod.GET, null, String.class);
    } catch (ResourceAccessException e) {
      Assertions.assertTrue(e.getMessage().toLowerCase().contains("read timed out"));
    } catch (Exception e) {
      Assertions.fail();
    }
  }

  protected String createURLWithPort(String uri) {
    return URL + port + uri;
  }
}
