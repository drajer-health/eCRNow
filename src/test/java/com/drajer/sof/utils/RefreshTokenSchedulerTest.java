package com.drajer.sof.utils;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import com.drajer.sof.model.ClientDetails;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.client.MappingBuilder;
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;
import com.github.tomakehurst.wiremock.junit.WireMockClassRule;
import java.io.IOException;
import org.apache.http.HttpHeaders;
import org.eclipse.jetty.http.HttpStatus;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.springframework.http.MediaType;

public class RefreshTokenSchedulerTest {

  @ClassRule
  public static WireMockClassRule wireMockRule =
      new WireMockClassRule(wireMockConfig().dynamicPort());

  @Rule public WireMockClassRule mockServer = wireMockRule;

  private ClientDetails clientDetails;
  private RefreshTokenScheduler token = new RefreshTokenScheduler();
  ObjectMapper mapper = new ObjectMapper();

  private int wireMockPort = wireMockRule.port();

  @Before
  public void setup() {

    try {
      clientDetails =
          mapper.readValue(
              this.getClass().getClassLoader().getResourceAsStream("clientDetails.json"),
              ClientDetails.class);
      clientDetails.setTokenURL("http://localhost:" + wireMockPort + "/authorization");

    } catch (IOException e) {

      e.printStackTrace();
      fail("This exception is not expected, fix the test");
    }
  }

  @Test
  public void testgetSystemAccessToken_Succes() {

    try {

      String accesstoken =
          "{\"access_token\":\"eyJraWQiOiIy\",\"scope\":\"system\\/MedicationRequest.read\",\"token_type\":\"Bearer\",\"expires_in\":570}";

      MappingBuilder mappingBuilder = post(urlEqualTo("/authorization"));
      ResponseDefinitionBuilder response =
          aResponse()
              .withStatus(HttpStatus.OK_200)
              .withBody(accesstoken)
              .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);

      stubFor(mappingBuilder.willReturn(response));

      // Test
      JSONObject authresponse = token.getSystemAccessToken(clientDetails);

      verify(postRequestedFor(urlEqualTo("/authorization")));
      assertEquals(authresponse.getString("access_token"), "eyJraWQiOiIy");
      assertEquals(authresponse.getInt("expires_in"), 570);

    } catch (Exception e) {

      fail(e.getMessage() + ": This exception is not expected, fix the test");
    }
  }

  @Test
  public void testgetSystemAccessToken_Unauthorized() {

    try {

      MappingBuilder mappingBuilder = post(urlEqualTo("/authorization"));
      ResponseDefinitionBuilder response = aResponse().withStatus(HttpStatus.UNAUTHORIZED_401);
      stubFor(mappingBuilder.willReturn(response));

      // Test
      JSONObject authresponse = token.getSystemAccessToken(clientDetails);

      verify(postRequestedFor(urlEqualTo("/authorization")));
      assertNull(authresponse);

    } catch (Exception e) {

      fail(e.getMessage() + ": This exception is not expected, fix the test");
    }
  }
}
