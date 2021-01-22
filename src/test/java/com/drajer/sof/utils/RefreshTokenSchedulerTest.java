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
import com.drajer.test.util.TestUtils;
import com.github.tomakehurst.wiremock.client.MappingBuilder;
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;
import com.github.tomakehurst.wiremock.junit.WireMockClassRule;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
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

  private int wireMockPort = wireMockRule.port();

  @Before
  public void setup() {

    clientDetails =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDataEntry1.json", ClientDetails.class);
    clientDetails.setTokenURL("http://localhost:" + wireMockPort + "/authorization");
  }

  @Test
  public void testgetSystemAccessToken_Succes() {

    try {

      String accesstoken =
          "{\"access_token\":\"eyJraWQiOiIy\",\"scope\":\"system\\/MedicationRequest.read\",\"token_type\":\"Bearer\",\"expires_in\":570}";

      MappingBuilder mappingBuilder = post(urlEqualTo("/authorization"));
      ResponseDefinitionBuilder response =
          aResponse()
              .withStatus(HttpStatus.SC_OK)
              .withBody(accesstoken)
              .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);

      stubFor(mappingBuilder.willReturn(response));

      // Test
      JSONObject authresponse = token.getSystemAccessToken(clientDetails);

      verify(postRequestedFor(urlEqualTo("/authorization")));
      assertEquals("eyJraWQiOiIy", authresponse.getString("access_token"));
      assertEquals(570, authresponse.getInt("expires_in"));

    } catch (Exception e) {

      fail(e.getMessage() + ": This exception is not expected, fix the test");
    }
  }

  @Test
  public void testgetSystemAccessToken_Unauthorized() {

    try {

      MappingBuilder mappingBuilder = post(urlEqualTo("/authorization"));
      ResponseDefinitionBuilder response = aResponse().withStatus(HttpStatus.SC_UNAUTHORIZED);
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
