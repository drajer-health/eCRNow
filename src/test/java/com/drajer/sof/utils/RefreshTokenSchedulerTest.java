package com.drajer.sof.utils;

import org.junit.After;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.Assert;
import org.springframework.http.MediaType;
import org.apache.http.HttpHeaders;
import org.eclipse.jetty.http.HttpStatus;
import org.json.JSONObject;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.Response;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;

import com.github.tomakehurst.wiremock.client.MappingBuilder;
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.junit.WireMockClassRule;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static org.junit.Assert.fail;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.put;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;


public class RefreshTokenSchedulerTest {
	
	
	
	@ClassRule
	public static WireMockClassRule wireMockRule = new WireMockClassRule(options().port(8089));

	@Rule
	public WireMockClassRule mockServer = wireMockRule;
	
	//private WireMockServer  wireMockServer;
	private ClientDetails clientDetails;
	private RefreshTokenScheduler token = new RefreshTokenScheduler();
	ObjectMapper mapper = new ObjectMapper();
	
	@Before
	public void setup() {
		
				
		try {
			clientDetails = mapper.readValue(this.getClass().getClassLoader().getResourceAsStream("clientDetails.json"),
					ClientDetails.class);
		} catch (IOException e) {
						
			e.printStackTrace();
			fail("This exception is not expected, fix the test");
		}
		
	}
	
	@Test
	public void testgetSystemAccessToken_Succes() {
		
		try {
			
			String accesstoken = "{\"access_token\":\"eyJraWQiOiIy\",\"scope\":\"system\\/MedicationRequest.read\",\"token_type\":\"Bearer\",\"expires_in\":570}";
			
			MappingBuilder mappingBuilder = post(urlEqualTo("/tenants/0b8a0111-e8e6-4c26-a91c-5069cbc6b1ca/protocols/oauth2/profiles/smart-v1/token"));
			ResponseDefinitionBuilder  response = aResponse()
													.withStatus(HttpStatus.OK_200)
													.withBody(accesstoken)
													.withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);	
			
			stubFor(mappingBuilder.willReturn(response));
			
			//Test
			JSONObject authresponse = token.getSystemAccessToken(clientDetails);
			
			verify(postRequestedFor(urlEqualTo("/tenants/0b8a0111-e8e6-4c26-a91c-5069cbc6b1ca/protocols/oauth2/profiles/smart-v1/token")));
			assertEquals(authresponse.getString("access_token"), "eyJraWQiOiIy");
			assertEquals(authresponse.getInt("expires_in"), 570);
			
		}catch (Exception e) {
			
			fail(e.getMessage() + ": This exception is not expected, fix the test");
		}
		
	}
	
	@Test
	public void testgetSystemAccessToken_Unauthorized() {
		
		try {
			
			MappingBuilder mappingBuilder = post(urlEqualTo("/tenants/0b8a0111-e8e6-4c26-a91c-5069cbc6b1ca/protocols/oauth2/profiles/smart-v1/token"));
			ResponseDefinitionBuilder  response = aResponse()
													.withStatus(HttpStatus.UNAUTHORIZED_401);
			stubFor(mappingBuilder.willReturn(response));
			
			//Test
			JSONObject authresponse = token.getSystemAccessToken(clientDetails);
			
			verify(postRequestedFor(urlEqualTo("/tenants/0b8a0111-e8e6-4c26-a91c-5069cbc6b1ca/protocols/oauth2/profiles/smart-v1/token")));
			assertNull(authresponse);
			
			
		}catch (Exception e) {
			
			fail(e.getMessage() + ": This exception is not expected, fix the test");
		}
		
	}
		
}
