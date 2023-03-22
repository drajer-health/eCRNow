package com.drajer.ecrapp.controller;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.jdbc.Sql.ExecutionPhase;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.google.gson.Gson;

@RunWith(SpringRunner.class)

@ActiveProfiles("test")

@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class PhMessageControllerTest {

	private static final String BASE_URL = "http://localhost:";

	@LocalServerPort
	private int port;

	@Autowired
	private TestRestTemplate restTemplate;

	private Map<String, String> searchParam;

	private List<Map<String, Object>> exceptedPublicHealthMessageDetails;

	@BeforeEach
	public void setUp() throws IOException {

		exceptedPublicHealthMessageDetails = TestUtils.readFileContents("ecrTestData/PhMessageOutput/PhMessage.json",
				new TypeReference<List<Map<String, Object>>>() {
				});

		searchParam = (Map<String, String>) TestUtils.readFileContents("ecrTestData/phMessageInput/searchParam.json",
				new TypeReference<Map<String, String>>() {
				});

	}

	@Sql(executionPhase = ExecutionPhase.BEFORE_TEST_METHOD, scripts = "classpath:ecrTestData/sql/Insert_PhMessager.sql")
	@Test
	public void testGetEicrData() throws JsonProcessingException {

		String url = BASE_URL + port + "/api/phMessage";

		UriComponentsBuilder builder = UriComponentsBuilder.fromUriString(url)
				.queryParam("fhirServerBaseUrl", searchParam.get("fhirServerBaseUrl"))
				.queryParam("patientId", searchParam.get("patientId"))
				.queryParam("encounterId", searchParam.get("encounterId"))
				.queryParam("xRequestId", searchParam.get("xRequestId"))
				.queryParam("submittedDataId", searchParam.get("submittedDataId"))
				.queryParam("version", searchParam.get("version"))
				.queryParam("responseDataId", searchParam.get("responseDataId"))
				.queryParam("responseProcessingInstruction", searchParam.get("responseProcessingInstruction"))
				.queryParam("notifiedResourceId", searchParam.get("notifiedResourceId"))
				.queryParam("notifiedResourceType", searchParam.get("notifiedResourceType"))
				.queryParam("karUniqueId", searchParam.get("karUniqueId"))
				.queryParam("notificationId", searchParam.get("notificationId"));
		URI uri = builder.build().toUri();
		HttpHeaders headers = new HttpHeaders();
		headers.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));
		HttpEntity<Void> entity = new HttpEntity<>(headers);
		ResponseEntity<String> response = restTemplate.exchange(uri, HttpMethod.GET, entity, String.class);
		assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
		String actualResult = response.getBody();
		String expectedResult = TestUtils.toJsonString(exceptedPublicHealthMessageDetails);
		assertEquals(expectedResult, actualResult);
	}

}
