package com.drajer.ecrapp.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;

import java.io.IOException;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.service.PhMessageService;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;

@ExtendWith(MockitoExtension.class)
public class PhMessageControllerTest {

	private List<PublicHealthMessage> expectedPublicHealthMessageDetails;

	@InjectMocks
	private PhMessageController phMessageController;

	@Mock
	private PhMessageService phMessageService;

	@BeforeEach
	public void setUp() throws IOException {

		expectedPublicHealthMessageDetails = TestUtils.readFileContents("ecrTestData/PhMessageOutput/PhMessage.json",
				new TypeReference<List<PublicHealthMessage>>() {
				});

	}

	@ParameterizedTest
	@CsvSource({ "fhirServerBaseUrl1, 13, 3, 32, 25, null, 15, 19, 11, '', 12, 8",
			"null, '', 3, 32, 25, '', 15, 19, 11, null, 12, 8" })
	public void testGetPhMessageDetails(String fhirServerBaseUrl, String patientId, String encounterId,
			String xRequestId, String submittedDataId, String version, String responseDataId,
			String responseProcessingInstruction, String notifiedResourceId, String notificationId,
			String notifiedResourceType, String karUniqueId) {

		Mockito.when(phMessageService.getPhMessageData(any())).thenReturn(expectedPublicHealthMessageDetails);

		ResponseEntity<Object> actualResponse = phMessageController.getPhMessageDetails(fhirServerBaseUrl, patientId,
				encounterId, xRequestId, submittedDataId, version, responseDataId, responseProcessingInstruction,
				notifiedResourceId, notificationId, notifiedResourceType, karUniqueId);

		// Assert
		assertEquals(TestUtils.toJsonString(expectedPublicHealthMessageDetails),
				TestUtils.toJsonString(actualResponse.getBody()));
	}

}