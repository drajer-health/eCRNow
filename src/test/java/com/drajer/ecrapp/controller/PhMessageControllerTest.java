package com.drajer.ecrapp.controller;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.drajer.bsa.controller.PhMessageController;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.bsa.service.PhMessageService;
import com.drajer.sof.model.PublicHealthMessageData;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.IOException;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@ExtendWith(MockitoExtension.class)
public class PhMessageControllerTest {

  private List<PublicHealthMessage> expectedPublicHealthMessageDetails;

  @InjectMocks private PhMessageController phMessageController;

  @Mock private PhMessageService phMessageService;

  @BeforeEach
  public void setUp() throws IOException {

    expectedPublicHealthMessageDetails =
        TestUtils.readFileContents(
            "ecrTestData/PhMessageOutput/PhMessage.json",
            new TypeReference<List<PublicHealthMessage>>() {});
  }

  @ParameterizedTest
  @CsvSource({
    "fhirServerBaseUrl1, 13, 3, 32, 25, null, 15, 19, 11, '', 12, 8, 6534236, 2023-07-04, 2023-08-04,true",
    "null, '', 3, 32, 25, '', 15, 19, 11, null, 12, 8, 6534236, 2023-07-04, 2023-08-04,false"
  })
  public void testGetPhMessageDetails(
      String fhirServerBaseUrl,
      String patientId,
      String encounterId,
      String xRequestId,
      String submittedDataId,
      String version,
      String responseDataId,
      String responseProcessingInstruction,
      String notifiedResourceId,
      String notificationId,
      String notifiedResourceType,
      String karUniqueId,
      String correlationId,
      String startTime,
      String endTime,
      String summaryFlagString) {

    boolean summaryFlag = Boolean.parseBoolean(summaryFlagString);
    Mockito.when(phMessageService.getPhMessageData(any(), anyBoolean()))
        .thenReturn(expectedPublicHealthMessageDetails);

    ResponseEntity<Object> actualResponse =
        phMessageController.getPhMessageDetails(
            fhirServerBaseUrl,
            patientId,
            encounterId,
            xRequestId,
            submittedDataId,
            version,
            responseDataId,
            responseProcessingInstruction,
            notifiedResourceId,
            notificationId,
            notifiedResourceType,
            karUniqueId,
            correlationId,
            startTime,
            endTime,
            summaryFlag);

    // Assert
    assertEquals(
        TestUtils.toJsonString(expectedPublicHealthMessageDetails),
        TestUtils.toJsonString(actualResponse.getBody()));
  }

  @Test
  public void testDeletePhMessagesWithValidInput() {
    // Create a test PublicHealthMessageData object
    PublicHealthMessageData publicHealthMessageData = new PublicHealthMessageData();
    publicHealthMessageData.setId(UUID.randomUUID());

    Mockito.lenient()
        .when(phMessageService.getPhMessageByParameters(publicHealthMessageData))
        .thenReturn(expectedPublicHealthMessageDetails);

    doNothing().when(phMessageService).deletePhMessage(any(PublicHealthMessage.class));

    ResponseEntity<String> response = phMessageController.deletePhMessages(publicHealthMessageData);

    assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
    assertThat(response.getBody()).isEqualTo("phMessages deleted successfully");

    verify(phMessageService).getPhMessageByParameters(publicHealthMessageData);
    verify(phMessageService, times(expectedPublicHealthMessageDetails.size()))
        .deletePhMessage(any(PublicHealthMessage.class));
  }
}
