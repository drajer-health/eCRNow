package com.drajer.ecrapp.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
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
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@RunWith(MockitoJUnitRunner.class)
public class PhMessageControllerTest {

  private List<PublicHealthMessage> expectedPublicHealthMessageDetails;

  @InjectMocks private PhMessageController phMessageController;

  @Mock private PhMessageService phMessageService;

  @Before
  public void setUp() throws IOException {
    expectedPublicHealthMessageDetails =
        TestUtils.readFileContents(
            "ecrTestData/PhMessageOutput/PhMessage.json",
            new TypeReference<List<PublicHealthMessage>>() {});
  }

  // Converted from ParameterizedTest - case 1
  @Test
  public void testGetPhMessageDetails_case1() {

    Mockito.when(phMessageService.getPhMessageData(any(), anyBoolean()))
        .thenReturn(expectedPublicHealthMessageDetails);

    ResponseEntity<Object> actualResponse =
        phMessageController.getPhMessageDetails(
            "fhirServerBaseUrl1",
            "13",
            "3",
            "32",
            "25",
            null,
            "15",
            "19",
            "11",
            "",
            "12",
            "8",
            "6534236",
            "2023-07-04",
            "2023-08-04",
            true);

    assertEquals(
        TestUtils.toJsonString(expectedPublicHealthMessageDetails),
        TestUtils.toJsonString(actualResponse.getBody()));
  }

  // Converted from ParameterizedTest - case 2
  @Test
  public void testGetPhMessageDetails_case2() {

    Mockito.when(phMessageService.getPhMessageData(any(), anyBoolean()))
        .thenReturn(expectedPublicHealthMessageDetails);

    ResponseEntity<Object> actualResponse =
        phMessageController.getPhMessageDetails(
            null,
            "",
            "3",
            "32",
            "25",
            "",
            "15",
            "19",
            "11",
            null,
            "12",
            "8",
            "6534236",
            "2023-07-04",
            "2023-08-04",
            false);

    assertEquals(
        TestUtils.toJsonString(expectedPublicHealthMessageDetails),
        TestUtils.toJsonString(actualResponse.getBody()));
  }

  @Test
  public void testDeletePhMessagesWithValidInput() {

    PublicHealthMessageData publicHealthMessageData = new PublicHealthMessageData();
    publicHealthMessageData.setId(UUID.randomUUID());

    Mockito.lenient()
        .when(phMessageService.getPhMessageByParameters(publicHealthMessageData))
        .thenReturn(expectedPublicHealthMessageDetails);

    doNothing().when(phMessageService).deletePhMessage(any(PublicHealthMessage.class));

    ResponseEntity<String> response = phMessageController.deletePhMessages(publicHealthMessageData);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals("phMessages deleted successfully", response.getBody());

    verify(phMessageService).getPhMessageByParameters(publicHealthMessageData);
    verify(phMessageService, times(expectedPublicHealthMessageDetails.size()))
        .deletePhMessage(any(PublicHealthMessage.class));
  }
}
