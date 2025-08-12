package com.drajer.bsa.service.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.dao.PhMessageDao;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.sof.model.PublicHealthMessageData;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.IOException;
import java.util.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class PhMessageServiceImplTest {

  @InjectMocks private PhMessageServiceImpl phMessageServiceImpl;

  @Mock private PhMessageDao phMessageDao;

  private Map<String, String> searchParam;
  private boolean summaryFlag = false;
  private List<PublicHealthMessage> expectedPublicHealthMessageDetails;

  @Before
  public void setUp() throws IOException {
    MockitoAnnotations.initMocks(this);

    expectedPublicHealthMessageDetails =
        TestUtils.readFileContents(
            "ecrTestData/PhMessageOutput/PhMessage.json",
            new TypeReference<List<PublicHealthMessage>>() {});

    searchParam =
        TestUtils.readFileContents(
            "ecrTestData/phMessageInput/searchParam.json",
            new TypeReference<Map<String, String>>() {});
  }

  @Test
  public void testGetPhMessageData() throws IOException {
    Mockito.lenient()
        .when(phMessageDao.getPhMessageData(searchParam, summaryFlag))
        .thenReturn(expectedPublicHealthMessageDetails);

    List<PublicHealthMessage> result =
        phMessageServiceImpl.getPhMessageData(searchParam, summaryFlag);

    assertEquals(expectedPublicHealthMessageDetails, result);
    assertThat(result).hasSize(expectedPublicHealthMessageDetails.size());
    verify(phMessageDao).getPhMessageData(searchParam, summaryFlag);
  }

  @Test
  public void testGetPhMessageDataSummary() {
    when(phMessageDao.getPhMessageDataSummary(searchParam))
        .thenReturn(expectedPublicHealthMessageDetails);

    List<PublicHealthMessage> result = phMessageServiceImpl.getPhMessageDataSummary(searchParam);

    assertEquals(expectedPublicHealthMessageDetails, result);
    assertThat(result).hasSize(expectedPublicHealthMessageDetails.size());
    verify(phMessageDao).getPhMessageDataSummary(searchParam);
  }

  @Test
  public void testGetPhMessageDataByXRequestIds() throws IOException {
    List<String> xRequestIds = Arrays.asList("xRequestId1", "xRequestId2");

    Mockito.lenient()
        .when(phMessageDao.getPhMessageByXRequestIds(xRequestIds, summaryFlag))
        .thenReturn(expectedPublicHealthMessageDetails);

    List<PublicHealthMessage> result =
        phMessageServiceImpl.getPhMessageDataByXRequestIds(xRequestIds, summaryFlag);

    assertEquals(expectedPublicHealthMessageDetails, result);
    assertThat(result).hasSize(expectedPublicHealthMessageDetails.size());
    verify(phMessageDao).getPhMessageByXRequestIds(xRequestIds, summaryFlag);
  }

  @Test
  public void testGetPhMessagesContainingXRequestIds() throws IOException {
    List<String> xRequestIds = Arrays.asList("xRequestId1", "xRequestId2");

    Mockito.lenient()
        .when(phMessageDao.getPhMessagesContainingXRequestIds(xRequestIds, summaryFlag))
        .thenReturn(expectedPublicHealthMessageDetails);

    List<PublicHealthMessage> result =
        phMessageServiceImpl.getPhMessagesContainingXRequestIds(xRequestIds, summaryFlag);

    assertEquals(expectedPublicHealthMessageDetails, result);
    assertThat(result).hasSize(expectedPublicHealthMessageDetails.size());
    verify(phMessageDao).getPhMessagesContainingXRequestIds(xRequestIds, summaryFlag);
  }

  @Test
  public void testGetPhMessageByParameters() throws IOException {
    PublicHealthMessageData publicHealthMessageData = new PublicHealthMessageData();
    publicHealthMessageData.setId(UUID.randomUUID());

    Mockito.lenient()
        .when(phMessageDao.getPhMessageByParameters(publicHealthMessageData))
        .thenReturn(expectedPublicHealthMessageDetails);

    List<PublicHealthMessage> result =
        phMessageServiceImpl.getPhMessageByParameters(publicHealthMessageData);

    assertThat(result).isNotEmpty();
    assertEquals(expectedPublicHealthMessageDetails, result);
    verify(phMessageDao).getPhMessageByParameters(publicHealthMessageData);
  }

  @Test
  public void testDelete() throws IOException {
    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();

    Mockito.doNothing().when(phMessageDao).delete(publicHealthMessage);

    phMessageServiceImpl.deletePhMessage(publicHealthMessage);

    verify(phMessageDao).delete(publicHealthMessage);
  }
}
