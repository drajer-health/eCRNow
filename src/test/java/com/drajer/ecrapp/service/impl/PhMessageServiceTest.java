package com.drajer.ecrapp.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.bsa.dao.PhMessageDao;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.bsa.service.impl.PhMessageServiceImpl;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class PhMessageServiceTest {

  @InjectMocks PhMessageServiceImpl phMessageServiceImpl;

  @Mock PhMessageDao phMessageDao;

  private Map<String, String> searchParam;

  private List<PublicHealthMessage> expectedPublicHealthMessageDetails;

  @BeforeEach
  public void setUp() throws IOException {

    expectedPublicHealthMessageDetails =
        (List<PublicHealthMessage>)
            TestUtils.readFileContents(
                "ecrTestData/PhMessageOutput/PhMessage.json",
                new TypeReference<List<PublicHealthMessage>>() {});

    searchParam =
        (Map<String, String>)
            TestUtils.readFileContents(
                "ecrTestData/phMessageInput/searchParam.json",
                new TypeReference<Map<String, String>>() {});
  }

  @Test
  public void testGetPhMessageData() throws IOException {

    Mockito.lenient()
        .when(phMessageDao.getPhMessageData(searchParam))
        .thenReturn(expectedPublicHealthMessageDetails);

    List<PublicHealthMessage> result = phMessageServiceImpl.getPhMessageData(searchParam);

    assertEquals(expectedPublicHealthMessageDetails, result);
  }
}
