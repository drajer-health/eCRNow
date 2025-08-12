package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.routing.impl.DirectTransportImpl;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class CheckResponseTest {

  @InjectMocks CheckResponse checkResponse;

  @Mock HealthcareSetting healthcareSetting;
  @Mock DirectTransportImpl directReceiver;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testProcess() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("kar-001");
    kar.setKarName("Central Cancer Reporting Decision Support");
    kar.setKarVersion("r4");
    NotificationContext mockContext = mock(NotificationContext.class);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(mockContext);
    HealthcareSetting healthcareSetting1 = new HealthcareSetting();
    healthcareSetting1.setId(1);
    healthcareSetting1.setImapUrl("imaps://mail.healthcare.org");
    healthcareSetting1.setDirectUser("user@healthcare.org");
    healthcareSetting1.setDirectPwd("securePassword123");
    healthcareSetting1.setDirectTlsVersion("TLSv1.2");
    healthcareSetting1.setIsDirect(true);
    data.setHealthcareSetting(healthcareSetting1);
    data.setxCorrelationId("Xc-Id");
    EhrQueryService ehrQueryService = mock(EhrQueryService.class);
    when(healthcareSetting.getIsDirect()).thenReturn(true);
    checkResponse.setDirectReceiver(directReceiver);
    directReceiver.receiveRrDataUsingDirect(data);

    BsaActionStatus status = checkResponse.process(data, ehrQueryService);

    verify(directReceiver, times(2)).receiveRrDataUsingDirect(data);
    assertNull("Status should be null", status);
    assertEquals("Xc-Id", data.getxCorrelationId());
    assertNotNull(data.getKar());
    assertEquals("Central Cancer Reporting Decision Support", data.getKar().getKarName());
    assertTrue(data.getHealthcareSetting().getIsDirect());
  }

  @Test
  public void testSetAndGetPhDao() {
    PublicHealthMessagesDao publicHealthMessagesDao = mock(PublicHealthMessagesDao.class);
    checkResponse.setPhDao(publicHealthMessagesDao);
    assertEquals(
        publicHealthMessagesDao,
        checkResponse.getPhDao(),
        "Expected same PublicHealthMessagesDao instance");
  }

  @Test
  public void testSetAndGetDirectReceiver() {
    DirectTransportImpl directTransport = mock(DirectTransportImpl.class);
    checkResponse.setDirectReceiver(directTransport);
    assertEquals(
        directTransport,
        checkResponse.getDirectReceiver(),
        "Expected same DirectTransportImpl instance");
  }
}
