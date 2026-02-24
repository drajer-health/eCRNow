package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.*;

import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.routing.impl.DirectTransportImpl;
import org.junit.Before;
import org.junit.Test;

public class CheckResponseTest {

  private CheckResponse checkResponse;
  private DirectTransportImpl directReceiver;

  @Before
  public void setup() {
    checkResponse = new CheckResponse();
    directReceiver = mock(DirectTransportImpl.class);
    checkResponse.setDirectReceiver(directReceiver);
  }

  @Test
  public void testSetAndGetPhDao() {
    CheckResponse action = new CheckResponse();
    PublicHealthMessagesDao dao = mock(PublicHealthMessagesDao.class);
    action.setPhDao(dao);
    assertEquals(dao, action.getPhDao());
  }

  @Test
  public void testSetAndGetDirectReceiver() {
    CheckResponse action = new CheckResponse();
    DirectTransportImpl receiver = new DirectTransportImpl();
    action.setDirectReceiver(receiver);
    assertEquals(receiver, action.getDirectReceiver());
  }

  @Test
  public void testProcess_WhenHealthcareSettingIsDirect() {
    KarProcessingData data = new KarProcessingData();
    data.setExecutionSequenceId("exec-1");
    data.setxCorrelationId("corr-123");
    NotificationContext ctx = new NotificationContext();
    ctx.setPatientId("patient-1");
    data.setNotificationContext(ctx);
    HealthcareSetting hs = new HealthcareSetting();
    hs.setId(10);
    hs.setIsDirect(true);
    hs.setDirectUser("direct@hospital.org");
    hs.setDirectPwd("password");
    hs.setDirectTlsVersion("TLSv1.2");
    data.setHealthcareSetting(hs);
    EhrQueryService ehrService = null;
    BsaActionStatus result = checkResponse.process(data, ehrService);
    verify(directReceiver, times(1)).receiveRrDataUsingDirect(data);
    assertNull(result);
    assertEquals(true, data.getHealthcareSetting().getIsDirect());
    assertEquals("corr-123", data.getxCorrelationId());
    assertEquals("patient-1", data.getNotificationContext().getPatientId());
  }

  @Test
  public void testProcess_WhenHealthcareSettingIsNotDirect() {
    KarProcessingData data = new KarProcessingData();
    data.setExecutionSequenceId("exec-2");
    HealthcareSetting hs = new HealthcareSetting();
    hs.setIsDirect(false);
    data.setHealthcareSetting(hs);
    BsaActionStatus result = checkResponse.process(data, null);
    verifyNoInteractions(directReceiver);
    assertNull(result);
    assertEquals(false, data.getHealthcareSetting().getIsDirect());
  }
}
