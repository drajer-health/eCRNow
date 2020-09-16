package com.drajer.routing.impl;

import static org.junit.Assert.fail;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.Properties;
import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Transport;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest(Session.class)
public class DataRoutingTest {

  ObjectMapper mapper = new ObjectMapper();

  @InjectMocks DirectEicrSender directEicrSender;

  private Transport mockTransport;
  private Session mockSession;
  private Properties mockProperties;

  @Before
  public void setup() {

    mockTransport = PowerMockito.mock(Transport.class);
    mockProperties = PowerMockito.mock(Properties.class);

    if (mockSession == null) {

      mockSession = PowerMockito.mock(Session.class);
    }
    PowerMockito.mockStatic(Session.class);
    when(Session.getInstance(any(), any())).thenReturn(mockSession);
    when(mockSession.getProperties()).thenReturn(mockProperties);
  }

  @Test
  public void sendDataTest() throws Exception {
    try {

      // Setup
      Object context =
          mapper.readValue(
              this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"),
              LaunchDetails.class);
      String data = "This is a EICR report";

      when(mockSession.getTransport("smtp")).thenReturn(mockTransport);

      // Test
      directEicrSender.sendData(context, data);

      // Validate
      verify(mockTransport, times(1)).sendMessage(any(Message.class), any());

    } catch (Exception e) {

      fail("This exception is not expected, fix the test method");
    }
  }

  @Test
  public void receiveResponseTest() throws JsonParseException, JsonMappingException, IOException {

    DirectResponseReceiver directReceiver = new DirectResponseReceiver();
    DirectResponseReceiver directResponseReceiverSpy = Mockito.spy(directReceiver);

    // Setup
    Object context =
        mapper.readValue(
            this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"),
            LaunchDetails.class);

    doNothing().when(directResponseReceiverSpy).readMail(any(), any());

    // Test
    directResponseReceiverSpy.receiveRespone(context);

    // Validate

    Mockito.verify(directResponseReceiverSpy, times(1)).readMail(any(), any());
  }
}
