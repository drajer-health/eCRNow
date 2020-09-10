package com.cerner.sof.launch.unit;


import static org.mockito.Mockito.when;
import static org.mockito.Mockito.any;

import java.util.Properties;

import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Transport;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;

import com.drajer.routing.impl.DirectEicrSender;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.databind.ObjectMapper;


@RunWith(PowerMockRunner.class)
@PrepareForTest(Session.class)
public class DataRoutingTest {
	
	ObjectMapper mapper = new ObjectMapper();
		
	@InjectMocks
	DirectEicrSender directEicrSender;
	
	private Transport mockTransport;	
	private Session mockSession;
	private Properties mockProperties;
	
	@Before
	public void setup() {		
		
		mockTransport = PowerMockito.mock(Transport.class);
		mockProperties = PowerMockito.mock(Properties.class);
		
		if(mockSession == null) {
			
			mockSession = PowerMockito.mock(Session.class);
		}
	    PowerMockito.mockStatic(Session.class);
	    when(Session.getInstance(any(), any())).thenReturn(mockSession);
	    when(mockSession.getProperties()).thenReturn(mockProperties);
	    
	}
	
	@Test
	public void sendDataTest() throws Exception
	{
		try {
			
			//Setup
			Object context = mapper.readValue(
					this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
			String data = "This is a EICR report";		
			
			when(mockSession.getTransport("smtp")).thenReturn(mockTransport);
			
			//Test		
			directEicrSender.sendData(context, data);
			
			//Validate
			verify(mockTransport, times(1)).sendMessage(any(Message.class), any());
			
		}catch( Exception e) {
			
			fail("This exception is not expected, fix the test method");
		}
				
	}
	
}
