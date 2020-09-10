package com.cerner.sof.launch.unit;

import static org.hamcrest.CoreMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.Properties;

import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Transport;

import org.apache.commons.io.IOUtils;
import org.apache.tomcat.jni.Address;
import org.hibernate.SessionFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.mockito.runners.MockitoJUnitRunner;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import static org.mockito.Mockito.eq;

import com.drajer.routing.impl.DirectEicrSender;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;


@RunWith(PowerMockRunner.class)
@PrepareForTest(Session.class)
public class DataRoutingTest {
	
	ObjectMapper mapper = new ObjectMapper();
	
	//@Spy
	@InjectMocks
	DirectEicrSender directEicrSender; //= new DirectEicrSender();
	
	private static final String FILE_NAME = "eICR Report";
	
	private Transport transport;
	
	private Session mockSession;
	
	//@Mock
	//Properties props;
	
	@Before
	public void init() {
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void sendDataTest() throws Exception
	{
		Object context = mapper.readValue(
				this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
		LaunchDetails details = (LaunchDetails) context;
		String data = getFileContentAsString("EICR_expected");
		InputStream is = IOUtils.toInputStream(data, StandardCharsets.UTF_8);
		
		//DirectEicrSender directEicrSender2 = new DirectEicrSender();
		
		//DirectEicrSender sender = mock(DirectEicrSender.class);
		//doNothing().when(directEicrSender).sendMail(details.getDirectHost(), details.getDirectUser(), details.getDirectPwd(), details.getDirectRecipient(), is, FILE_NAME);
		//Mockito.doNothing().when(directEicrSender).sendMail(details.getDirectHost(), details.getDirectUser(), details.getDirectPwd(), details.getDirectRecipient(), is, FILE_NAME);
		
		if(mockSession == null) {
			mockSession = PowerMockito.mock(Session.class);
		}
	    PowerMockito.mockStatic(Session.class);
		//Message msz = Mockito.spy(Message.class);
		
		
		//PowerMockito.mockStatic(Session.class);		
		//Session mockSession = Mockito.mock(Session.class);
		
		Properties mockProp = PowerMockito.mock(Properties.class);
		
		when(Session.getInstance(eq(mockProp), eq(null))).thenReturn(mockSession);
		//PowerMockito.when().thenReturn(session);
		
		
		Transport mockTransport = Mockito.mock(Transport.class);		
		
		when(mockSession.getTransport("smtp")).thenReturn(mockTransport);
		
		
		//doNothing().when(mockTransport).connect(Mockito.anyString(), Mockito.anyInt(), Mockito.anyString(), Mockito.anyString());
		//doNothing().when(transport).connect(details.getDirectHost(), 25, details.getDirectUser(), details.getDirectPwd());
		//doNothing().when(transport).sendMessage(Mockito.any(Message.class), Mockito.any());
		
		directEicrSender.sendData(context, data);
				
	}
	
	/*@Test
	public void sendDataTest() throws Exception
	{
		Object context = mapper.readValue(
				this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
		LaunchDetails details = (LaunchDetails) context;
		String data = getFileContentAsString("EICR_expected");
		InputStream is = IOUtils.toInputStream(data, StandardCharsets.UTF_8);
		
		//DirectEicrSender directEicrSender2 = new DirectEicrSender();
		
		//DirectEicrSender sender = mock(DirectEicrSender.class);
		doNothing().when(directEicrSender).sendMail(details.getDirectHost(), details.getDirectUser(), details.getDirectPwd(), details.getDirectRecipient(), is, FILE_NAME);
		//Mockito.doNothing().when(directEicrSender).sendMail(details.getDirectHost(), details.getDirectUser(), details.getDirectPwd(), details.getDirectRecipient(), is, FILE_NAME);
		
		
	    //PowerMockito.mockStatic(Session.class);
		//Message msz = Mockito.spy(Message.class);
		
		
		//PowerMockito.mockStatic(Session.class);
		
		
		
		//Session mockSession = Mockito.mock(Session.class);
		
		//Properties mockProp = PowerMockito.mock(Properties.class);
		
		//when(Session.getInstance(mockProp, null)).thenReturn(mockSession);
		//PowerMockito.when().thenReturn(session);
		
		
		//Transport mockTransport = Mockito.mock(Transport.class);		
		
		//when(mockSession.getTransport("smtp")).thenReturn(mockTransport);
		
		
		//doNothing().when(mockTransport).connect(Mockito.anyString(), Mockito.anyInt(), Mockito.anyString(), Mockito.anyString());
		//doNothing().when(transport).connect(details.getDirectHost(), 25, details.getDirectUser(), details.getDirectPwd());
		//doNothing().when(transport).sendMessage(Mockito.any(Message.class), Mockito.any());
		
		directEicrSender.sendData(context, data);
				
	}*/
	
	protected String getFileContentAsString(String fileName) throws IOException {
		String fileContent = "";
		InputStream stream = this.getClass().getClassLoader().getResourceAsStream(fileName);
		StringWriter writer = new StringWriter();
		IOUtils.copy(stream, writer, StandardCharsets.UTF_8);
		fileContent = writer.toString();
		stream.close();
		writer.close();
		return fileContent;

	}

}
