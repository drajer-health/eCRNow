package com.drajer.routing.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.ecrapp.service.EicrRRService;
import com.drajer.sof.model.LaunchDetails;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.Properties;
import java.util.Vector;
import javax.mail.*;
import javax.mail.internet.InternetAddress;
import javax.mail.search.FlagTerm;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({Session.class})
public class DirectResponseReceiverTest {
  @InjectMocks DirectResponseReceiver receiver;
  @Mock EicrRRService rrService;
  @Mock LaunchDetails launchDetails;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testReceiveRespone_withLaunchDetails() throws Exception {
    DirectResponseReceiver spyReceiver = PowerMockito.spy(receiver);
    doNothing().when(spyReceiver).deleteMail(any(), anyString(), anyString());
    LaunchDetails details = mock(LaunchDetails.class);
    Object result = spyReceiver.receiveRespone(details);
    assertNotNull(result);
    verify(spyReceiver, times(1)).readMail(details);
  }

  @Test
  public void testReceiveRespone_withOtherObject() {
    String input = "Hello";
    Object result = receiver.receiveRespone(input);
    assertEquals(input, result);
  }

  @Test
  @Ignore
  public void testReadMail_imapUrlNull_headersLoop_xmlAttachment() throws Exception {
    when(launchDetails.getImapUrl()).thenReturn(null);
    when(launchDetails.getImapPort()).thenReturn("993");
    when(launchDetails.getDirectHost()).thenReturn("host");
    when(launchDetails.getDirectUser()).thenReturn("user");
    when(launchDetails.getDirectPwd()).thenReturn("pwd");

    DirectResponseReceiver spyReceiver = PowerMockito.spy(receiver);
    doNothing().when(spyReceiver).deleteMail(any(), anyString(), anyString());

    Session sessionMock = PowerMockito.mock(Session.class);
    Store storeMock = mock(Store.class);
    Folder folderMock = mock(Folder.class);
    Message messageMock = mock(Message.class);
    Multipart multipartMock = mock(Multipart.class);
    BodyPart bodyPartMock = mock(BodyPart.class);

    PowerMockito.mockStatic(Session.class);
    when(Session.getInstance(any(Properties.class), any())).thenReturn(sessionMock);
    when(sessionMock.getStore("imap")).thenReturn(storeMock);
    doNothing().when(storeMock).connect(anyString(), anyInt(), anyString(), anyString());
    when(storeMock.getFolder("Inbox")).thenReturn(folderMock);
    doNothing().when(folderMock).open(Folder.READ_WRITE);

    when(folderMock.search(any(FlagTerm.class))).thenReturn(new Message[] {messageMock});

    Vector<Header> headerVector = new Vector<>();
    headerVector.add(new Header("Message-ID", "12345"));
    when(messageMock.getAllHeaders()).thenReturn(headerVector.elements());

    when(messageMock.getFrom()).thenReturn(new Address[] {new InternetAddress("sender@test.com")});
    when(messageMock.getContent()).thenReturn(multipartMock);
    when(multipartMock.getCount()).thenReturn(1);
    when(multipartMock.getBodyPart(0)).thenReturn(bodyPartMock);
    when(bodyPartMock.getFileName()).thenReturn("file.XML");
    ByteArrayInputStream xmlStream =
        new ByteArrayInputStream("<xml>test</xml>".getBytes(StandardCharsets.UTF_8));
    when(bodyPartMock.getInputStream()).thenReturn(xmlStream);

    Object result = spyReceiver.receiveRespone(launchDetails);

    assertNotNull(result);
    verify(rrService, times(1))
        .handleReportabilityResponse(any(ReportabilityResponse.class), eq("12345"), eq(true));
  }

  @Test
  @Ignore
  public void testReadMail_imapUrlNonEmpty_multipleBodyParts() throws Exception {
    when(launchDetails.getImapUrl()).thenReturn("imap.test.com");
    when(launchDetails.getImapPort()).thenReturn("993");
    when(launchDetails.getDirectHost()).thenReturn("host");
    when(launchDetails.getDirectUser()).thenReturn("user");
    when(launchDetails.getDirectPwd()).thenReturn("pwd");

    DirectResponseReceiver spyReceiver = PowerMockito.spy(receiver);
    doNothing().when(spyReceiver).deleteMail(any(), anyString(), anyString());

    Session sessionMock = PowerMockito.mock(Session.class);
    Store storeMock = mock(Store.class);
    Folder folderMock = mock(Folder.class);
    Message messageMock = mock(Message.class);
    Multipart multipartMock = mock(Multipart.class);
    BodyPart bodyPartXml = mock(BodyPart.class);
    BodyPart bodyPartNonXml = mock(BodyPart.class);

    PowerMockito.mockStatic(Session.class);
    when(Session.getInstance(any(Properties.class), any())).thenReturn(sessionMock);
    when(sessionMock.getStore("imap")).thenReturn(storeMock);
    doNothing().when(storeMock).connect(anyString(), anyInt(), anyString(), anyString());
    when(storeMock.getFolder("Inbox")).thenReturn(folderMock);
    doNothing().when(folderMock).open(Folder.READ_WRITE);

    when(folderMock.search(any(FlagTerm.class))).thenReturn(new Message[] {messageMock});
    when(messageMock.getAllHeaders()).thenReturn(new Vector<Header>().elements());
    when(messageMock.getFrom()).thenReturn(null);
    when(messageMock.getContent()).thenReturn(multipartMock);
    when(multipartMock.getCount()).thenReturn(2);
    when(multipartMock.getBodyPart(0)).thenReturn(bodyPartXml);
    when(multipartMock.getBodyPart(1)).thenReturn(bodyPartNonXml);

    when(bodyPartXml.getFileName()).thenReturn("file.XML");
    ByteArrayInputStream xmlStream =
        new ByteArrayInputStream("<xml>test2</xml>".getBytes(StandardCharsets.UTF_8));
    when(bodyPartXml.getInputStream()).thenReturn(xmlStream);

    when(bodyPartNonXml.getFileName()).thenReturn("file.txt");

    Object result = spyReceiver.receiveRespone(launchDetails);

    assertNotNull(result);
    verify(rrService, times(1))
        .handleReportabilityResponse(any(ReportabilityResponse.class), anyString(), eq(true));
  }

  @Test
  public void testReadMail_exception() throws Exception {
    DirectResponseReceiver spyReceiver = PowerMockito.spy(receiver);
    doThrow(new RuntimeException("Test Exception"))
        .when(spyReceiver)
        .deleteMail(any(), anyString(), anyString());
    Object result = spyReceiver.receiveRespone(launchDetails);
    assertNotNull(result);
  }

  @Test
  public void testDeleteMail_withMessages() throws Exception {
    when(launchDetails.getImapUrl()).thenReturn("imap.test.com");
    when(launchDetails.getImapPort()).thenReturn("993");
    when(launchDetails.getDirectUser()).thenReturn("user");
    when(launchDetails.getDirectPwd()).thenReturn("pwd");
    when(launchDetails.getDirectHost()).thenReturn("host");

    Session sessionMock = PowerMockito.mock(Session.class);
    Store storeMock = mock(Store.class);
    Folder folderMock = mock(Folder.class);
    Message messageMock = mock(Message.class);

    PowerMockito.mockStatic(Session.class);
    when(Session.getInstance(any(Properties.class), any())).thenReturn(sessionMock);
    when(sessionMock.getStore("imap")).thenReturn(storeMock);
    doNothing().when(storeMock).connect(anyString(), anyInt(), anyString(), anyString());
    when(storeMock.getFolder("Inbox")).thenReturn(folderMock);
    doNothing().when(folderMock).open(Folder.READ_WRITE);

    when(folderMock.search(any(FlagTerm.class))).thenReturn(new Message[] {messageMock});
    doNothing().when(messageMock).setFlag(Flags.Flag.DELETED, true);
    doNothing().when(folderMock).close(true);

    receiver.deleteMail(launchDetails, "user", "pwd");

    verify(messageMock, times(1)).setFlag(Flags.Flag.DELETED, true);
    verify(folderMock, times(1)).close(true);
  }

  @Test
  @Ignore
  public void testReadMail_multipleHeaders() throws Exception {
    when(launchDetails.getImapUrl()).thenReturn("");
    when(launchDetails.getImapPort()).thenReturn("993");
    when(launchDetails.getDirectHost()).thenReturn("host");
    when(launchDetails.getDirectUser()).thenReturn("user");
    when(launchDetails.getDirectPwd()).thenReturn("pwd");

    DirectResponseReceiver spyReceiver = PowerMockito.spy(receiver);
    doNothing().when(spyReceiver).deleteMail(any(), anyString(), anyString());

    Session sessionMock = PowerMockito.mock(Session.class);
    Store storeMock = mock(Store.class);
    Folder folderMock = mock(Folder.class);
    Message messageMock = mock(Message.class);
    Multipart multipartMock = mock(Multipart.class);
    BodyPart bodyPartMock = mock(BodyPart.class);

    PowerMockito.mockStatic(Session.class);
    when(Session.getInstance(any(Properties.class), any())).thenReturn(sessionMock);
    when(sessionMock.getStore("imap")).thenReturn(storeMock);
    doNothing().when(storeMock).connect(anyString(), anyInt(), anyString(), anyString());
    when(storeMock.getFolder("Inbox")).thenReturn(folderMock);
    doNothing().when(folderMock).open(Folder.READ_WRITE);

    when(folderMock.search(any(FlagTerm.class))).thenReturn(new Message[] {messageMock});

    Vector<Header> headerVector = new Vector<>();
    headerVector.add(new Header("From", "someone@test.com"));
    headerVector.add(new Header("Message-ID", "msg-12345"));
    when(messageMock.getAllHeaders()).thenReturn(headerVector.elements());

    when(messageMock.getFrom()).thenReturn(new Address[] {new InternetAddress("sender@test.com")});
    when(messageMock.getContent()).thenReturn(multipartMock);
    when(multipartMock.getCount()).thenReturn(1);
    when(multipartMock.getBodyPart(0)).thenReturn(bodyPartMock);
    when(bodyPartMock.getFileName()).thenReturn("file.XML");
    when(bodyPartMock.getInputStream())
        .thenReturn(new ByteArrayInputStream("<xml>test</xml>".getBytes(StandardCharsets.UTF_8)));

    Object result = spyReceiver.receiveRespone(launchDetails);
    assertNotNull(result);
    verify(rrService, times(1)).handleReportabilityResponse(any(), eq("msg-12345"), eq(true));
  }

  @Test
  public void testReadMail_bodyPartFilenameNullOrNonXml() throws Exception {
    when(launchDetails.getImapUrl()).thenReturn("");
    when(launchDetails.getImapPort()).thenReturn("993");
    when(launchDetails.getDirectHost()).thenReturn("host");
    when(launchDetails.getDirectUser()).thenReturn("user");
    when(launchDetails.getDirectPwd()).thenReturn("pwd");

    DirectResponseReceiver spyReceiver = PowerMockito.spy(receiver);
    doNothing().when(spyReceiver).deleteMail(any(), anyString(), anyString());

    Session sessionMock = PowerMockito.mock(Session.class);
    Store storeMock = mock(Store.class);
    Folder folderMock = mock(Folder.class);
    Message messageMock = mock(Message.class);
    Multipart multipartMock = mock(Multipart.class);
    BodyPart bodyPartMock = mock(BodyPart.class);

    PowerMockito.mockStatic(Session.class);
    when(Session.getInstance(any(Properties.class), any())).thenReturn(sessionMock);
    when(sessionMock.getStore("imap")).thenReturn(storeMock);
    doNothing().when(storeMock).connect(anyString(), anyInt(), anyString(), anyString());
    when(storeMock.getFolder("Inbox")).thenReturn(folderMock);
    doNothing().when(folderMock).open(Folder.READ_WRITE);

    when(folderMock.search(any(FlagTerm.class))).thenReturn(new Message[] {messageMock});
    when(messageMock.getAllHeaders()).thenReturn(new Vector<Header>().elements());
    when(messageMock.getFrom()).thenReturn(new Address[] {new InternetAddress("sender@test.com")});
    when(messageMock.getContent()).thenReturn(multipartMock);
    when(multipartMock.getCount()).thenReturn(1);
    when(multipartMock.getBodyPart(0)).thenReturn(bodyPartMock);
    when(bodyPartMock.getFileName()).thenReturn(null); // null branch

    Object result = spyReceiver.receiveRespone(launchDetails);
    assertNotNull(result);
    verify(rrService, times(0)).handleReportabilityResponse(any(), anyString(), eq(true));
  }

  @Test
  public void testReadMail_messageFromNull() throws Exception {
    when(launchDetails.getImapUrl()).thenReturn("");
    when(launchDetails.getImapPort()).thenReturn("993");
    when(launchDetails.getDirectHost()).thenReturn("host");
    when(launchDetails.getDirectUser()).thenReturn("user");
    when(launchDetails.getDirectPwd()).thenReturn("pwd");

    DirectResponseReceiver spyReceiver = PowerMockito.spy(receiver);
    doNothing().when(spyReceiver).deleteMail(any(), anyString(), anyString());

    Session sessionMock = PowerMockito.mock(Session.class);
    Store storeMock = mock(Store.class);
    Folder folderMock = mock(Folder.class);
    Message messageMock = mock(Message.class);

    PowerMockito.mockStatic(Session.class);
    when(Session.getInstance(any(Properties.class), any())).thenReturn(sessionMock);
    when(sessionMock.getStore("imap")).thenReturn(storeMock);
    doNothing().when(storeMock).connect(anyString(), anyInt(), anyString(), anyString());
    when(storeMock.getFolder("Inbox")).thenReturn(folderMock);
    doNothing().when(folderMock).open(Folder.READ_WRITE);

    when(folderMock.search(any(FlagTerm.class))).thenReturn(new Message[] {messageMock});
    when(messageMock.getAllHeaders()).thenReturn(new Vector<Header>().elements());
    when(messageMock.getFrom()).thenReturn(null);
    when(messageMock.getContent()).thenReturn("String content");

    Object result = spyReceiver.receiveRespone(launchDetails);
    assertNotNull(result);
  }
}
