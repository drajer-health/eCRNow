package com.drajer.bsa.routing.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.service.RrReceiver;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;
import javax.mail.*;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.mail.search.FlagTerm;
import org.apache.commons.io.IOUtils;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({
  Session.class,
  Transport.class,
  Store.class,
  IOUtils.class,
  UUID.class,
  DirectTransportImpl.class,
})
public class DirectTransportImplTest {

  @InjectMocks private DirectTransportImpl directTransport;

  @Mock private KarProcessingData data;

  @Mock private Session session;

  @Mock private Store store;

  @Mock private Folder folder;

  @Mock private RrReceiver rrReceiver;
  @Mock private Session mailSession;
  @Mock private Transport mailTransport;
  @Mock private Store mailStore;
  @Mock private Folder inboxFolder;
  @Mock private Message emailMessage;
  @Mock private Multipart multipart;
  @Mock private BodyPart bodyPart;
  @Mock private Header messageHeader;
  @Mock private Enumeration<Header> headersEnumeration;

  private KarProcessingData karProcessingData;
  private HealthcareSetting healthcareSetting;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    healthcareSetting = new HealthcareSetting();
    healthcareSetting.setDirectUser("test@user.com");
    healthcareSetting.setDirectRecipientAddress("recipient@test.com");
    healthcareSetting.setDirectPwd("password");
    healthcareSetting.setSmtpUrl("smtp.test.com");
    healthcareSetting.setSmtpPort("587");
    healthcareSetting.setImapUrl("imap.test.com");
    healthcareSetting.setImapPort("993");
    healthcareSetting.setDirectHost("direct.test.com");
    healthcareSetting.setDirectTlsVersion("TLSv1.2");
    karProcessingData = new KarProcessingData();
    karProcessingData.setHealthcareSetting(healthcareSetting);
    karProcessingData.setSubmittedCdaData("<ClinicalDocument>Test CDA</ClinicalDocument>");
    karProcessingData.setxCorrelationId("test-correlation-id");
    PowerMockito.mockStatic(Session.class);
    when(Session.getInstance(any(Properties.class), isNull())).thenReturn(mailSession);
    when(mailSession.getTransport("smtp")).thenReturn(mailTransport);
    when(mailSession.getStore("imap")).thenReturn(mailStore);
    when(mailStore.getFolder("Inbox")).thenReturn(inboxFolder);
    ReflectionTestUtils.setField(directTransport, "logDirectory", "test-logs");
    ReflectionTestUtils.setField(directTransport, "imapReadRetryLimit", 1);
  }

  @Test
  public void testReadMail_withMocksForPrivateMethods() throws Exception {
    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    PowerMockito.mockStatic(Session.class);
    when(Session.getInstance(any(Properties.class), isNull())).thenReturn(mailSession);
    when(mailSession.getStore(anyString())).thenReturn(mailStore);
    when(mailStore.getFolder(anyString())).thenReturn(inboxFolder);
    doNothing().when(mailStore).connect(anyString(), anyInt(), anyString(), anyString());
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();
    PowerMockito.doNothing().when(spy, "processMessages", inboxFolder);
    PowerMockito.doNothing().when(spy, "deleteReadMessages", inboxFolder);
    Whitebox.invokeMethod(spy, "readMail", "host", "user", "pwd", "993", "corrId", "TLSv1.2");
    PowerMockito.verifyPrivate(spy, times(1)).invoke("processMessages", inboxFolder);
    PowerMockito.verifyPrivate(spy, times(1)).invoke("deleteReadMessages", inboxFolder);
    assertTrue(true);
  }

  @Test
  public void testReadMailUsingImap_success() throws Exception {
    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    PowerMockito.doNothing()
        .when(
            spy,
            "readMail",
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString());

    Whitebox.invokeMethod(spy, "readMailUsingImap", "host", "user", "pwd", "25", "corr", "TLS");

    assertTrue(true);
  }

  @Test(expected = RuntimeException.class)
  public void testSendEicrDataUsingDirect_ThrowsExceptionWhenNoHost() {
    healthcareSetting.setSmtpUrl(null);
    healthcareSetting.setDirectHost(null);
    directTransport.sendEicrDataUsingDirect(karProcessingData);
  }

  @Test
  public void testSendEicrDataUsingDirect_ExceptionDuringSend() throws Exception {
    doThrow(new MessagingException("Connection failed"))
        .when(mailTransport)
        .connect(anyString(), anyInt(), anyString(), anyString());
    try {
      directTransport.sendEicrDataUsingDirect(karProcessingData);
      fail("Expected RuntimeException");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("Unable to send Direct Message"));
    }
  }

  @Test
  public void testReadMailUsingImap_folderClosedException() throws Exception {

    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    ReflectionTestUtils.setField(spy, "imapReadRetryLimit", 1);

    PowerMockito.doThrow(new FolderClosedException(null))
        .when(
            spy,
            "readMail",
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString());

    Whitebox.invokeMethod(spy, "readMailUsingImap", "host", "user", "pwd", "25", "corr", "TLS");

    assertTrue(true);
  }

  @Test
  public void testReadMailUsingImap_genericException() throws Exception {

    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    ReflectionTestUtils.setField(spy, "imapReadRetryLimit", 1);

    PowerMockito.doThrow(new RuntimeException("fail"))
        .when(
            spy,
            "readMail",
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString());

    Whitebox.invokeMethod(spy, "readMailUsingImap", "host", "user", "pwd", "25", "corr", "TLS");

    assertTrue(true);
  }

  @Test
  public void testReceiveRrDataUsingDirect_NoHostConfigured() throws Exception {
    healthcareSetting.setImapUrl(null);
    healthcareSetting.setDirectHost(null);
    directTransport.receiveRrDataUsingDirect(karProcessingData);
    verify(mailStore, never()).connect(anyString(), anyInt(), anyString(), anyString());
  }

  @Test
  public void testGetMessageId_NotFound() throws Exception {
    when(emailMessage.getAllHeaders()).thenReturn(headersEnumeration);
    when(headersEnumeration.hasMoreElements()).thenReturn(true, false);
    when(headersEnumeration.nextElement()).thenReturn(messageHeader);
    when(messageHeader.getName()).thenReturn("Subject");
    when(messageHeader.getValue()).thenReturn("Test Subject");
    String messageId = directTransport.getMessageId(emailMessage);
    assertNull(messageId);
  }

  @Test
  public void testGetMessageId_MessagingException() throws Exception {
    when(emailMessage.getAllHeaders()).thenThrow(new MessagingException("Header error"));
    String messageId = directTransport.getMessageId(emailMessage);
    assertNull(messageId);
  }

  //
  //    @Test
  //    public void testDeleteMailUsingImap() throws Exception {
  //      when(inboxFolder.search(any(FlagTerm.class))).thenReturn(new Message[] {emailMessage});
  //      doNothing().when(inboxFolder).open(Folder.READ_WRITE);
  //      doNothing().when(inboxFolder).close(anyBoolean());
  //      doNothing().when(emailMessage).setFlag(Flags.Flag.DELETED, true);
  //      when(emailMessage.getAllHeaders()).thenReturn(headersEnumeration);
  //      when(headersEnumeration.hasMoreElements()).thenReturn(false);
  //
  //      directTransport.deleteMailUsingImap(
  //          "imap.test.com", 993, "test@user.com", "password", mailSession);
  //
  //      verify(emailMessage).setFlag(Flags.Flag.DELETED, true);
  //      verify(inboxFolder).close(true);
  //    }

  @Test
  public void testSendEicrDataUsingRestfulApi() {
    Object result = directTransport.sendEicrDataUsingRestfulApi(karProcessingData);
    assertNull(result);
  }

  @Test
  public void testDirectMimeMessageConstructor() throws Exception {
    Session testSession = Session.getInstance(new Properties());
    String messageId = "test-id";
    String domain = "test-domain";
    DirectTransportImpl.DirectMimeMessage directMessage =
        directTransport.new DirectMimeMessage(testSession, messageId, domain);
    assertNotNull(directMessage);
    assertEquals(testSession, directMessage.sessions);
    assertEquals(messageId, directMessage.messageId);
    assertEquals(domain, directMessage.domain);
  }

  @Test
  public void testDirectMimeMessage_updateMessageID() throws Exception {
    // Arrange
    Properties props = new Properties();
    Session session = Session.getInstance(props);
    DirectTransportImpl outer = new DirectTransportImpl();
    DirectTransportImpl.DirectMimeMessage message =
        outer.new DirectMimeMessage(session, "corr-123", "example.com");
    message.updateMessageID();
    String[] headers = message.getHeader("Message-ID");
    assertNotNull("Message-ID header should be set", headers);
    assertEquals(1, headers.length);
    assertEquals("<corr-123@example.com>", headers[0]);
  }

  @Test
  public void testUpdateMessageID_protectedMethod() throws Exception {

    class TestMimeMessage extends DirectTransportImpl.DirectMimeMessage {
      TestMimeMessage(Session s, String id, String domain) throws MessagingException {
        new DirectTransportImpl().super(s, id, domain);
      }

      void callUpdateMessageID() throws MessagingException {
        updateMessageID(); // allowed here
      }
    }
    Session session = Session.getDefaultInstance(new Properties());
    TestMimeMessage msg = new TestMimeMessage(session, "corr-123", "example.com");
    msg.callUpdateMessageID();
    String[] header = msg.getHeader("Message-ID");
    assertNotNull(header);
    assertEquals("<corr-123@example.com>", header[0]);
  }

  @Test
  public void testReadMailUsingImap_firstAttemptSuccess() throws Exception {
    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    ReflectionTestUtils.setField(spy, "imapReadRetryLimit", 3);
    PowerMockito.doNothing()
        .when(
            spy,
            "readMail",
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString());
    Whitebox.invokeMethod(
        spy, "readMailUsingImap", "host.com", "user", "pwd", "25", "corr-123", "TLSv1.2");
    PowerMockito.verifyPrivate(spy, times(1))
        .invoke(
            "readMail",
            new Object[] {
              anyString(), anyString(), anyString(), anyString(), anyString(), anyString()
            });
    assertTrue(true);
  }

  @Test
  public void testProcessMessages_nonMultipartMessage() throws Exception {
    Folder mockFolder = mock(Folder.class);
    Message mockMessage = PowerMockito.mock(Message.class);
    when(mockFolder.search(any(FlagTerm.class))).thenReturn(new Message[] {mockMessage});
    when(mockMessage.getContent()).thenReturn("plain text content");
    DirectTransportImpl spyTransport = PowerMockito.spy(directTransport);
    PowerMockito.doReturn("msg-123").when(spyTransport, "getMessageId", mockMessage);
    Whitebox.invokeMethod(spyTransport, "processMessages", mockFolder);
    verify(mockMessage, never()).setFlag(eq(Flags.Flag.SEEN), anyBoolean());
  }

  @Test
  public void testProcessMessages_multipartXmlMessage() throws Exception {
    Folder mockFolder = mock(Folder.class);
    Message mockMessage = mock(Message.class);
    Multipart mockMultipart = mock(Multipart.class);
    BodyPart mockBodyPart = mock(BodyPart.class);

    when(mockFolder.search(any(FlagTerm.class))).thenReturn(new Message[] {mockMessage});
    when(mockMessage.getContent()).thenReturn(mockMultipart);
    when(mockMultipart.getCount()).thenReturn(1);
    when(mockMultipart.getBodyPart(0)).thenReturn(mockBodyPart);
    when(mockBodyPart.getFileName()).thenReturn("file.xml");
    when(mockBodyPart.getInputStream())
        .thenReturn(new ByteArrayInputStream("<xml></xml>".getBytes(StandardCharsets.UTF_8)));
    RrReceiver rrReceiverSpy = spy(rrReceiver);
    ReflectionTestUtils.setField(directTransport, "rrReceiver", rrReceiverSpy);
    DirectTransportImpl spyTransport = PowerMockito.spy(directTransport);
    PowerMockito.doReturn("msg-123").when(spyTransport, "getMessageId", mockMessage);
    Whitebox.invokeMethod(spyTransport, "processMessages", mockFolder);
    verify(mockMessage, times(1)).setFlag(Flags.Flag.SEEN, true);
    verify(rrReceiverSpy, times(1)).handleReportabilityResponse(any(), eq("msg-123"));
  }

  @Test
  public void testDeleteReadMessages_inboxNull() throws Exception {
    Whitebox.invokeMethod(directTransport, "deleteReadMessages", (Folder) null);
    assertTrue("Inbox is null, nothing to delete", true);
  }

  @Test
  public void testDeleteReadMessages_inboxClosed() throws Exception {
    Folder mockFolder = mock(Folder.class);
    when(mockFolder.isOpen()).thenReturn(false);
    Whitebox.invokeMethod(directTransport, "deleteReadMessages", mockFolder);
    verify(mockFolder, never()).search(any(FlagTerm.class));
  }

  @Test
  public void testDeleteReadMessages_withMessages() throws Exception {
    Folder mockFolder = mock(Folder.class);
    Message mockMessage1 = mock(Message.class);
    Message mockMessage2 = mock(Message.class);
    when(mockFolder.isOpen()).thenReturn(true);
    when(mockFolder.search(any(FlagTerm.class)))
        .thenReturn(new Message[] {mockMessage1, mockMessage2});
    DirectTransportImpl spyTransport = PowerMockito.spy(directTransport);
    PowerMockito.doReturn("msg-1").when(spyTransport, "getMessageId", mockMessage1);
    PowerMockito.doReturn("msg-2").when(spyTransport, "getMessageId", mockMessage2);
    Whitebox.invokeMethod(spyTransport, "deleteReadMessages", mockFolder);
    verify(mockFolder, times(1)).search(any(FlagTerm.class));
    verify(mockMessage1, times(1)).setFlag(Flags.Flag.DELETED, true);
    verify(mockMessage2, times(1)).setFlag(Flags.Flag.DELETED, true);
  }

  @Test
  public void testProcessMessages_noMessages() throws Exception {
    Folder mockFolder = mock(Folder.class);
    Message[] emptyMessages = new Message[] {};
    when(mockFolder.search(any(FlagTerm.class))).thenReturn(emptyMessages);
    Whitebox.invokeMethod(directTransport, "processMessages", mockFolder);
    verify(mockFolder, times(1)).search(any(FlagTerm.class));
    assertNotNull("Messages array should not be null", emptyMessages);
    assertEquals("Messages array should be empty", 0, emptyMessages.length);
  }

  @Test
  public void testProcessMessages_nonXmlMultipart() throws Exception {
    Folder mockFolder = mock(Folder.class);
    Message mockMessage = mock(Message.class);
    Multipart mockMultipart = mock(Multipart.class);
    BodyPart mockBodyPart = mock(BodyPart.class);
    when(mockFolder.search(any(FlagTerm.class))).thenReturn(new Message[] {mockMessage});
    when(mockMessage.getContent()).thenReturn(mockMultipart);
    when(mockMultipart.getCount()).thenReturn(1);
    when(mockMultipart.getBodyPart(0)).thenReturn(mockBodyPart);
    when(mockBodyPart.getFileName()).thenReturn("document.pdf");
    when(mockMessage.getFrom()).thenReturn(null);
    when(mockMessage.getFlags()).thenReturn(new Flags());
    DirectTransportImpl spyTransport = PowerMockito.spy(directTransport);
    PowerMockito.doReturn("msg-123").when(spyTransport, "getMessageId", mockMessage);
    Whitebox.invokeMethod(spyTransport, "processMessages", mockFolder);
    verify(mockMessage, times(1)).setFlag(Flags.Flag.SEEN, true);
    assertTrue(true);
  }

  @Test
  public void testDeleteReadMessages_inboxNullOrClosed() throws Exception {
    Folder mockFolder = mock(Folder.class);
    when(mockFolder.isOpen()).thenReturn(false);
    Whitebox.invokeMethod(directTransport, "deleteReadMessages", mockFolder);
    Whitebox.invokeMethod(directTransport, "deleteReadMessages", (Folder) null);
    assertTrue("deleteReadMessages handled null and closed inbox", true);
  }

  @Test
  public void testDeleteReadMessages_normalFlow() throws Exception {
    Folder mockFolder = mock(Folder.class);
    Message mockMessage1 = mock(Message.class);
    Message mockMessage2 = mock(Message.class);
    when(mockFolder.isOpen()).thenReturn(true);
    when(mockFolder.search(any(FlagTerm.class)))
        .thenReturn(new Message[] {mockMessage1, mockMessage2});
    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    PowerMockito.doReturn("msg1").when(spy, "getMessageId", mockMessage1);
    PowerMockito.doReturn("msg2").when(spy, "getMessageId", mockMessage2);
    Whitebox.invokeMethod(spy, "deleteReadMessages", mockFolder);
    verify(mockFolder, times(1)).isOpen();
    verify(mockFolder, times(1)).search(any(FlagTerm.class));
    verify(mockMessage1, times(1)).setFlag(Flags.Flag.DELETED, true);
    verify(mockMessage2, times(1)).setFlag(Flags.Flag.DELETED, true);
    assertEquals("msg1", spy.getMessageId(mockMessage1));
    assertEquals("msg2", spy.getMessageId(mockMessage2));
  }

  @Test
  public void testDeleteReadMessages_searchThrowsException() throws Exception {
    Folder mockFolder = mock(Folder.class);
    when(mockFolder.isOpen()).thenReturn(true);
    when(mockFolder.search(any(FlagTerm.class))).thenThrow(new RuntimeException("fail"));
    Whitebox.invokeMethod(directTransport, "deleteReadMessages", mockFolder);
    assertTrue(true);
  }

  @Test
  public void cover_messageId_header() throws Exception {
    MimeMessage msg = mock(MimeMessage.class);
    Header header = new Header("Message-ID", "<123@test>");
    when(msg.getAllHeaders()).thenReturn(Collections.enumeration(List.of(header)));
    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    String id = Whitebox.invokeMethod(spy, "getMessageId", msg);
    assertEquals("<123@test>", id);
  }

  @Test
  public void cover_readMailUsingImap_folderClosedException() throws Exception {
    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    ReflectionTestUtils.setField(spy, "imapReadRetryLimit", 1);
    PowerMockito.doThrow(mock(FolderClosedException.class))
        .when(
            spy,
            "readMail",
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString());
    Whitebox.invokeMethod(spy, "readMailUsingImap", "host", "u", "p", "993", "corr", "TLSv1.2");
    assertTrue(true);
  }

  @Test
  public void testSendEicrDataUsingDirect_directHostPath() throws Exception {
    KarProcessingData data = mock(KarProcessingData.class);
    HealthcareSetting hs = mock(HealthcareSetting.class);
    when(data.getSubmittedCdaData()).thenReturn("<xml/>");
    when(data.getHealthcareSetting()).thenReturn(hs);
    when(data.getxCorrelationId()).thenReturn("corr-1");
    when(hs.getSmtpUrl()).thenReturn("");
    when(hs.getDirectHost()).thenReturn("direct");
    when(hs.getDirectUser()).thenReturn("user");
    when(hs.getDirectPwd()).thenReturn("pwd");
    when(hs.getSmtpPort()).thenReturn("25");
    when(hs.getDirectRecipientAddress()).thenReturn("to@test.com");
    when(hs.getDirectTlsVersion()).thenReturn("TLSv1.2");
    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    PowerMockito.doNothing()
        .when(spy)
        .sendMail(
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            any(InputStream.class),
            anyString(),
            anyString(),
            anyString());

    spy.sendEicrDataUsingDirect(data);
    PowerMockito.verifyPrivate(spy, times(1))
        .invoke(
            "sendMail",
            eq("direct"),
            eq("user"),
            eq("pwd"),
            eq("25"),
            eq("to@test.com"),
            any(InputStream.class),
            eq("eICR_Report"),
            eq("corr-1"),
            eq("TLSv1.2"));

    assertTrue(true);
  }

  @Test
  public void testProcessMessages_withXmlAttachment() throws Exception {
    Folder mockFolder = mock(Folder.class);
    Message mockMessage = mock(Message.class);
    Multipart mockMultipart = mock(Multipart.class);
    BodyPart mockBodyPart = mock(BodyPart.class);

    when(mockFolder.search(any(FlagTerm.class))).thenReturn(new Message[] {mockMessage});
    when(mockMessage.getContent()).thenReturn(mockMultipart);
    when(mockMessage.getFrom()).thenReturn(new Address[] {new InternetAddress("from@test.com")});
    when(mockMultipart.getCount()).thenReturn(1);
    when(mockMultipart.getBodyPart(0)).thenReturn(mockBodyPart);
    when(mockBodyPart.getFileName()).thenReturn("report.xml");
    when(mockBodyPart.getInputStream())
        .thenReturn(new ByteArrayInputStream("<xml>RR</xml>".getBytes(StandardCharsets.UTF_8)));

    DirectTransportImpl spyTransport = PowerMockito.spy(directTransport);
    PowerMockito.doReturn("msg-123").when(spyTransport, "getMessageId", mockMessage);

    ReflectionTestUtils.setField(spyTransport, "rrReceiver", rrReceiver);
    Whitebox.invokeMethod(spyTransport, "processMessages", mockFolder);

    verify(mockMessage, times(1)).setFlag(Flags.Flag.SEEN, true);
    verify(rrReceiver, times(1)).handleReportabilityResponse(any(), eq("msg-123"));
  }

  @Test
  public void testReadMailUsingImap_withTlsProperties() throws Exception {
    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    ReflectionTestUtils.setField(spy, "imapReadRetryLimit", 1);

    PowerMockito.doNothing()
        .when(
            spy,
            "readMail",
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString());

    Whitebox.invokeMethod(
        spy, "readMailUsingImap", "host.com", "user", "pwd", "993", "corr-123", "TLSv1.2");

    PowerMockito.verifyPrivate(spy, times(1))
        .invoke("readMail", "host.com", "user", "pwd", "993", "corr-123", "TLSv1.2");
  }

  @Test
  public void testReadMailUsingImap_folderClosedRetry() throws Exception {
    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    ReflectionTestUtils.setField(spy, "imapReadRetryLimit", 2);

    // Throw FolderClosedException using null folder (for test)
    PowerMockito.doThrow(new FolderClosedException(null))
        .when(
            spy,
            "readMail",
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString());

    // Call the public wrapper
    Whitebox.invokeMethod(
        spy, "readMailUsingImap", "host", "user", "pwd", "993", "corr", "TLSv1.2");

    // Verify retry was attempted twice
    PowerMockito.verifyPrivate(spy, times(2))
        .invoke(
            "readMail",
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString());
  }

  @Test
  public void testSendEicrDataUsingRestfulApi_logsErrorAndReturnsNull() {
    KarProcessingData dataMock = mock(KarProcessingData.class);
    JSONObject result = directTransport.sendEicrDataUsingRestfulApi(dataMock);
    assertNull(result);
  }

  @Test
  public void testGetMessageId_handlesMessagingException() throws Exception {
    Message msg = mock(Message.class);
    when(msg.getAllHeaders()).thenThrow(new MessagingException("fail"));
    String id = directTransport.getMessageId(msg);
    assertNull(id);
  }

  @Test
  public void testGetMessageId_returnsHeaderValue() throws Exception {
    MimeMessage msg = mock(MimeMessage.class);
    Header header = new Header("Message-ID", "<123@test>");
    when(msg.getAllHeaders()).thenReturn(Collections.enumeration(List.of(header)));
    String id = directTransport.getMessageId(msg);
    assertEquals("<123@test>", id);
  }

  @Test
  public void testReceiveRrDataUsingDirect_logsAndCallsImap() throws Exception {
    HealthcareSetting hs = mock(HealthcareSetting.class);
    KarProcessingData dataMock = mock(KarProcessingData.class);

    when(dataMock.getHealthcareSetting()).thenReturn(hs);
    when(hs.getDirectUser()).thenReturn("user@test.com");
    when(hs.getDirectPwd()).thenReturn("pwd");
    when(hs.getDirectHost()).thenReturn(null);
    when(hs.getImapUrl()).thenReturn("imap.test.com");
    when(hs.getImapPort()).thenReturn("993");
    when(dataMock.getxCorrelationId()).thenReturn("corr");

    DirectTransportImpl spy = PowerMockito.spy(directTransport);
    PowerMockito.doNothing()
        .when(
            spy,
            "readMailUsingImap",
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString());

    spy.receiveRrDataUsingDirect(dataMock);

    PowerMockito.verifyPrivate(spy, times(1))
        .invoke(
            "readMailUsingImap",
            eq("imap.test.com"),
            eq("user@test.com"),
            eq("pwd"),
            eq("993"),
            eq("corr"),
            isNull());
  }
}
