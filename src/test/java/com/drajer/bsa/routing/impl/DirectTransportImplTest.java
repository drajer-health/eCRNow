package com.drajer.bsa.routing.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.service.RrReceiver;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;
import javax.mail.*;
import javax.mail.internet.InternetAddress;
import javax.mail.search.FlagTerm;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
@PrepareForTest({Session.class, Transport.class, Store.class})
public class DirectTransportImplTest {

  @InjectMocks private DirectTransportImpl directTransport;

  @Mock private RrReceiver rrReceiver;
  @Mock private Session mailSession;
  @Mock private Transport mailTransport;
  @Mock private Store mailStore;
  @Mock private Folder inboxFolder;
  @Mock private Message emailMessage;

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

    // Mock static Session - let production code run
    PowerMockito.mockStatic(Session.class);
    when(Session.getInstance(any(Properties.class), isNull())).thenReturn(mailSession);
    when(mailSession.getTransport("smtp")).thenReturn(mailTransport);
    when(mailSession.getStore("imap")).thenReturn(mailStore);
    when(mailStore.getFolder("Inbox")).thenReturn(inboxFolder);
    when(mailSession.getProperties()).thenReturn(new Properties());

    // Set Spring value fields
    ReflectionTestUtils.setField(directTransport, "logDirectory", "test-logs");
    ReflectionTestUtils.setField(directTransport, "imapReadRetryLimit", 1);
    ReflectionTestUtils.setField(directTransport, "smtpAuth", "true");
    ReflectionTestUtils.setField(directTransport, "smtpSslEnable", "true");
    ReflectionTestUtils.setField(directTransport, "smtpStartTlsEnable", "false");
    ReflectionTestUtils.setField(directTransport, "smtpSslTrust", "*");
    ReflectionTestUtils.setField(directTransport, "smtpSslProtocols", "");
    ReflectionTestUtils.setField(directTransport, "imapAuth", "true");
    ReflectionTestUtils.setField(directTransport, "imapSslEnable", "true");
    ReflectionTestUtils.setField(directTransport, "imapSslTrust", "*");
    ReflectionTestUtils.setField(directTransport, "imapConnectionPoolSize", "1");
    ReflectionTestUtils.setField(directTransport, "imapSslProtocols", "");
    ReflectionTestUtils.setField(directTransport, "imapBatchSize", 10);
  }

  // ========== TEST 1: Send EICR with SMTP URL ==========
  @Test
  public void test01_SendEicrDataUsingDirect_WithSmtpUrl() throws Exception {
    // Setup dependencies - let sendMail() execute
    doNothing()
        .when(mailTransport)
        .connect(anyString(), anyInt(), anyString(), nullable(String.class));
    doNothing().when(mailTransport).sendMessage(any(Message.class), any(Address[].class));
    doNothing().when(mailTransport).close();

    directTransport.sendEicrDataUsingDirect(karProcessingData);

    // Verify production code executed
    verify(mailSession, times(1)).getTransport("smtp");
    verify(mailTransport, times(1))
        .connect(anyString(), anyInt(), anyString(), nullable(String.class));
    verify(mailTransport, times(1)).sendMessage(any(Message.class), any(Address[].class));
    verify(mailTransport, times(1)).close();
  }

  // ========== TEST 2: Send EICR with Direct Host ==========
  @Test
  public void test02_SendEicrDataUsingDirect_WithDirectHost() throws Exception {
    healthcareSetting.setSmtpUrl(null);

    doNothing()
        .when(mailTransport)
        .connect(anyString(), anyInt(), anyString(), nullable(String.class));
    doNothing().when(mailTransport).sendMessage(any(Message.class), any(Address[].class));
    doNothing().when(mailTransport).close();

    directTransport.sendEicrDataUsingDirect(karProcessingData);

    verify(mailTransport, times(1))
        .connect(anyString(), anyInt(), anyString(), nullable(String.class));
    verify(mailTransport, times(1)).sendMessage(any(Message.class), any(Address[].class));
  }

  // ========== TEST 3: Send EICR with no host configured ==========
  @Test(expected = IllegalStateException.class)
  public void test03_SendEicrDataUsingDirect_NoHostConfigured() throws Exception {
    healthcareSetting.setSmtpUrl(null);
    healthcareSetting.setDirectHost(null);

    directTransport.sendEicrDataUsingDirect(karProcessingData);
  }

  // ========== TEST 4: Receive RR with IMAP URL ==========
  @Test
  public void test04_ReceiveRrDataUsingDirect_WithImapUrl() throws Exception {
    when(inboxFolder.getMessageCount()).thenReturn(0);
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();

    directTransport.receiveRrDataUsingDirect(karProcessingData);

    verify(mailStore, times(1)).getFolder("Inbox");
    verify(inboxFolder, times(1)).open(Folder.READ_WRITE);
    verify(inboxFolder, times(1)).close(true);
  }

  // ========== TEST 5: Receive RR with Direct Host ==========
  @Test
  public void test05_ReceiveRrDataUsingDirect_WithDirectHost() throws Exception {
    healthcareSetting.setImapUrl(null);

    when(inboxFolder.getMessageCount()).thenReturn(0);
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();

    directTransport.receiveRrDataUsingDirect(karProcessingData);

    verify(inboxFolder, times(1)).open(Folder.READ_WRITE);
  }

  // ========== TEST 6: Receive RR with no host configured ==========
  @Test
  public void test06_ReceiveRrDataUsingDirect_NoHostConfigured() throws Exception {
    healthcareSetting.setImapUrl(null);
    healthcareSetting.setDirectHost(null);

    directTransport.receiveRrDataUsingDirect(karProcessingData);

    verify(mailStore, never()).connect(anyString(), anyInt(), anyString(), anyString());
  }

  // ========== TEST 7: Get Message ID from headers ==========
  @Test
  public void test07_GetMessageId_FoundInHeaders() throws Exception {
    Header header = new Header("Message-ID", "<test-123@example.com>");

    when(emailMessage.getAllHeaders()).thenReturn(Collections.enumeration(List.of(header)));

    String messageId = directTransport.getMessageId(emailMessage);

    assertEquals("Should extract message ID", "<test-123@example.com>", messageId);
  }

  // ========== TEST 8: Get Message ID not found ==========
  @Test
  public void test08_GetMessageId_NotFound() throws Exception {
    Header header = new Header("Subject", "Test Subject");

    when(emailMessage.getAllHeaders()).thenReturn(Collections.enumeration(List.of(header)));

    String messageId = directTransport.getMessageId(emailMessage);

    assertNull("Should return null when Message-ID not found", messageId);
  }

  // ========== TEST 9: Get Message ID with exception ==========
  @Test
  public void test09_GetMessageId_MessagingException() throws Exception {
    when(emailMessage.getAllHeaders()).thenThrow(new MessagingException("Test error"));

    String messageId = directTransport.getMessageId(emailMessage);

    assertNull("Should return null on exception", messageId);
  }

  // ========== TEST 10: Send EICR using RESTful API not supported ==========
  @Test
  public void test10_SendEicrDataUsingRestfulApi_ReturnsNull() {
    JSONObject result = directTransport.sendEicrDataUsingRestfulApi(karProcessingData);

    assertNull("RESTful API not supported", result);
  }

  // ========== TEST 11: DirectMimeMessage constructor ==========
  @Test
  public void test11_DirectMimeMessageConstructor() throws Exception {
    Session testSession = Session.getInstance(new Properties());
    DirectTransportImpl outer = new DirectTransportImpl(rrReceiver);

    DirectTransportImpl.DirectMimeMessage message =
        outer.new DirectMimeMessage(testSession, "corr-123", "example.com");

    assertNotNull("Should create DirectMimeMessage", message);
    assertEquals("Session should be set", testSession, message.sessions);
    assertEquals("Message ID should be set", "corr-123", message.messageId);
    assertEquals("Domain should be set", "example.com", message.domain);
  }

  // ========== TEST 12: DirectMimeMessage updateMessageID ==========
  @Test
  public void test12_DirectMimeMessage_UpdateMessageID() throws Exception {
    Session session = Session.getInstance(new Properties());
    DirectTransportImpl outer = new DirectTransportImpl(rrReceiver);

    DirectTransportImpl.DirectMimeMessage message =
        outer.new DirectMimeMessage(session, "corr-456", "test.com");
    message.updateMessageID();

    String[] headers = message.getHeader("Message-ID");
    assertNotNull("Header should be set", headers);
    assertEquals("Should have one header", 1, headers.length);
    assertEquals("Header should match format", "<corr-456@test.com>", headers[0]);
  }

  // ========== TEST 13: Send connection timeout error ==========
  @Test(expected = IllegalStateException.class)
  public void test13_SendEicrDataUsingDirect_ConnectionError() throws Exception {
    doThrow(new MessagingException("Connection timeout"))
        .when(mailTransport)
        .connect(anyString(), anyInt(), anyString(), nullable(String.class));

    directTransport.sendEicrDataUsingDirect(karProcessingData);
  }

  // ========== TEST 14: Read mail with no messages ==========
  @Test
  public void test14_ReceiveRrDataUsingDirect_NoMessages() throws Exception {
    when(inboxFolder.getMessageCount()).thenReturn(0);
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();

    directTransport.receiveRrDataUsingDirect(karProcessingData);

    verify(inboxFolder, times(1)).getMessageCount();
  }

  // ========== TEST 15: Message processing with multiple unread messages ==========
  @Test
  public void test15_ReceiveRrDataUsingDirect_MultipleMessages() throws Exception {
    when(inboxFolder.getMessageCount()).thenReturn(2);
    when(inboxFolder.getUnreadMessageCount()).thenReturn(2);
    when(inboxFolder.getMessages(1, 2)).thenReturn(new Message[] {emailMessage, emailMessage});
    when(inboxFolder.search(any(FlagTerm.class), any(Message[].class)))
        .thenReturn(new Message[] {emailMessage, emailMessage});
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();

    directTransport.receiveRrDataUsingDirect(karProcessingData);

    verify(inboxFolder, times(1)).getMessageCount();
    verify(inboxFolder, times(1)).getUnreadMessageCount();
  }

  // ========== TEST 16: Process single message with XML attachment ==========
  @Test
  public void test16_ProcessMessage_WithXmlAttachment() throws Exception {
    Multipart multipart = mock(Multipart.class);
    BodyPart bodyPart = mock(BodyPart.class);

    when(emailMessage.getContent()).thenReturn(multipart);
    when(emailMessage.getFrom()).thenReturn(new Address[] {new InternetAddress("sender@test.com")});
    when(multipart.getCount()).thenReturn(1);
    when(multipart.getBodyPart(0)).thenReturn(bodyPart);
    when(bodyPart.getFileName()).thenReturn("report.xml");
    when(bodyPart.getInputStream())
        .thenReturn(
            new ByteArrayInputStream("<xml>content</xml>".getBytes(StandardCharsets.UTF_8)));
    when(emailMessage.getAllHeaders())
        .thenReturn(Collections.enumeration(List.of(new Header("Message-ID", "<msg-1>"))));

    when(inboxFolder.getMessageCount()).thenReturn(1);
    when(inboxFolder.getUnreadMessageCount()).thenReturn(1);
    when(inboxFolder.getMessages(1, 1)).thenReturn(new Message[] {emailMessage});
    when(inboxFolder.search(any(FlagTerm.class), any(Message[].class)))
        .thenReturn(new Message[] {emailMessage});
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();
    doNothing().when(emailMessage).setFlag(Flags.Flag.SEEN, true);

    // Execute and verify method doesn't return null
    directTransport.receiveRrDataUsingDirect(karProcessingData);

    // Assertions
    assertNotNull("Data should not be null", karProcessingData);
    assertNotNull(
        "Healthcare setting should not be null", karProcessingData.getHealthcareSetting());
    assertNotNull("XML attachment should be processed", emailMessage);
    assertTrue("Message count should be 1", inboxFolder.getMessageCount() == 1);
    assertTrue("Unread count should be 1", inboxFolder.getUnreadMessageCount() == 1);

    verify(rrReceiver, times(1)).handleReportabilityResponse(any(), eq("<msg-1>"));
    verify(emailMessage, times(1)).setFlag(Flags.Flag.SEEN, true);
    verify(inboxFolder, times(1)).close(true);
    verify(mailStore, times(1)).close();
  }

  // ========== TEST 17: Process single message with non-XML attachment ==========
  @Test
  public void test17_ProcessMessage_WithNonXmlAttachment() throws Exception {
    Multipart multipart = mock(Multipart.class);
    BodyPart bodyPart = mock(BodyPart.class);

    when(emailMessage.getContent()).thenReturn(multipart);
    when(emailMessage.getFrom()).thenReturn(new Address[] {new InternetAddress("sender@test.com")});
    when(multipart.getCount()).thenReturn(1);
    when(multipart.getBodyPart(0)).thenReturn(bodyPart);
    when(bodyPart.getFileName()).thenReturn("document.pdf");
    when(emailMessage.getAllHeaders())
        .thenReturn(Collections.enumeration(List.of(new Header("Message-ID", "<msg-2>"))));

    when(inboxFolder.getMessageCount()).thenReturn(1);
    when(inboxFolder.getUnreadMessageCount()).thenReturn(1);
    when(inboxFolder.getMessages(1, 1)).thenReturn(new Message[] {emailMessage});
    when(inboxFolder.search(any(FlagTerm.class), any(Message[].class)))
        .thenReturn(new Message[] {emailMessage});
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();

    directTransport.receiveRrDataUsingDirect(karProcessingData);

    // Assertions - non-XML attachments should NOT be processed
    assertNotNull("Message should not be null", emailMessage);
    assertNotNull("Data should not be null", karProcessingData);
    assertFalse("PDF should not contain .xml", "document.pdf".toLowerCase().contains(".xml"));
    assertTrue("Content should be multipart", emailMessage.getContent() instanceof Multipart);

    verify(rrReceiver, never()).handleReportabilityResponse(any(), anyString());
    verify(emailMessage, never()).setFlag(Flags.Flag.SEEN, true);
    verify(inboxFolder, times(1)).close(true);
    verify(mailStore, times(1)).close();
  }

  // ========== TEST 18: Process non-multipart message ==========
  @Test
  public void test18_ProcessMessage_NonMultipartMessage() throws Exception {
    when(emailMessage.getContent()).thenReturn("plain text content");
    when(emailMessage.getFrom()).thenReturn(new Address[] {new InternetAddress("sender@test.com")});
    when(emailMessage.getAllHeaders())
        .thenReturn(Collections.enumeration(List.of(new Header("Message-ID", "<msg-3>"))));

    when(inboxFolder.getMessageCount()).thenReturn(1);
    when(inboxFolder.getUnreadMessageCount()).thenReturn(1);
    when(inboxFolder.getMessages(1, 1)).thenReturn(new Message[] {emailMessage});
    when(inboxFolder.search(any(FlagTerm.class), any(Message[].class)))
        .thenReturn(new Message[] {emailMessage});
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();

    directTransport.receiveRrDataUsingDirect(karProcessingData);

    // Assertions - non-multipart messages should be logged but not processed
    assertNotNull("Message should not be null", emailMessage);
    assertNotNull("Data should not be null", karProcessingData);
    assertTrue("Content should be plain text String", emailMessage.getContent() instanceof String);
    assertEquals("Content should match", "plain text content", emailMessage.getContent());
    assertFalse("Content should NOT be Multipart", emailMessage.getContent() instanceof Multipart);

    verify(rrReceiver, never()).handleReportabilityResponse(any(), anyString());
    verify(emailMessage, never()).setFlag(Flags.Flag.SEEN, true);
    verify(inboxFolder, times(1)).close(true);
  }

  // ========== TEST 19: Delete read messages ==========
  @Test
  public void test19_DeleteReadMessages_WithReadMessages() throws Exception {
    Message readMessage1 = mock(Message.class);
    Message readMessage2 = mock(Message.class);

    when(inboxFolder.getMessageCount()).thenReturn(2);
    when(inboxFolder.getUnreadMessageCount()).thenReturn(0);
    when(inboxFolder.isOpen()).thenReturn(true);
    when(inboxFolder.search(any(FlagTerm.class)))
        .thenReturn(new Message[] {readMessage1, readMessage2});
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();
    when(readMessage1.getAllHeaders())
        .thenReturn(Collections.enumeration(List.of(new Header("Message-ID", "<read-1>"))));
    when(readMessage2.getAllHeaders())
        .thenReturn(Collections.enumeration(List.of(new Header("Message-ID", "<read-2>"))));
    doNothing().when(readMessage1).setFlag(Flags.Flag.DELETED, true);
    doNothing().when(readMessage2).setFlag(Flags.Flag.DELETED, true);

    directTransport.receiveRrDataUsingDirect(karProcessingData);

    // Assertions - both read messages should be marked for deletion
    assertNotNull("Message 1 should not be null", readMessage1);
    assertNotNull("Message 2 should not be null", readMessage2);
    assertNotNull("Data should not be null", karProcessingData);
    assertTrue("Message count should be 2", inboxFolder.getMessageCount() == 2);
    assertTrue("Unread count should be 0", inboxFolder.getUnreadMessageCount() == 0);
    assertTrue("Inbox should be open", inboxFolder.isOpen());

    verify(inboxFolder, times(1)).search(any(FlagTerm.class));
    verify(readMessage1, times(1)).setFlag(Flags.Flag.DELETED, true);
    verify(readMessage2, times(1)).setFlag(Flags.Flag.DELETED, true);
    verify(inboxFolder, times(1)).close(true);
    verify(mailStore, times(1)).close();
  }

  // ========== TEST 20: Message with null sender address ==========
  @Test
  public void test20_ProcessMessage_NullSenderAddress() throws Exception {
    Multipart multipart = mock(Multipart.class);
    BodyPart bodyPart = mock(BodyPart.class);

    when(emailMessage.getContent()).thenReturn(multipart);
    when(emailMessage.getFrom()).thenReturn(null);
    when(multipart.getCount()).thenReturn(1);
    when(multipart.getBodyPart(0)).thenReturn(bodyPart);
    when(bodyPart.getFileName()).thenReturn("report.xml");
    when(bodyPart.getInputStream())
        .thenReturn(
            new ByteArrayInputStream("<xml>content</xml>".getBytes(StandardCharsets.UTF_8)));
    when(emailMessage.getAllHeaders())
        .thenReturn(Collections.enumeration(List.of(new Header("Message-ID", "<msg-4>"))));

    when(inboxFolder.getMessageCount()).thenReturn(1);
    when(inboxFolder.getUnreadMessageCount()).thenReturn(1);
    when(inboxFolder.getMessages(1, 1)).thenReturn(new Message[] {emailMessage});
    when(inboxFolder.search(any(FlagTerm.class), any(Message[].class)))
        .thenReturn(new Message[] {emailMessage});
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();
    doNothing().when(emailMessage).setFlag(Flags.Flag.SEEN, true);

    directTransport.receiveRrDataUsingDirect(karProcessingData);

    // Assertions - XML should still be processed even with null sender
    assertNull("Sender address should be null", emailMessage.getFrom());
    assertNotNull("Message should not be null", emailMessage);
    assertNotNull("Data should not be null", karProcessingData);
    assertTrue("Content should be multipart", emailMessage.getContent() instanceof Multipart);
    assertEquals("Message count should be 1", 1, inboxFolder.getMessageCount());
    assertEquals("Unread count should be 1", 1, inboxFolder.getUnreadMessageCount());

    verify(rrReceiver, times(1)).handleReportabilityResponse(any(), eq("<msg-4>"));
    verify(emailMessage, times(1)).setFlag(Flags.Flag.SEEN, true);
    verify(inboxFolder, times(1)).close(true);
    verify(mailStore, times(1)).close();
  }

  // ========== TEST 21: Process message with error - continues processing ==========
  @Test
  public void test21_ProcessMessage_WithErrorContinuesProcessing() throws Exception {
    Multipart multipart = mock(Multipart.class);
    BodyPart bodyPart = mock(BodyPart.class);

    when(emailMessage.getContent()).thenReturn(multipart);
    when(emailMessage.getFrom()).thenReturn(new Address[] {new InternetAddress("sender@test.com")});
    when(multipart.getCount()).thenReturn(1);
    // Throw exception when processing body part
    when(multipart.getBodyPart(0)).thenThrow(new MessagingException("Error reading attachment"));
    when(emailMessage.getAllHeaders())
        .thenReturn(Collections.enumeration(List.of(new Header("Message-ID", "<msg-5>"))));

    when(inboxFolder.getMessageCount()).thenReturn(1);
    when(inboxFolder.getUnreadMessageCount()).thenReturn(1);
    when(inboxFolder.getMessages(1, 1)).thenReturn(new Message[] {emailMessage});
    when(inboxFolder.search(any(FlagTerm.class), any(Message[].class)))
        .thenReturn(new Message[] {emailMessage});
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();

    // Method should complete successfully despite error
    directTransport.receiveRrDataUsingDirect(karProcessingData);

    // Assertions - method should complete and close folder
    assertNotNull("Message should not be null", emailMessage);
    assertNotNull("Data should not be null", karProcessingData);
    assertTrue("Message count should be 1", inboxFolder.getMessageCount() == 1);
    assertTrue("Content should be multipart", emailMessage.getContent() instanceof Multipart);

    verify(inboxFolder, times(1)).close(true);
    verify(mailStore, times(1)).close();
    // Handler should not be called due to error
    verify(rrReceiver, never()).handleReportabilityResponse(any(), anyString());
  }

  // ========== TEST 22: Process message with generic exception ==========
  @Test
  public void test22_ProcessMessage_GenericException() throws Exception {
    Multipart multipart = mock(Multipart.class);
    BodyPart bodyPart = mock(BodyPart.class);

    when(emailMessage.getContent()).thenReturn(multipart);
    when(emailMessage.getFrom()).thenReturn(new Address[] {new InternetAddress("sender@test.com")});
    when(multipart.getCount()).thenReturn(1);
    when(multipart.getBodyPart(0)).thenThrow(new RuntimeException("Processing error"));
    when(emailMessage.getAllHeaders())
        .thenReturn(Collections.enumeration(List.of(new Header("Message-ID", "<msg-6>"))));

    when(inboxFolder.getMessageCount()).thenReturn(1);
    when(inboxFolder.getUnreadMessageCount()).thenReturn(1);
    when(inboxFolder.getMessages(1, 1)).thenReturn(new Message[] {emailMessage});
    when(inboxFolder.search(any(FlagTerm.class), any(Message[].class)))
        .thenReturn(new Message[] {emailMessage});
    doNothing().when(inboxFolder).open(Folder.READ_WRITE);
    doNothing().when(inboxFolder).close(true);
    doNothing().when(mailStore).close();

    // Generic exception should be caught and logged, not rethrown
    directTransport.receiveRrDataUsingDirect(karProcessingData);

    // Assertions - generic exception should NOT be rethrown
    assertNotNull("Message should not be null", emailMessage);
    assertNotNull("Data should not be null", karProcessingData);
    assertTrue("Message count should be 1", inboxFolder.getMessageCount() == 1);
    assertTrue("Unread count should be 1", inboxFolder.getUnreadMessageCount() == 1);

    // Verify folder operations completed despite exception
    verify(inboxFolder, times(1)).close(true);
    verify(mailStore, times(1)).close();
    // RR handler should not be called due to exception
    verify(rrReceiver, never()).handleReportabilityResponse(any(), anyString());
  }
}
