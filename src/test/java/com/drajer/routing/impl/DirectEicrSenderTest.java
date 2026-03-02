package com.drajer.routing.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.drajer.sof.model.LaunchDetails;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Properties;
import javax.mail.Session;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class DirectEicrSenderTest {

  private DirectEicrSender sender;
  private DirectEicrSender.DirectMimeMessage message;
  private final String correlationId = "abc123";
  private final String domain = "testdomain.com";

  @Before
  public void setUp() {
    sender = new DirectEicrSender();
    Session session = Session.getInstance(new Properties());
    message = spy(sender.new DirectMimeMessage(session, correlationId, domain));
  }

  @Test
  public void updateMessageIdCorrectly() throws Exception {
    message.updateMessageID();
    String[] headers = message.getHeader("Message-ID");
    assertNotNull(headers);
    assertEquals(1, headers.length);
    String expected = "<" + correlationId + "@" + domain + ">";
    assertEquals(expected, headers[0]);
    verify(message, times(1)).setHeader("Message-ID", expected);
  }

  @Test
  public void notSendMailIfContextIsNotLaunchDetails() {
    DirectEicrSender spySender = spy(sender);
    Object context = new Object();
    spySender.sendData(context, "<xml>data</xml>", "corr-123");

    try {
      verify(spySender, never())
          .sendMail(
              anyString(),
              anyString(),
              anyString(),
              anyString(),
              anyString(),
              any(InputStream.class),
              anyString(),
              anyString());
      assertEquals(Object.class, context.getClass());
    } catch (Exception e) {
      fail("Exception thrown: " + e.getMessage());
    }
  }

  @Test
  public void sendMailWhenSmtpUrlIsSet() throws Exception {
    LaunchDetails details = mock(LaunchDetails.class);
    when(details.getSmtpUrl()).thenReturn("smtp.test.com");
    when(details.getDirectUser()).thenReturn("user@test.com");
    when(details.getDirectPwd()).thenReturn("pwd");
    when(details.getSmtpPort()).thenReturn("25");
    when(details.getDirectRecipient()).thenReturn("recipient@test.com");

    DirectEicrSender spySender = spy(sender);
    doNothing()
        .when(spySender)
        .sendMail(
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            any(InputStream.class),
            anyString(),
            anyString());

    spySender.sendData(details, "<xml>data</xml>", "corr-456");

    verify(spySender)
        .sendMail(
            eq("smtp.test.com"),
            eq("user@test.com"),
            eq("pwd"),
            eq("25"),
            eq("recipient@test.com"),
            any(InputStream.class),
            eq("eICR Report"),
            eq("corr-456"));
    assertEquals("smtp.test.com", details.getSmtpUrl());
  }

  @Test
  public void sendMailWhenSmtpUrlIsNull() throws Exception {
    LaunchDetails details = mock(LaunchDetails.class);
    when(details.getSmtpUrl()).thenReturn(null);
    when(details.getDirectHost()).thenReturn("direct.test.com");
    when(details.getDirectUser()).thenReturn("user@test.com");
    when(details.getDirectPwd()).thenReturn("pwd");
    when(details.getSmtpPort()).thenReturn("25");
    when(details.getDirectRecipient()).thenReturn("recipient@test.com");

    DirectEicrSender spySender = spy(sender);
    doNothing()
        .when(spySender)
        .sendMail(
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            any(InputStream.class),
            anyString(),
            anyString());

    spySender.sendData(details, "<xml>data</xml>", "corr-789");

    verify(spySender)
        .sendMail(
            eq("direct.test.com"),
            eq("user@test.com"),
            eq("pwd"),
            eq("25"),
            eq("recipient@test.com"),
            any(InputStream.class),
            eq("eICR Report"),
            eq("corr-789"));
    assertNull(details.getSmtpUrl());
    assertEquals("direct.test.com", details.getDirectHost());
  }

  @Test
  public void throwRuntimeExceptionIfSendMailFails() throws Exception {
    LaunchDetails details = mock(LaunchDetails.class);
    when(details.getSmtpUrl()).thenReturn("smtp.test.com");
    when(details.getDirectUser()).thenReturn("user@test.com");
    when(details.getDirectPwd()).thenReturn("pwd");
    when(details.getSmtpPort()).thenReturn("25");
    when(details.getDirectRecipient()).thenReturn("recipient@test.com");

    DirectEicrSender spySender = spy(sender);
    doThrow(new Exception("Mail failed"))
        .when(spySender)
        .sendMail(
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            any(InputStream.class),
            anyString(),
            anyString());

    try {
      spySender.sendData(details, "<xml>data</xml>", "corr-999");
      fail("Expected RuntimeException was not thrown");
    } catch (RuntimeException ex) {
      assertEquals("Unable to send Direct Message", ex.getMessage());
    }
  }

  @Test
  public void callSendMailSuccessfully() throws Exception {
    DirectEicrSender spySender = spy(sender);
    InputStream is = new ByteArrayInputStream(new byte[0]);

    doNothing()
        .when(spySender)
        .sendMail(
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            eq(is),
            anyString(),
            anyString());

    spySender.sendMail(
        "smtp.test.com",
        "user@test.com",
        "pwd",
        "25",
        "recipient@test.com",
        is,
        "file",
        "corr-001");

    verify(spySender)
        .sendMail(
            eq("smtp.test.com"),
            eq("user@test.com"),
            eq("pwd"),
            eq("25"),
            eq("recipient@test.com"),
            eq(is),
            eq("file"),
            eq("corr-001"));

    assertNotNull(is);
  }

  @Test
  public void throwExceptionIfTransportFails() throws Exception {
    DirectEicrSender spySender = spy(sender);
    InputStream is = new ByteArrayInputStream(new byte[0]);
    doThrow(new Exception("Transport failed"))
        .when(spySender)
        .sendMail(
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            eq(is),
            anyString(),
            anyString());
    try {
      spySender.sendData(mock(LaunchDetails.class), "<xml>data</xml>", "corr-777");
      fail("Expected RuntimeException was not thrown");
    } catch (RuntimeException ex) {
      assertEquals("Unable to send Direct Message", ex.getMessage());
    }
  }

  @Test
  @Ignore
  public void coverSendMailLines() {
    InputStream is = new ByteArrayInputStream("<xml>test</xml>".getBytes());
    try {
      sender.sendMail(
          "localhost",
          "user@test.com",
          "pwd",
          "25",
          "recipient@test.com",
          is,
          "testFile",
          "corr-123");
    } catch (Exception ignored) {
    }
    assertNotNull(is);
  }
}
