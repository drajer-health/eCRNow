package com.drajer.routing.impl;

import com.drajer.routing.EicrSender;
import com.drajer.sof.model.LaunchDetails;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Properties;
import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.mail.BodyPart;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.util.ByteArrayDataSource;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class DirectEicrSender extends EicrSender {

  private static final Logger logger = LoggerFactory.getLogger(DirectEicrSender.class);

  private static final String FILE_NAME = "eICR Report";

  public class DirectMimeMessage extends MimeMessage {

    Session session1;
    String messageId;
    String domain;

    public DirectMimeMessage(Session sess, String id, String domainName) {
      super(sess);
      session1 = sess;
      messageId = id;
      domain = domainName;
    }

    @Override
    protected void updateMessageID() throws MessagingException {
      setHeader("Message-ID", "<" + messageId + "@" + domain + ">");
    }
  }

  @Override
  public void sendData(Object context, String data, String correlationId) {

    logger.info(" **** START Executing Direct SEND **** ");

    if (context instanceof LaunchDetails) {

      logger.info(" Obtained Launch Details ");
      LaunchDetails details = (LaunchDetails) context;

      InputStream is = IOUtils.toInputStream(data, StandardCharsets.UTF_8);

      try {

        logger.info(
            " Sending Mail from {} to {}",
            StringEscapeUtils.escapeJava(details.getDirectUser()),
            StringEscapeUtils.escapeJava(details.getDirectRecipient()));
        if (details.getSmtpUrl() != null) {
          logger.info(
              "Using SMTP URL to send:::::{}", StringEscapeUtils.escapeJava(details.getSmtpUrl()));
          sendMail(
              details.getSmtpUrl(),
              details.getDirectUser(),
              details.getDirectPwd(),
              details.getSmtpPort(),
              details.getDirectRecipient(),
              is,
              DirectEicrSender.FILE_NAME,
              correlationId);
        } else {
          logger.info(
              "Using Direct Host to send:::::{}",
              StringEscapeUtils.escapeJava(details.getDirectHost()));
          sendMail(
              details.getDirectHost(),
              details.getDirectUser(),
              details.getDirectPwd(),
              details.getSmtpPort(),
              details.getDirectRecipient(),
              is,
              DirectEicrSender.FILE_NAME,
              correlationId);
        }

      } catch (Exception e) {

        String msg = "Unable to send Direct Message";
        logger.error(msg, e);

        throw new RuntimeException(msg);
      }
    }
  }

  public void sendMail(
      String host,
      String username,
      String password,
      String port,
      String receipientAddr,
      InputStream is,
      String filename,
      String correlationId)
      throws Exception {

    Properties props = new Properties();

    props.put("mail.smtp.auth", "true");

    props.setProperty("mail.smtp.ssl.trust", "*");
    props.setProperty("mail.smtp.ssl.enable", "true");

    Session session = Session.getInstance(props, null);

    logger.info(" Retrieve Session instance for sending Direct mail ");

    DirectMimeMessage message = new DirectMimeMessage(session, correlationId, host);

    logger.info("Setting From Address {}", StringEscapeUtils.escapeJava(username));
    message.setFrom(new InternetAddress(username));

    String toAddr = StringUtils.deleteWhitespace(receipientAddr);
    logger.info("Setting recipients {}, length : {}", toAddr, toAddr.length());
    message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(toAddr));
    logger.info("Finished setting recipients {}", toAddr);

    message.setSubject("eICR Report ");
    message.setText(FILE_NAME);

    logger.info("Creating Message Body Part ");
    BodyPart messageBodyPart = new MimeBodyPart();
    Multipart multipart = new MimeMultipart();
    DataSource source = new ByteArrayDataSource(is, "application/xml; charset=UTF-8");
    messageBodyPart.setDataHandler(new DataHandler(source));

    messageBodyPart.setFileName(filename + "_eICRReport.xml");
    multipart.addBodyPart(messageBodyPart);

    // Send the complete message parts
    message.setContent(multipart);

    logger.info(" Completed constructing the Message ");
    Transport transport = session.getTransport("smtp");
    transport.connect(host, Integer.parseInt(port), username, password);

    logger.info(" Connection successful to the direct host {}", StringEscapeUtils.escapeJava(host));
    transport.sendMessage(message, message.getAllRecipients());

    logger.info("Finished sending Direct message successfully, closing connection ");
    transport.close();

    logger.info(" Finished sending Direct Message ");
  }
}
