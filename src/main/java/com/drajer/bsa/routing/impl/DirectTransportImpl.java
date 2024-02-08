package com.drajer.bsa.routing.impl;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.routing.DataTransportInterface;
import com.drajer.bsa.service.RrReceiver;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.ecrapp.model.EicrTypes;
import com.drajer.ecrapp.model.ReportabilityResponse;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;
import java.util.Properties;
import java.util.UUID;
import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.mail.Address;
import javax.mail.BodyPart;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Header;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.search.FlagTerm;
import javax.mail.util.ByteArrayDataSource;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/*
 * <h1>DataTransportImpl</h1>
 *
 * The class implements the DataTransportInterface methods for Direct Send and Receive methods.
 */
@Service
public class DirectTransportImpl implements DataTransportInterface {

  private static final Logger logger = LoggerFactory.getLogger(DirectTransportImpl.class);

  private static final String CDA_FILE_NAME = "eICR_Report";
  private static final String IMAP = "imap";
  private static final String INBOX = "Inbox";

  @Autowired RrReceiver rrReceiver;

  @Value("${bsa.output.directory:bsa-output}")
  String logDirectory;

  public class DirectMimeMessage extends MimeMessage {

    Session sessions;
    String messageId;
    String domain;

    public DirectMimeMessage(Session ses, String id, String domainName) {
      super(ses);
      sessions = ses;
      messageId = id;
      domain = domainName;
    }

    @Override
    protected void updateMessageID() throws MessagingException {
      setHeader("Message-ID", "<" + messageId + "@" + domain + ">");
    }
  }

  /**
   * The method is used to send data to a PHA/TTP using Direct Transport. As a direct client, the
   * implementer has to connect to a HISP and then send the message via Direct.
   *
   * @param data - The KarProcessingData that contains the necessary data to process and send the
   *     information.
   */
  @Override
  public void sendEicrDataUsingDirect(KarProcessingData data) {

    logger.info(" **** START Executing Direct Transmission **** ");

    String payload = data.getSubmittedCdaData();

    InputStream is = IOUtils.toInputStream(payload, StandardCharsets.UTF_8);

    HealthcareSetting hs = data.getHealthcareSetting();

    try {

      logger.info(
          " Sending Mail from {} to {}", hs.getDirectUser(), hs.getDirectRecipientAddress());

      if (!StringUtils.isEmpty(hs.getSmtpUrl())) {

        logger.info("Using SMTP URL {} to send the data", hs.getSmtpUrl());

        sendMail(
            hs.getSmtpUrl(),
            hs.getDirectUser(),
            hs.getDirectPwd(),
            hs.getSmtpPort(),
            hs.getDirectRecipientAddress(),
            is,
            CDA_FILE_NAME,
            data.getxCorrelationId(),
            hs.getDirectTlsVersion());

        logger.info(" Finished sending the message using Direct ");

      } else if (!StringUtils.isEmpty(hs.getDirectHost())) {

        logger.info("Using Direct Host to send:::::{}", hs.getDirectHost());

        sendMail(
            hs.getDirectHost(),
            hs.getDirectUser(),
            hs.getDirectPwd(),
            hs.getSmtpPort(),
            hs.getDirectRecipientAddress(),
            is,
            CDA_FILE_NAME,
            data.getxCorrelationId(),
            hs.getDirectTlsVersion());
      } else {

        logger.error(" Cannot send Direct message since both Direct Host and SMTP Urls are empty ");
      }

    } catch (Exception e) {

      String msg = "Unable to send Direct Message due to a connection exception";
      logger.error(msg, e);

      throw new RuntimeException(msg);
    }
  }

  public void sendMail(
      String host,
      String username,
      String password,
      String port,
      String recipientAddr,
      InputStream is,
      String filename,
      String correlationId,
      String directTlsVersion)
      throws Exception {

    Properties props = new Properties();

    // Setup the property to authenticate.
    props.put("mail.smtp.auth", "true");

    // Trust all certificates
    props.setProperty("mail.smtp.ssl.trust", "*");

    //  Enable SSL Connections from the client.
    props.setProperty("mail.smtp.ssl.enable", "true");

    // Set TLS protocols
    if (!StringUtils.isEmpty(directTlsVersion)) {
      props.setProperty("smtp.ssl.protocols", directTlsVersion);
    }

    Session session = Session.getInstance(props, null);

    logger.info(" Retrieve Session instance for sending Direct mail ");

    DirectMimeMessage message = new DirectMimeMessage(session, correlationId, host);

    logger.info("Setting From Address {}", username);
    message.setFrom(new InternetAddress(username));

    String toAddr = StringUtils.deleteWhitespace(recipientAddr);

    message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(toAddr));

    logger.info("Finished setting recipients {}", toAddr);

    message.setSubject("eICR Report ");
    message.setText(CDA_FILE_NAME);

    logger.info("Creating Message Body Part ");
    BodyPart mimeBodyPart = new MimeBodyPart();
    Multipart multipart = new MimeMultipart();
    DataSource source = new ByteArrayDataSource(is, "application/xml; charset=UTF-8");
    mimeBodyPart.setDataHandler(new DataHandler(source));

    mimeBodyPart.setFileName(filename + ".xml");
    multipart.addBodyPart(mimeBodyPart);

    // Send the complete message parts
    message.setContent(multipart);

    logger.info(" Completed constructing the Message ");
    Transport transport = session.getTransport("smtp");
    transport.connect(host, Integer.parseInt(port), username, password);

    logger.info(" Connection successful to the direct host {}", host);
    transport.sendMessage(message, message.getAllRecipients());

    logger.info("Finished sending Direct message successfully, closing connection ");
    transport.close();

    logger.info(" Finished sending Direct Message ");
  }

  /**
   * The method is used to receive data from a PHA/TTP using Direct Transport. As a direct client,
   * the implementer has to connect to a HISP and then receive the message via Direct.
   *
   * @param data - The KarProcessingData that contains the necessary data to process and receive the
   *     information.
   */
  @Override
  public void receiveRrDataUsingDirect(KarProcessingData data) {

    logger.info(" Start receiving process from Direct HISP ");

    HealthcareSetting hs = data.getHealthcareSetting();

    logger.info(" Receiving Mail from Inbox {}", hs.getDirectUser());

    if (!StringUtils.isEmpty(hs.getImapUrl())) {

      logger.info("Using Imap URL {} to receive the data", hs.getImapUrl());

      readMailUsingImap(
          hs.getImapUrl(),
          hs.getDirectUser(),
          hs.getDirectPwd(),
          hs.getImapPort(),
          data.getxCorrelationId(),
          hs.getDirectTlsVersion());

      logger.info(" Finished sending the message using Direct ");

    } else if (!StringUtils.isEmpty(hs.getDirectHost())) {

      readMailUsingImap(
          hs.getDirectHost(),
          hs.getDirectUser(),
          hs.getDirectPwd(),
          hs.getImapPort(),
          data.getxCorrelationId(),
          hs.getDirectTlsVersion());

    } else {

      logger.error(
          " Cannot receive Direct message since both Direct Host and SMTP Urls are empty ");
    }

    logger.info(" Finish receiving process from Direct HISP ");
  }

  /**
   * The method is used to receive the mail using HISP's INBOX using IMAP protocol.
   *
   * @param host
   * @param username
   * @param password
   * @param port
   * @param coorleationId
   */
  public void readMailUsingImap(
      String host,
      String username,
      String password,
      String port,
      String coorleationId,
      String directTlsVersion) {

    try {

      String mId;
      logger.info("Reading mail..");
      logger.info("coorleationId:{}", coorleationId);

      // Properties for Javamail
      Properties props = new Properties();
      props.put("mail.imap.auth", "true");
      props.put("mail.imap.ssl.enable", "true");
      props.put("mail.imap.ssl.trust", "*");
      if (!StringUtils.isEmpty(directTlsVersion)) {
        props.put("mail.imap.ssl.protocols", directTlsVersion);
      }

      Session session = Session.getInstance(props, null);

      Store store = session.getStore(IMAP);

      int portNum = Integer.parseInt(port);
      logger.info("Connecting to IMAP Inbox");
      store.connect(host, portNum, username, password);

      Folder inbox = store.getFolder(INBOX);
      inbox.open(Folder.READ_WRITE);

      Flags seen = new Flags(Flags.Flag.SEEN);
      FlagTerm unseenFlagTerm = new FlagTerm(seen, false);
      Message[] messages = inbox.search(unseenFlagTerm);

      for (Message message : messages) {

        logger.info("Found unread email");
        message.getAllHeaders();

        mId = getMessageId(message);
        logger.info("Message-ID: {}", mId);

        Address[] froms = message.getFrom();
        String senderAddress = froms == null ? null : ((InternetAddress) froms[0]).getAddress();

        if (message.getContent() instanceof Multipart) {
          Multipart multipart = (Multipart) message.getContent();
          for (int i = 0; i < multipart.getCount(); i++) {
            BodyPart bodyPart = multipart.getBodyPart(i);

            if (bodyPart.getFileName() != null
                && (bodyPart.getFileName().contains(".xml")
                    || bodyPart.getFileName().contains(".XML"))) {

              logger.debug("Found XML Attachment");

              try (InputStream stream = bodyPart.getInputStream()) {

                ReportabilityResponse data = new ReportabilityResponse();
                data.setResponseType(EicrTypes.RrType.REPORTABILITY_RESPONSE.toString());
                String rrXml = "<?xml version=\"1.0\"?>";
                rrXml += IOUtils.toString(stream, StandardCharsets.UTF_8);
                data.setRrXml(rrXml);

                String filename = logDirectory + "_" + UUID.randomUUID() + ".xml";
                BsaServiceUtils.saveDataToFile(data.getRrXml(), filename);

                // Invoke the rrReceiver Handler for handling Reportability Response.
                logger.debug(" RrXML : {}", data.getRrXml());

                rrReceiver.handleReportabilityResponse(data, mId);
              }

              logger.info(
                  " Need to determine what to do with the response received from :  {}",
                  senderAddress);
            } else {

              logger.info(" Not an XML attachment, so ignoring the multipart file ");
            }
          }
        } else {

          // Handle Processed and Failure MDN Messages
          // The MDN format according to RFC3798 is as outlined at
          // https://datatracker.ietf.org/doc/html/rfc3798#section-3.2.6:

          // Retrieve the Disposition Header

          // Parse to remove the Disposition Mode attribute and retrieve Disposition Type

          // If Type is Processed, dispatchedjust ignore it.
          // If Type is failed - process Failure MDN

          logger.info("Not a multipart email, so ignoring for now ");
        }
      } // for all messages

      // Close the inbox since all the reads are done.
      inbox.close(true);

      // Open a new connection to remove all the deleted messages.
      deleteMailUsingImap(host, portNum, username, password, session);

    } catch (Exception e) {

      logger.error("Error while reading mail", e);
    }
  }

  /**
   * The method connects to a Direct HISP's INBOX for a specific account and deletes all emails that
   * have been read already.
   *
   * @param host
   * @param port
   * @param username
   * @param password
   * @param session
   * @throws Exception
   */
  public void deleteMailUsingImap(
      String host, int port, String username, String password, Session session) throws Exception {
    logger.info("Deleting mail..");

    Store store = session.getStore(IMAP);

    logger.info("Connecting to IMAP Inbox");
    store.connect(host, port, username, password);

    Folder inbox = store.getFolder(INBOX);
    inbox.open(Folder.READ_WRITE);
    Flags seen = new Flags(Flags.Flag.SEEN);

    FlagTerm seenFlagTerm = new FlagTerm(seen, true);
    Message[] messages = inbox.search(seenFlagTerm);

    for (Message message : messages) {

      logger.info(" Deleting message with id {}", getMessageId(message));
      message.setFlag(Flags.Flag.DELETED, true);
    }

    inbox.close(true);
  }

  /**
   * The method returns the Header Value for the Header Element with name Message-ID.
   *
   * @param m
   * @return
   */
  public String getMessageId(Message m) {

    Enumeration headers;
    try {
      headers = m.getAllHeaders();
      while (headers.hasMoreElements()) {
        Header h = (Header) headers.nextElement();
        if (h.getName().contains("Message-ID")) {
          return h.getValue();
        }
      }
    } catch (MessagingException e) {

      logger.error(" Could not retrieve the Message-ID header for the message");
    }

    return null;
  }

  @Override
  public JSONObject sendEicrDataUsingRestfulApi(KarProcessingData data) {

    String error = " RESTful API method invoked on Direct Transport which is not supported ";
    logger.error(error);

    return null;
  }
}
