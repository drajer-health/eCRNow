package com.drajer.bsa.routing.impl;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.routing.DataTransportInterface;
import com.drajer.bsa.service.RrReceiver;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.ecrapp.model.EicrTypes;
import com.drajer.ecrapp.model.ReportabilityResponse;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;
import java.util.Properties;
import java.util.UUID;
import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.mail.*;
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

  @Value("${mail.read.retries}")
  private Integer imapReadRetryLimit;

  @Value("${mail.imap.batch.size:500}")
  private int imapBatchSize;

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

    HealthcareSetting hs = data.getHealthcareSetting();
    String correlationId = data.getxCorrelationId();

    logger.info(
        "**** START Direct Transmission: user={}, correlationId={}, to={} ****",
        hs.getDirectUser(),
        correlationId,
        hs.getDirectRecipientAddress());

    String payload = data.getSubmittedCdaData();
    InputStream is = IOUtils.toInputStream(payload, StandardCharsets.UTF_8);

    try {
      if (!StringUtils.isEmpty(hs.getSmtpUrl())) {
        logger.info(
            "sendEicrDataUsingDirect: using SMTP URL, user={}, correlationId={}, smtpUrl={}",
            hs.getDirectUser(),
            correlationId,
            hs.getSmtpUrl());

        sendMail(
            hs.getSmtpUrl(),
            hs.getDirectUser(),
            hs.getDirectPwd(),
            hs.getSmtpPort(),
            hs.getDirectRecipientAddress(),
            is,
            CDA_FILE_NAME,
            correlationId,
            hs.getDirectTlsVersion());

      } else if (!StringUtils.isEmpty(hs.getDirectHost())) {
        logger.info(
            "sendEicrDataUsingDirect: using Direct host, user={}, correlationId={}, host={}",
            hs.getDirectUser(),
            correlationId,
            hs.getDirectHost());

        sendMail(
            hs.getDirectHost(),
            hs.getDirectUser(),
            hs.getDirectPwd(),
            hs.getSmtpPort(),
            hs.getDirectRecipientAddress(),
            is,
            CDA_FILE_NAME,
            correlationId,
            hs.getDirectTlsVersion());
      } else {
        String msg =
            "sendEicrDataUsingDirect: cannot send — both Direct Host and SMTP URL are empty,"
                + " user="
                + hs.getDirectUser()
                + ", correlationId="
                + correlationId;
        logger.error(msg);
        throw new RuntimeException(msg);
      }

      logger.info(
          "**** END Direct Transmission: user={}, correlationId={} ****",
          hs.getDirectUser(),
          correlationId);

    } catch (RuntimeException re) {
      throw re;
    } catch (Exception e) {
      String msg =
          "sendEicrDataUsingDirect: unable to send Direct message, user="
              + hs.getDirectUser()
              + ", correlationId="
              + correlationId;
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

    logger.info(
        "sendMail: start — user={}, correlationId={}, to={}, host={}, port={}",
        username,
        correlationId,
        recipientAddr,
        host,
        port);

    Properties props = new Properties();
    props.put("mail.smtp.auth", "true");
    props.setProperty("mail.smtp.ssl.trust", "*");
    props.setProperty("mail.smtp.ssl.enable", "true");
    if (!StringUtils.isEmpty(directTlsVersion)) {
      props.setProperty("mail.smtp.ssl.protocols", directTlsVersion);
    }

    Session session = Session.getInstance(props, null);
    DirectMimeMessage message = new DirectMimeMessage(session, correlationId, host);
    message.setFrom(new InternetAddress(username));

    String toAddr = StringUtils.deleteWhitespace(recipientAddr);
    message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(toAddr));
    message.setSubject("eICR Report ");
    message.setText(CDA_FILE_NAME);

    BodyPart mimeBodyPart = new MimeBodyPart();
    Multipart multipart = new MimeMultipart();
    DataSource source = new ByteArrayDataSource(is, "application/xml; charset=UTF-8");
    mimeBodyPart.setDataHandler(new DataHandler(source));
    mimeBodyPart.setFileName(filename + ".xml");
    multipart.addBodyPart(mimeBodyPart);
    message.setContent(multipart);

    logger.info(
        "sendMail: message constructed, user={}, correlationId={}, to={} — connecting to host",
        username,
        correlationId,
        toAddr);

    Transport transport = session.getTransport("smtp");
    transport.connect(host, Integer.parseInt(port), username, password);

    logger.info(
        "sendMail: connected to host={}, user={}, correlationId={} — sending message",
        host,
        username,
        correlationId);

    transport.sendMessage(message, message.getAllRecipients());

    transport.close();

    logger.info(
        "sendMail: message sent and connection closed — user={}, correlationId={}, to={}",
        username,
        correlationId,
        toAddr);
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

    HealthcareSetting hs = data.getHealthcareSetting();
    String correlationId = data.getxCorrelationId();

    logger.info(
        "receiveRrDataUsingDirect: start — user={}, correlationId={}",
        hs.getDirectUser(),
        correlationId);

    if (!StringUtils.isEmpty(hs.getImapUrl())) {
      logger.info(
          "receiveRrDataUsingDirect: using IMAP URL, user={}, correlationId={}, imapUrl={}",
          hs.getDirectUser(),
          correlationId,
          hs.getImapUrl());

      readMailUsingImap(
          hs.getImapUrl(),
          hs.getDirectUser(),
          hs.getDirectPwd(),
          hs.getImapPort(),
          correlationId,
          hs.getDirectTlsVersion());

    } else if (!StringUtils.isEmpty(hs.getDirectHost())) {
      logger.info(
          "receiveRrDataUsingDirect: using Direct host, user={}, correlationId={}, host={}",
          hs.getDirectUser(),
          correlationId,
          hs.getDirectHost());

      readMailUsingImap(
          hs.getDirectHost(),
          hs.getDirectUser(),
          hs.getDirectPwd(),
          hs.getImapPort(),
          correlationId,
          hs.getDirectTlsVersion());

    } else {
      logger.error(
          "receiveRrDataUsingDirect: cannot receive — both Direct Host and IMAP URL are empty,"
              + " user={}, correlationId={}",
          hs.getDirectUser(),
          correlationId);
    }

    logger.info(
        "receiveRrDataUsingDirect: end — user={}, correlationId={}",
        hs.getDirectUser(),
        correlationId);
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

  private void readMailUsingImap(
      String host,
      String username,
      String password,
      String port,
      String correlationId,
      String directTlsVersion) {

    int attempt = 0;
    boolean success = false;

    while (attempt < imapReadRetryLimit && !success) {
      attempt++;
      try {
        readMail(host, username, password, port, correlationId, directTlsVersion);
        success = true;
      } catch (FolderClosedException fce) {
        logger.warn(
            "readMailUsingImap: FolderClosedException — user={}, correlationId={}, attempt {}/{}",
            username,
            correlationId,
            attempt,
            imapReadRetryLimit);
        try {
          Thread.sleep(2000);
        } catch (InterruptedException ignored) {
          Thread.currentThread().interrupt();
        }
      } catch (Exception e) {
        logger.error(
            "readMailUsingImap: error on attempt {}/{} — user={}, correlationId={}",
            attempt,
            imapReadRetryLimit,
            username,
            correlationId,
            e);
      }
    }

    if (!success) {
      logger.error(
          "readMailUsingImap: failed after {} attempt(s) — user={}, correlationId={}",
          imapReadRetryLimit,
          username,
          correlationId);
    }
  }

  private void readMail(
      String host,
      String username,
      String password,
      String port,
      String correlationId,
      String directTlsVersion)
      throws Exception {

    logger.info(
        "readMail: start — user={}, correlationId={}, host={}, port={}",
        username,
        correlationId,
        host,
        port);

    Properties props = new Properties();
    props.put("mail.imap.auth", "true");
    props.put("mail.imap.ssl.enable", "true");
    props.setProperty("mail.imap.ssl.trust", "*");
    props.put("mail.imap.connectionpoolsize", "1");
    if (!StringUtils.isEmpty(directTlsVersion)) {
      props.put("mail.imap.ssl.protocols", directTlsVersion);
    }

    Session session = Session.getInstance(props, null);
    Store store = session.getStore(IMAP);
    store.connect(host, Integer.parseInt(port), username, password);

    logger.info(
        "readMail: connected to IMAP store — user={}, correlationId={}", username, correlationId);

    Folder inbox = store.getFolder(INBOX);
    inbox.open(Folder.READ_WRITE);

    try {
      processMessages(inbox, username, correlationId);
    } finally {
      inbox.close(true);
      store.close();
      logger.info("readMail: inbox closed — user={}, correlationId={}", username, correlationId);
    }
  }

  private void processMessages(Folder inbox, String username, String correlationId)
      throws Exception {

    int total = inbox.getMessageCount();
    int unseenCount = inbox.getUnreadMessageCount();

    if (unseenCount == 0 && total > 0) {
      logger.info(
          "processMessages: no unread messages in inbox — user={}, correlationId={}",
          username,
          correlationId);
      deleteReadMessages(inbox, username, correlationId);
      return;
    }
    int batchCount = (total == 0) ? 0 : (int) Math.ceil((double) total / imapBatchSize);

    logger.info(
        "processMessages: user={}, correlationId={}, totalMessages={}, batchSize={}, totalBatches={}",
        username,
        correlationId,
        total,
        imapBatchSize,
        batchCount);

    if (total == 0) {
      logger.info(
          "processMessages: no messages in inbox — user={}, correlationId={}",
          username,
          correlationId);
      return;
    }

    Flags seen = new Flags(Flags.Flag.SEEN);
    FlagTerm unseenFlagTerm = new FlagTerm(seen, false);

    for (int batch = 1; batch <= batchCount; batch++) {
      int start = (batch - 1) * imapBatchSize + 1;
      int end = Math.min(batch * imapBatchSize, total);

      logger.info(
          "processMessages: batch {} of {} — user={}, correlationId={}, fetchingMessages={}-{} of {}",
          batch,
          batchCount,
          username,
          correlationId,
          start,
          end,
          total);

      Message[] batchMessages = inbox.getMessages(start, end);

      Message[] unread = inbox.search(unseenFlagTerm, batchMessages);

      logger.info(
          "processMessages: batch {} of {} — user={}, correlationId={}, unreadInBatch={} of {} fetched",
          batch,
          batchCount,
          username,
          correlationId,
          unread.length,
          batchMessages.length);

      for (int i = 0; i < unread.length; i++) {
        Message message = unread[i];
        String msgId = getMessageId(message);

        logger.info(
            "processMessages: batch {} of {}, message {} of {} — user={}, correlationId={}, messageId={}",
            batch,
            batchCount,
            (i + 1),
            unread.length,
            username,
            correlationId,
            msgId);

        try {
          processSingleMessage(message, username, correlationId, msgId);

        } catch (FolderClosedException fe) {
          logger.warn(
              "processMessages: FolderClosedException on message {} of batch {} of {} — user={}, correlationId={}, messageId={} — rethrowing for retry",
              (i + 1),
              batch,
              batchCount,
              username,
              correlationId,
              msgId);
          throw fe;
        } catch (Exception e) {
          logger.error(
              "processMessages: error processing message {} of batch {} of {} — user={}, correlationId={}, messageId={}",
              (i + 1),
              batch,
              batchCount,
              username,
              correlationId,
              msgId,
              e);
        }
      }
    }
    deleteReadMessages(inbox, username, correlationId);

    logger.info(
        "processMessages: completed all {} batch(es) — user={}, correlationId={}",
        batchCount,
        username,
        correlationId);
  }

  private void processSingleMessage(
      Message message, String username, String correlationId, String msgId)
      throws MessagingException, IOException {

    Address[] froms = message.getFrom();
    String senderAddress = froms == null ? null : ((InternetAddress) froms[0]).getAddress();

    if (message.getContent() instanceof Multipart) {
      Multipart multipart = (Multipart) message.getContent();
      for (int i = 0; i < multipart.getCount(); i++) {
        BodyPart bodyPart = multipart.getBodyPart(i);
        if (bodyPart.getFileName() != null
            && bodyPart.getFileName().toLowerCase().contains(".xml")) {

          logger.info(
              "processSingleMessage: XML attachment found — user={}, correlationId={}, messageId={}, sender={}",
              username,
              correlationId,
              msgId,
              senderAddress);

          try (InputStream stream = bodyPart.getInputStream()) {
            ReportabilityResponse data = new ReportabilityResponse();
            data.setResponseType(EicrTypes.RrType.REPORTABILITY_RESPONSE.toString());
            String rrXml =
                "<?xml version=\"1.0\"?>" + IOUtils.toString(stream, StandardCharsets.UTF_8);
            data.setRrXml(rrXml);

            String filename = logDirectory + "_" + UUID.randomUUID() + ".xml";
            BsaServiceUtils.saveDataToFile(data.getRrXml(), filename);

            logger.info(
                "processSingleMessage: invoking RR handler — user={}, correlationId={}, messageId={}",
                username,
                correlationId,
                msgId);

            rrReceiver.handleReportabilityResponse(data, msgId);
          }
          message.setFlag(Flags.Flag.SEEN, true);
          logger.info(
              "processSingleMessage: marked SEEN — user={}, correlationId={}, messageId={}, sender={}",
              username,
              correlationId,
              msgId,
              senderAddress);

        } else {
          logger.info(
              "processSingleMessage: non-XML attachment, skipping — user={}, correlationId={}, messageId={}",
              username,
              correlationId,
              msgId);
        }
      }
    } else {
      logger.info(
          "processSingleMessage: not a multipart message, ignoring — user={}, correlationId={}, messageId={}",
          username,
          correlationId,
          msgId);
    }
  }

  private void deleteReadMessages(Folder inbox, String username, String correlationId) {
    try {
      if (inbox != null && inbox.isOpen()) {
        Flags seen = new Flags(Flags.Flag.SEEN);
        FlagTerm seenFlagTerm = new FlagTerm(seen, true);
        Message[] messages = inbox.search(seenFlagTerm);

        logger.info(
            "deleteReadMessages: found {} read message(s) to delete — user={}, correlationId={}",
            messages.length,
            username,
            correlationId);

        for (Message message : messages) {
          String msgId = getMessageId(message);
          logger.info(
              "deleteReadMessages: deleting — user={}, correlationId={}, messageId={}",
              username,
              correlationId,
              msgId);
          message.setFlag(Flags.Flag.DELETED, true);
        }
      }
    } catch (Exception e) {
      logger.error(
          "deleteReadMessages: error deleting read messages — user={}, correlationId={}",
          username,
          correlationId,
          e);
    }
  }
}
