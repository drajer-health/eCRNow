package com.drajer.routing.impl;

import com.drajer.ecrapp.model.EicrTypes;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.ecrapp.service.EicrRRService;
import com.drajer.routing.RRReceiver;
import com.drajer.sof.model.LaunchDetails;
import java.io.File;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;
import java.util.Properties;
import java.util.UUID;
import javax.mail.Address;
import javax.mail.BodyPart;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Header;
import javax.mail.Message;
import javax.mail.Multipart;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.internet.InternetAddress;
import javax.mail.search.FlagTerm;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class DirectResponseReceiver extends RRReceiver {

  private final Logger logger = LoggerFactory.getLogger(DirectResponseReceiver.class);

  private static final String IMAP = "imap";
  private static final String INBOX = "Inbox";

  @Autowired EicrRRService rrService;

  @Override
  public Object receiveRespone(Object context) {

    logger.info(" **** START Executing Direct Receive **** ");

    if (context instanceof LaunchDetails) {

      logger.info(" Obtained Launch Details ");
      LaunchDetails details = (LaunchDetails) context;
      readMail(details);
    }

    return context;
  }

  public void readMail(LaunchDetails details) {

    try {

      String mId = "";
      logger.info("Reading mail..");
      // Properties for Javamail
      Properties props = new Properties();
      props.put("mail.imap.auth", "true");
      props.put("mail.imap.ssl.enable", "true");
      props.put("mail.imap.ssl.trust", "*");

      Session session = Session.getInstance(props, null);

      Store store = session.getStore(IMAP);
      int port = Integer.parseUnsignedInt(details.getImapPort());

      logger.info("Connecting to IMAP Inbox ");
      if (details.getImapUrl() == null || details.getImapUrl().isEmpty()) {
        logger.info(
            "Connecting to IMAP Inbox using the imap url {} and port {}",
            StringEscapeUtils.escapeJava(details.getDirectHost()),
            port);
        store.connect(
            details.getDirectHost(), port, details.getDirectUser(), details.getDirectPwd());
      } else {
        logger.info(
            "Connecting to IMAP Inbox using imap url {} and port {}",
            StringEscapeUtils.escapeJava(details.getDirectHost()),
            port);
        store.connect(details.getImapUrl(), port, details.getDirectUser(), details.getDirectPwd());
      }

      Folder inbox = store.getFolder(INBOX);
      inbox.open(Folder.READ_WRITE);
      logger.info("Opened the inbox for reading/writing ");

      Flags seen = new Flags(Flags.Flag.SEEN);
      FlagTerm unseenFlagTerm = new FlagTerm(seen, false);
      Message[] messages = inbox.search(unseenFlagTerm);

      for (Message message : messages) {

        logger.info("Found unread emails");
        Enumeration<?> headers = message.getAllHeaders();

        while (headers.hasMoreElements()) {
          Header h = (Header) headers.nextElement();
          if (h.getName().contains("Message-ID")) {
            mId = h.getValue();
            logger.info("Message-ID: {}", mId);
          }
        }

        Address[] froms = message.getFrom();
        String senderAddress = froms == null ? null : ((InternetAddress) froms[0]).getAddress();
        logger.info("Sender Address :{}", senderAddress);

        if (message.getContent() instanceof Multipart) {
          Multipart multipart = (Multipart) message.getContent();
          for (int i = 0; i < multipart.getCount(); i++) {
            BodyPart bodyPart = multipart.getBodyPart(i);

            if (bodyPart.getFileName() != null
                && (bodyPart.getFileName().contains(".xml")
                    || bodyPart.getFileName().contains(".XML"))) {
              String filename = UUID.randomUUID() + ".xml";
              logger.info("Found XML Attachment");

              try (InputStream stream = bodyPart.getInputStream()) {

                ReportabilityResponse data = new ReportabilityResponse();
                data.setResponseType(EicrTypes.RrType.REPORTABILITY_RESPONSE.toString());
                String rrXml = "<?xml version=\"1.0\"?>";
                rrXml += IOUtils.toString(stream, StandardCharsets.UTF_8);
                data.setRrXml(rrXml);

                FileUtils.writeStringToFile(
                    new File(filename), data.getRrXml(), StandardCharsets.UTF_8);

                logger.debug(" RrXML : {}", data.getRrXml());

                rrService.handleReportabilityResponse(data, mId, true);
              }
            }
          }
        }
      }

      deleteMail(details, details.getDirectUser(), details.getDirectPwd());

    } catch (Exception e) {

      logger.error("Error while reading mail", e);
    }
  }

  public void deleteMail(LaunchDetails details, String username, String password) throws Exception {

    logger.info("Deleting mail of User:{}{}", username, password);

    Properties props = new Properties();

    props.setProperty("mail.imap.ssl.trust", "*");
    props.setProperty("mail.imap.ssl.enable", "true");
    Session session = Session.getInstance(props, null);

    Store store = session.getStore("imap");
    int port = Integer.parseUnsignedInt(details.getImapPort());

    if (details.getImapUrl() == null || details.getImapUrl().isEmpty()) {
      logger.info(
          "Connecting to IMAP Inbox using the imap url {} and port {}",
          StringEscapeUtils.escapeJava(details.getDirectHost()),
          port);
      store.connect(details.getDirectHost(), port, details.getDirectUser(), details.getDirectPwd());
    } else {
      logger.info(
          "Connecting to IMAP Inbox using imap url {} and port {}",
          StringEscapeUtils.escapeJava(details.getDirectHost()),
          port);
      store.connect(details.getImapUrl(), port, details.getDirectUser(), details.getDirectPwd());
    }

    Folder inbox = store.getFolder(INBOX);
    inbox.open(Folder.READ_WRITE);
    Flags seen = new Flags(Flags.Flag.SEEN);

    FlagTerm seenFlagTerm = new FlagTerm(seen, true);
    Message[] messages = inbox.search(seenFlagTerm);

    logger.info(" Marking messages as deleted ");
    for (Message message : messages) {
      message.setFlag(Flags.Flag.DELETED, true);
    }

    inbox.close(true);
  }
}
