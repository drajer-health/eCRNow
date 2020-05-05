package com.drajer.routing.impl;

import com.drajer.eca.model.PatientExecutionState;
import com.drajer.eca.model.SubmitEicrAction;
import com.drajer.routing.EicrSender;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Properties;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.mail.BodyPart;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Message;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class DirectEicrSender extends EicrSender {
	
	private final Logger logger = LoggerFactory.getLogger(DirectEicrSender.class);
	
	private static final String FILE_NAME = "eICR Report";

	@Override
	public void sendData(Object context, String data) {
		
		logger.info(" Getting ready to send Direct Eicr ");
			
		if (context instanceof LaunchDetails) {

			logger.info(" Obtained Launch Details ");
			LaunchDetails details = (LaunchDetails) context;
			ObjectMapper mapper = new ObjectMapper();
			PatientExecutionState state = null;

			try {
				state = mapper.readValue(details.getStatus(), PatientExecutionState.class);			
			} catch (JsonMappingException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (JsonProcessingException e1) {
				// TODO Auto-generated catch bloc
				e1.printStackTrace();
			}
		
			InputStream is = IOUtils.toInputStream(data, StandardCharsets.UTF_8);
			
			try {
			//	sendMail(details.getDirectHost(), details.getDirectUser(), details.getDirectPwd(), details.getDirectRecipient(), is, DirectEicrSender.FILE_NAME);
				
				// For testing purposes..use site.
				sendMail("ttpds2.sitenv.org", "hisp-testing@ttpds2.sitenv.org", "hisptestingpass", "hisp-testing@direct.ett.healthit.gov", is, DirectEicrSender.FILE_NAME);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		
	}
		
	public void sendMail(String host, String username, String password, String receipientAddr, InputStream is, String filename) throws Exception{
			
		Properties props = new Properties();
			
			
			props.put("mail.smtp.auth", "true");
			props.put("mail.smtp.auth.mechanisms", "PLAIN");
			props.setProperty("mail.smtp.ssl.trust", "*");

			Session session = Session.getInstance(props, null);

			Message message = new MimeMessage(session);
			message.setFrom(new InternetAddress(username));
			message.setRecipients(Message.RecipientType.TO,
					InternetAddress.parse(receipientAddr));
			message.setSubject("eICR Report ");
			message.setText("eICR Report");
			BodyPart messageBodyPart = new MimeBodyPart();
			Multipart multipart = new MimeMultipart();
			DataSource source = new ByteArrayDataSource(is,"application/xml");
			messageBodyPart.setDataHandler(new DataHandler(source));

			messageBodyPart.setFileName(filename + "_eICRReport.xml");
			

			multipart.addBodyPart(messageBodyPart);
			
			// Send the complete message parts
			message.setContent(multipart);
			Transport transport = session.getTransport("smtp");
			transport.connect(host, 25, username, password);
			transport.sendMessage(message, message.getAllRecipients());
			transport.close();
		}
		
		public void deleteMail(String host, String username, String password) throws Exception{
			
			Properties prop = new Properties();
			String path = "./application.properties";
			FileInputStream file = new FileInputStream(path);
			prop.load(file);
			file.close();
			
			Properties props = new Properties();
			
			Session session = Session.getInstance(props, null);

			Store store = session.getStore("imap");
			int port = Integer.parseInt(prop.getProperty("port"));
			store.connect(prop.getProperty("host"),port,prop.getProperty("username"), prop.getProperty("password"));

			Folder inbox = store.getFolder("Inbox");
			inbox.open(Folder.READ_WRITE);


			Flags seen = new Flags(Flags.Flag.SEEN);
			
			FlagTerm seenFlagTerm = new FlagTerm(seen,true);
			Message messages[] = inbox.search(seenFlagTerm);

			for (Message message : messages){

				message.setFlag(Flags.Flag.DELETED, true);
			}
			
			inbox.close(true);

		}

}
