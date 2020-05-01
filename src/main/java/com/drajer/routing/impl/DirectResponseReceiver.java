package com.drajer.routing.impl;

import com.drajer.eca.model.CloseOutEicrAction;
import com.drajer.routing.RRReceiver;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.Properties;

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
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.mime.HttpMultipartMode;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class DirectResponseReceiver extends RRReceiver {
	
	private final Logger logger = LoggerFactory.getLogger(DirectResponseReceiver.class); 

	@Override
	public Object receiveRespone(Object obj) {
		// TODO Auto-generated method stub
		
		
		return null;
	}
	
	

	


	/**
	 * Reads every unread email and passes the attachment. The mail is marked as read at the end of the transaction.
	 * 
	 * @return
	 */
	public void readMail() {

			try {
				
				String mId = null;

				DirectEicrSender util = new DirectEicrSender();
				//Reading properties file
				Properties prop = new Properties();
				String path = "./application.properties";
				FileInputStream file = new FileInputStream(path);
				prop.load(file);
				file.close();

				//Properties for Javamail
				Properties props = new Properties();
				Session session = Session.getInstance(props, null);

				Store store = session.getStore("imap");
				int port = Integer.parseInt(prop.getProperty("port"));
				logger.info("Connecting to IMAP Inbox");
				store.connect(prop.getProperty("host"),port,prop.getProperty("username"), prop.getProperty("password"));

				Folder inbox = store.getFolder("Inbox");
				inbox.open(Folder.READ_WRITE);


				Flags seen = new Flags(Flags.Flag.SEEN);
				FlagTerm unseenFlagTerm = new FlagTerm(seen, false);
				Message messages[] = inbox.search(unseenFlagTerm);

				for (Message message : messages) {

					logger.info("Found unread emails");
					Enumeration headers = message.getAllHeaders();
					while(headers.hasMoreElements()) {
						Header h = (Header) headers.nextElement();
						if(h.getName().contains("Message-ID")){
							mId = h.getValue();
						}
					}
					
					Address[] froms = message.getFrom();
					String senderAddress = froms == null ? null : ((InternetAddress) froms[0]).getAddress();
					
					if(message.getContent() instanceof Multipart){
						Multipart multipart = (Multipart) message.getContent();
						for (int i = 0; i < multipart.getCount(); i++) {
							BodyPart bodyPart = multipart.getBodyPart(i);
							InputStream stream = bodyPart.getInputStream();

							byte[] targetArray = IOUtils.toByteArray(stream);

							if (bodyPart.getFileName() != null) {
								if ((bodyPart.getFileName().contains(".xml") || bodyPart.getFileName().contains(".XML"))){
									String filename = bodyPart.getFileName();
								//	String filename = fname.split(".")[0];
									logger.info("Found XML Attachment");
									// Query Scorecard war endpoint
									CloseableHttpClient client = HttpClients.createDefault();
									FileUtils.writeByteArrayToFile(new File(bodyPart.getFileName()), targetArray);
									File file1 = new File(bodyPart.getFileName());
									HttpPost post = new HttpPost(prop.getProperty("endpoint"));
									FileBody fileBody = new FileBody(file1);

									logger.info("Calling web service");
									//POST Entity
									MultipartEntityBuilder builder = MultipartEntityBuilder.create();
									builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
									builder.addPart("ccdaFile", fileBody);
									HttpEntity entity = builder.build();
									post.setEntity(entity);


									HttpResponse response = client.execute(post);
									// Convert response to String
									InputStream iss = response.getEntity().getContent();

									logger.info("Scoring C-CDA complete");
									
								//	InputStream iss = new ByteArrayInputStream(result.getBytes());
								
									
									//Sending email with results
									util.sendMail(prop.getProperty("host"),prop.getProperty("username"), prop.getProperty("password"),senderAddress,iss,filename);
									logger.info("Email with results sent to "+senderAddress);
								}
							}
							
					
						}

					}


				}
				
				util.deleteMail(prop.getProperty("host"),prop.getProperty("username"), prop.getProperty("password"));
				

			}  catch (Exception e) {

				e.printStackTrace();
			}



		

	}

}
