package com.drajer.sampleehr.service;

import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Map;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class EicrReceiverService {
	
	@Value("${received.eicr.xml.path}")
	private String receivedEicrXmlPath;
	
	private final Logger logger = LoggerFactory.getLogger(EicrReceiverService.class);

	public void saveEicrDetails(Map<String,String> params,String eicrXml) {
		JSONObject data = new JSONObject();
		DataOutputStream outStream = null;
		try {
			
			data.put("eicrXml", eicrXml);
			
			logger.info("Writing data to file: " + receivedEicrXmlPath);
			outStream = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(receivedEicrXmlPath)));
			outStream.writeBytes(data.toString());
		} catch (IOException e) {
			logger.error("Unable to write data to file: " + receivedEicrXmlPath, e);
		} finally {
			if (outStream != null) {
				try {
					outStream.close();
				} catch (IOException e) {
					logger.error(" Unable to close Data output stream");
				}
			}
		}
	}
}
