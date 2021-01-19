package com.drajer.sampleehr.controller;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.drajer.sampleehr.service.EicrReceiverService;

@RestController
public class EicrReceiverController {

	@Autowired
	EicrReceiverService eicrReceiverService;
	
	private final Logger logger = LoggerFactory.getLogger(EicrReceiverController.class);

	@CrossOrigin
	@RequestMapping(value = "/api/receiveEicr", method = RequestMethod.POST)
	public ResponseEntity<String> saveEicrXML(@RequestParam Map<String,String> params, @RequestBody String eicrXml,HttpServletRequest request,
		      HttpServletResponse response) {
		JSONObject responseObj =null;
		logger.info("Received Eicr XML with Parameters");
		
			eicrReceiverService.saveEicrDetails(params, eicrXml);
			responseObj = new JSONObject();
			responseObj.put("message", "Received Eicr Details.");
		
		return new ResponseEntity<String>(responseObj.toString(), HttpStatus.OK);
	}
}
