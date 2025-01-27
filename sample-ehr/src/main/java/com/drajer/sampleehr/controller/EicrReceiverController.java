package com.drajer.sampleehr.controller;



import com.drajer.sampleehr.service.EicrReceiverService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

@RestController
public class EicrReceiverController {

	@Autowired
	EicrReceiverService eicrReceiverService;

	private final Logger logger = LoggerFactory.getLogger(EicrReceiverController.class);

	@CrossOrigin
	@RequestMapping(value = "/api/receiveEicr", method = RequestMethod.POST,produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<Object> saveEicrXML(@RequestParam() Map<String, String> params,
											  @RequestBody String eicrXml) {
		Map<String,Object> responseObj = new HashMap<>();
		logger.info("Received Eicr XML with Parameters");


		// Call service method to process the EICR details
		eicrReceiverService.saveEicrDetails(params, eicrXml);

		// Create a response with a success message
		responseObj.put("message", "Received Eicr Details.");

		// Return the response with HTTP Status OK
		return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON).body(responseObj);
	}
}
