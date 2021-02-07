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

import com.drajer.sampleehr.service.AuthorizationService;

@RestController
public class AuthorizationEndpoint {

	private final Logger logger = LoggerFactory.getLogger(AuthorizationEndpoint.class);
	
	@Autowired
	AuthorizationService authService;
	
	@CrossOrigin
	@RequestMapping(value = "/api/authorize", method = RequestMethod.POST)
	public ResponseEntity<String> getAuthorization(@RequestBody Map<String,String> params,HttpServletRequest request,
		      HttpServletResponse response) {
		JSONObject responseObj =null;
		logger.info("Received Authorization request with Parameters");
		try {
			if(!params.containsKey("client_id") || !params.containsKey("client_secret")) {
				return new ResponseEntity<String>("Please provide all the Requried Parameters ClientId and ClientSecret", HttpStatus.BAD_REQUEST);
			}
			responseObj = new JSONObject();
			responseObj.put("expires", "300");
			responseObj.put("token_type", "bearer");
			responseObj.put("access_token", authService.generateNewToken());
		}catch(Exception e) {
			logger.error("Error in Authorization");
		}

		if(responseObj != null) {
			return new ResponseEntity<String>(responseObj.toString(), HttpStatus.OK);	
		} else {
			responseObj = new JSONObject();
			responseObj.put("message", "Error in Processing the Request");
			return new ResponseEntity<String>(responseObj.toString(), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}
	
}
