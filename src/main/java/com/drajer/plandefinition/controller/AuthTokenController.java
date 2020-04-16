package com.drajer.plandefinition.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.drajer.plandefinition.TokenScheduler;
import com.drajer.plandefinition.model.AuthTokenDetails;
import com.drajer.plandefinition.service.AuthTokenDetailsService;

@RestController
@RequestMapping("/")
public class AuthTokenController {

	@Autowired
	AuthTokenDetailsService authDetailsService;
	
	@Autowired
	TokenScheduler tokenScheduler;
	
	@CrossOrigin
	@RequestMapping("authDetails/{tokenId}")
    public AuthTokenDetails getClientById(@PathVariable("tokenId") Integer tokenId) {
        return authDetailsService.getAuthDetailsById(tokenId);
    }
	
//	@CrossOrigin
//	@GetMapping("/clients")
//    public List<Client> getAllClients() {
//        return clientRepository.findAll();
//    }
	
	// POST method to create a Client
	@CrossOrigin
	@RequestMapping(value = "authDetails", method = RequestMethod.POST)
    public AuthTokenDetails createAuthToken(@RequestBody AuthTokenDetails authDetails) {
		authDetailsService.saveOrUpdate(authDetails);
		tokenScheduler.scheduleJob(authDetails);
        return authDetails;
    }
}
