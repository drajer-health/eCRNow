package com.drajer.sof.launch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LoadingQueryService;

@RestController
public class ClientDetailsController {

	@Autowired
	ClientDetailsService clientDetailsService;
	
	private final Logger logger = LoggerFactory.getLogger(ClientDetailsController.class);
	
	@CrossOrigin
	@RequestMapping("/api/clientDetails/{clientId}")
    public ClientDetails getClientDetailsById(@PathVariable("clientId") Integer clientId) {
        return clientDetailsService.getClientDetailsById(clientId);
    }
	
	// POST method to create a Client
	@CrossOrigin
	@RequestMapping(value = "/api/clientDetails", method = RequestMethod.POST)
    public ClientDetails createClientDetails(@RequestBody ClientDetails clientDetails) {
		clientDetailsService.saveOrUpdate(clientDetails);
        return clientDetails;
    }
	
	@CrossOrigin
	@RequestMapping("/api/clientDetails")
    public ClientDetails getClientDetailsById(@RequestParam(value="url") String url) {
		return clientDetailsService.getClientDetailsByUrl(url);
	}
}
