package com.drajer.sof.launch;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.utils.RefreshTokenScheduler;

@RestController
@RequestMapping("/")
public class LaunchController {

	@Autowired
	LaunchService authDetailsService;
	
	@Autowired
	RefreshTokenScheduler tokenScheduler;
	
	@CrossOrigin
	@RequestMapping("authDetails/{tokenId}")
    public LaunchDetails getClientById(@PathVariable("tokenId") Integer tokenId) {
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
    public LaunchDetails createAuthToken(@RequestBody LaunchDetails authDetails) {
		authDetailsService.saveOrUpdate(authDetails);
		tokenScheduler.scheduleJob(authDetails);
        return authDetails;
    }
}
