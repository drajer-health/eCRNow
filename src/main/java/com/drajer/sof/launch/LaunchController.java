package com.drajer.sof.launch;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import com.drajer.sof.utils.RefreshTokenScheduler;

@RestController
@RequestMapping("/")
public class LaunchController {

	@Autowired
	LaunchService authDetailsService;
	
	@Autowired
	RefreshTokenScheduler tokenScheduler;
	
	@Autowired
	TriggerQueryService triggerQueryService;
	
	@Autowired
	LoadingQueryService loadingQueryService;
	
	@CrossOrigin
	@RequestMapping("launchDetails/{tokenId}")
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
	@RequestMapping(value = "launchDetails", method = RequestMethod.POST)
    public LaunchDetails createAuthToken(@RequestBody LaunchDetails launchDetails) {
		authDetailsService.saveOrUpdate(launchDetails);
		tokenScheduler.scheduleJob(launchDetails);
        return launchDetails;
    }
	
	@CrossOrigin
	@RequestMapping("triggerQueryService/{tokenId}")
	public String triggerDataFromEHR(@PathVariable("tokenId") Integer tokenId) {
		LaunchDetails launchDetails = authDetailsService.getAuthDetailsById(tokenId);
		SimpleDateFormat ft = new SimpleDateFormat ("yyyy-MM-dd");
		try {
			Date start = ft.parse("2012-02-19");
			triggerQueryService.getData(launchDetails, start, new Date());
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return "Success";
	}
	
	@CrossOrigin
	@RequestMapping("loadingQueryService/{tokenId}")
	public String loadingDataFromEHR(@PathVariable("tokenId") Integer tokenId) {
		LaunchDetails launchDetails = authDetailsService.getAuthDetailsById(tokenId);
		SimpleDateFormat ft = new SimpleDateFormat ("yyyy-MM-dd");
		try {
			Date start = ft.parse("2012-02-19");
			loadingQueryService.getData(launchDetails, start, new Date());
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return "Success";
	}
}
