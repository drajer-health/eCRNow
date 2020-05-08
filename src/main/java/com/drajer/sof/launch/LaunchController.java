package com.drajer.sof.launch;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.MatchTriggerAction;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import com.drajer.sof.utils.RefreshTokenScheduler;

@RestController
public class LaunchController {

	private final Logger logger = LoggerFactory.getLogger(LaunchController.class);

	@Autowired
	LaunchService authDetailsService;

	@Autowired
	RefreshTokenScheduler tokenScheduler;

	@Autowired
	TriggerQueryService triggerQueryService;

	@Autowired
	LoadingQueryService loadingQueryService;

	@Autowired
	WorkflowService workflowService;

	@CrossOrigin
	@RequestMapping("/api/launchDetails/{tokenId}")
	public LaunchDetails getClientById(@PathVariable("tokenId") Integer tokenId) {
		return authDetailsService.getAuthDetailsById(tokenId);
	}

	// POST method to create a Client
	@CrossOrigin
	@RequestMapping(value = "/api/launchDetails", method = RequestMethod.POST)
	public LaunchDetails createAuthToken(@RequestBody LaunchDetails launchDetails) {

		logger.info(" Saving Launch Context");
		authDetailsService.saveOrUpdate(launchDetails);

		logger.info("Scheduling refresh token job ");
		tokenScheduler.scheduleJob(launchDetails);

		// Kick off the Launch Event Processing
		logger.info("Invoking SOF Launch workflow event handler ");
		workflowService.handleWorkflowEvent(WorkflowEvent.SOF_LAUNCH, launchDetails);

		return launchDetails;
	}

	@CrossOrigin
	@RequestMapping("/api/triggerQueryService/{tokenId}")
	public String triggerDataFromEHR(@PathVariable("tokenId") Integer tokenId) {
		LaunchDetails launchDetails = authDetailsService.getAuthDetailsById(tokenId);
		SimpleDateFormat ft = new SimpleDateFormat("yyyy-MM-dd");
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
	@RequestMapping("/api/loadingQueryService/{tokenId}")
	public String loadingDataFromEHR(@PathVariable("tokenId") Integer tokenId) {
		LaunchDetails launchDetails = authDetailsService.getAuthDetailsById(tokenId);
		SimpleDateFormat ft = new SimpleDateFormat("yyyy-MM-dd");
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
