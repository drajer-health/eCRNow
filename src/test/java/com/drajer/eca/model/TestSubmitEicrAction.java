package com.drajer.eca.model;

import java.util.Date;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;

import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.databind.ObjectMapper;

@RunWith(MockitoJUnitRunner.class)
public class TestSubmitEicrAction {
	
	@InjectMocks
	private SubmitEicrAction submitEicrAction;
	
	ObjectMapper mapper = new ObjectMapper();
	
	@Test
	public void testSubmitEicrAction() throws Exception {
		LaunchDetails launchDetails = mapper.readValue(this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
		
		WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;
		submitEicrAction.execute(launchDetails, launchType);
	}

}
