package com.drajer.eca.model;

import java.io.IOException;
import java.util.Date;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;

import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@RunWith(MockitoJUnitRunner.class)
public class PeriodicUpdateEicrActionTest {
	
	@InjectMocks
	private PeriodicUpdateEicrAction periodicUpdateEicrAction;
	
	ObjectMapper mapper = new ObjectMapper();
	
	@Test
	public void testPeriodicUpdateEicrAction() throws Exception {
		LaunchDetails launchDetails = mapper.readValue(this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
		
		WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;
		
		periodicUpdateEicrAction.execute(launchDetails, launchType);
	}
	
	/*
	 * In case invalid Object passed to Execute method,
	 */
	@Test(expected = RuntimeException.class)
	public void whenInvalidObjectPassedThrowRuntimeException() throws Exception {
		ObjectMapper mapper = new ObjectMapper();
		WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;
		
		periodicUpdateEicrAction.execute(mapper, launchType);
		
	}

}
