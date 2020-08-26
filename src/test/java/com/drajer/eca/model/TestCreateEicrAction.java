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
public class TestCreateEicrAction {
	
	@InjectMocks
	private CreateEicrAction createtEicrAction;
	
	ObjectMapper mapper = new ObjectMapper();
	
	@Test
	public void testCreateEicrAction() throws Exception {
		
		
		LaunchDetails launchDetails1 = mapper.readValue(this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
		
		WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;
		
		createtEicrAction.execute(launchDetails1, launchType);
	}
	/*
	 * In case invalid Object passed to Execute method,
	 */
	@Test(expected = RuntimeException.class)
	public void whenInvalidObjectPassedThrowRuntimeException() throws Exception {
		ObjectMapper mapper = new ObjectMapper();
		WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;
		
		createtEicrAction.execute(mapper, launchType);
		
	}
	
}

