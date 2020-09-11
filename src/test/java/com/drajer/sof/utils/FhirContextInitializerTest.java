package com.drajer.sof.utils;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.utils.FhirContextInitializer;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;

public class FhirContextInitializerTest {
	
	ObjectMapper mapper = new ObjectMapper();
	
	@InjectMocks
	FhirContextInitializer fhirContextInitializer;
	
	@Before
	public void init() {
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void bundlePopulatorTest() throws JsonParseException, JsonMappingException, IOException {
		
		LaunchDetails launchDetails = mapper.readValue(
				this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
		FhirContext context = Mockito.mock(FhirContext.class);
		IGenericClient client = Mockito.mock(IGenericClient.class);
		
		//fhirContextInitializer.getResourceByPatientId(launchDetails, client, context, "abc");
		//when(ReflectionTestUtils.invokeMethod(fhirContextInitializer,"getResourceByPatientId",))
		
		ReflectionTestUtils.invokeMethod(fhirContextInitializer, "getResourceByPatientId", launchDetails, client, context, "abc");
		
		
	}

}
