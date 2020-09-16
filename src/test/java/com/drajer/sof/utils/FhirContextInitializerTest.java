package com.drajer.sof.utils;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

import java.io.IOException;

import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.IQuery;
import ca.uhn.fhir.rest.gclient.IUntypedQuery;

public class FhirContextInitializerTest {
	
	private IUntypedQuery mockIUntypedQuery;
	private IBaseBundle mockIBase;
	private IQuery mockIquery;
	private IQuery mockIquery2;
	private Bundle mockBundle;
	private FhirContext context;
	private IGenericClient client;
	private LaunchDetails launchDetails;
	
	ObjectMapper mapper = new ObjectMapper();
	
	@InjectMocks
	FhirContextInitializer fhirContextInitializer;
	
	@Before
	public void init() {
		MockitoAnnotations.initMocks(this);
		context = Mockito.mock(FhirContext.class);
		client = Mockito.mock(IGenericClient.class);
		mockIUntypedQuery = Mockito.mock(IUntypedQuery.class);
		mockIBase = Mockito.mock(IBaseBundle.class);
		mockIquery = Mockito.mock(IQuery.class);
		mockIquery2 = Mockito.mock(IQuery.class);
		mockBundle = Mockito.mock(Bundle.class);
		
		try {
			launchDetails = mapper.readValue(
					this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	@Test
	public void getResourceByPatientIdTest() throws JsonParseException, JsonMappingException, IOException {
		
		when(client.search()).thenReturn(mockIUntypedQuery);
		when(mockIUntypedQuery.byUrl(any())).thenReturn(mockIquery);
		when(mockIquery.returnBundle(Bundle.class)).thenReturn(mockIquery2);
		when(mockIquery2.execute()).thenReturn(mockBundle);
		
		IBaseBundle bundleResponse = ReflectionTestUtils.invokeMethod(fhirContextInitializer, "getResourceByPatientId", 
				launchDetails, client, context, "Encounter");
		
		assertNotNull(bundleResponse);		
		
	}
	
	@Test
	public void getObservationByPatientIdTest() throws JsonParseException, JsonMappingException, IOException {
		
		when(client.search()).thenReturn(mockIUntypedQuery);
		when(mockIUntypedQuery.byUrl(any())).thenReturn(mockIquery);
		when(mockIquery.returnBundle(Bundle.class)).thenReturn(mockIquery2);
		when(mockIquery2.execute()).thenReturn(mockBundle);
		
		IBaseBundle bundleResponse = ReflectionTestUtils.invokeMethod(fhirContextInitializer, "getObservationByPatientId", 
				launchDetails, client, context, "Encounter", "Laboratory");
		
		assertNotNull(bundleResponse);		
		
	}
	
	@Test
	public void getObservationByPatientIdAndCodeTest() throws JsonParseException, JsonMappingException, IOException {
		
		when(client.search()).thenReturn(mockIUntypedQuery);
		when(mockIUntypedQuery.byUrl(any())).thenReturn(mockIquery);
		when(mockIquery.returnBundle(Bundle.class)).thenReturn(mockIquery2);
		when(mockIquery2.execute()).thenReturn(mockBundle);
		
		IBaseBundle bundleResponse = ReflectionTestUtils.invokeMethod(fhirContextInitializer, "getObservationByPatientIdAndCode", 
				launchDetails, client, context, "Encounter", "Laboratory", "Practitioner");
		
		assertNotNull(bundleResponse);		
		
	}

}
