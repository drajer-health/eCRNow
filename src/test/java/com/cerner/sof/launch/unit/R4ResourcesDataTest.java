package com.cerner.sof.launch.unit;

import static org.hamcrest.CoreMatchers.any;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.Date;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.IdType;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.R4ResourcesData;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;

@RunWith(PowerMockRunner.class)
@PrepareForTest({FhirContextInitializer.class})
public class R4ResourcesDataTest {
	
	ObjectMapper mapper = new ObjectMapper();
	
	@InjectMocks
	R4ResourcesData r4ResourcesData;
		
	@Mock
	FhirContextInitializer mockResourceData;
	
	@Before
	public void init() {
		
		PowerMockito.mock(FhirContextInitializer.class);		
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void observationDataTest() throws JsonParseException, JsonMappingException, IOException
	{
		LaunchDetails launchDetails = mapper.readValue(
				this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
		Bundle bundle = Mockito.mock(Bundle.class);
		IGenericClient client = Mockito.mock(IGenericClient.class);
		FhirContext context = Mockito.mock(FhirContext.class);
		R4FhirData r4FhirData = Mockito.mock(R4FhirData.class);
		
		Date startDate = launchDetails.getStartDate();
		Date endDate = launchDetails.getEndDate();
		
		IdType id = new IdType();
		id.setValue("test");
		Encounter encounter = new Encounter();
		encounter.setIdElement(id);
		
		
		//Encounter encounter = PowerMockito.mock(Encounter.class);
		//IdType id = PowerMockito.mock(IdType.class);
		
		
		
		//when(encounter.getIdElement()).thenReturn(id);
		//when(id.getValue()).thenReturn(Mockito.anyString());
		
		//when(encounter.getIdElement().getValue()).thenReturn(Mockito.anyString());
		
		//when(encounter.getIdElement().getValue().isEmpty()).thenReturn(false);
		
		r4ResourcesData.getPregnancyObservationData(context, client, launchDetails, r4FhirData, encounter, startDate, endDate);
		
		//when(mockResourceData.getObservationByPatientIdAndCode(launchDetails, client, context, "Observation", QueryConstants.PREGNANCY_CODE, QueryConstants.LOINC_CODE_SYSTEM)).
		//thenReturn(bundle);
		
		
	}

}
