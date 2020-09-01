package com.cerner.sof.launch.unit;

import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.Date;

import org.hl7.fhir.r4.model.Period;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.drajer.sof.launch.LaunchController;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.utils.FhirContextInitializer;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.rest.client.api.IGenericClient;

public class StartEndDateTest {

	ObjectMapper mapper = new ObjectMapper();

	@InjectMocks
	LaunchController launchController;

	@Mock
	FhirContextInitializer fhirContextInitializer;

	@Before
	public void init() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testStartEndDateDSTU2() throws JsonParseException, JsonMappingException, IOException {

		LaunchDetails currentStateDetails = mapper.readValue(
				this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
		ClientDetails clientDetails = mapper.readValue(
				this.getClass().getClassLoader().getResourceAsStream("saveClientDataEntry1.json"), ClientDetails.class);
		
		Date startDate = currentStateDetails.getStartDate();
		Date endDate = currentStateDetails.getEndDate();
		
		FhirContext context = Mockito.mock(FhirContext.class);
		IGenericClient client = Mockito.mock(IGenericClient.class);

		when(fhirContextInitializer.getFhirContext(currentStateDetails.getFhirVersion())).thenReturn(context);

		when(fhirContextInitializer.createClient(context, currentStateDetails.getEhrServerURL(),
				currentStateDetails.getAccessToken())).thenReturn(client);

		Encounter encounter = new Encounter();

		encounter.setPeriod(new PeriodDt());

		when(fhirContextInitializer.getResouceById(currentStateDetails, client,
				context, "Encounter", currentStateDetails.getEncounterId())).thenReturn(encounter);
		
		launchController.setStartAndEndDates(clientDetails, currentStateDetails);
		
		assertNotEquals(startDate, currentStateDetails.getStartDate());
		assertNotEquals(endDate, currentStateDetails.getEndDate());
		
	}
	
	@Test
	public void testStartEndDateR4() throws JsonParseException, JsonMappingException, IOException {

		LaunchDetails currentStateDetails = mapper.readValue(
				this.getClass().getClassLoader().getResourceAsStream("launchDetails.json"), LaunchDetails.class);
		ClientDetails clientDetails = mapper.readValue(
				this.getClass().getClassLoader().getResourceAsStream("saveClientDataEntry1.json"), ClientDetails.class);
		currentStateDetails.setFhirVersion("R4");
		
		Date startDate = currentStateDetails.getStartDate();
		Date endDate = currentStateDetails.getEndDate();
		
		FhirContext context = Mockito.mock(FhirContext.class);
		IGenericClient client = Mockito.mock(IGenericClient.class);

		when(fhirContextInitializer.getFhirContext(currentStateDetails.getFhirVersion())).thenReturn(context);

		when(fhirContextInitializer.createClient(context, currentStateDetails.getEhrServerURL(),
				currentStateDetails.getAccessToken())).thenReturn(client);

		org.hl7.fhir.r4.model.Encounter r4Encounter = new org.hl7.fhir.r4.model.Encounter();

		r4Encounter.setPeriod(new Period());

		when(fhirContextInitializer.getResouceById(currentStateDetails, client,
				context, "Encounter", currentStateDetails.getEncounterId())).thenReturn(r4Encounter);
		
		launchController.setStartAndEndDates(clientDetails, currentStateDetails);
		
		assertNotEquals(startDate, currentStateDetails.getStartDate());
		assertNotEquals(endDate, currentStateDetails.getEndDate());
		
	}

}
