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
	
	@Test
	public void testCreateEicrAction() throws Exception {
		LaunchDetails launchDetails = new LaunchDetails();
		launchDetails.setClientId("d01295ef-5053-4333-b40f-fd44062c89f9");
		launchDetails.setEhrServerURL("https://fhir-ehr.sandboxcerner.com/dstu2/0b8a0111-e8e6-4c26-a91c-5069cbc6b1ca");
		launchDetails.setAuthUrl("https://authorization.sandboxcerner.com/tenants/0b8a0111-e8e6-4c26-a91c-5069cbc6b1ca/protocols/oauth2/profiles/smart-v1/personas/provider/authorize");
		launchDetails.setTokenUrl("https://authorization.sandboxcerner.com/tenants/0b8a0111-e8e6-4c26-a91c-5069cbc6b1ca/protocols/oauth2/profiles/smart-v1/token");
		launchDetails.setAccessToken("eyJraWQiOiIyMDIwLTA3LTIwVDA2OjE5OjI1LjIzNi5lYyIsInR5cCI6IkpXVCIsImFsZyI6IkVTMjU2In0.eyJzdWIiOiJwb3J0YWwiLCJ1cm46Y29tOmNlcm5lcjphdXRob3JpemF0aW9uOmNsYWltcyI6eyJ2ZXIiOiIxLjAiLCJlbmNvdW50ZXIiOiI0MDI3OTEyIiwidG50IjoiMGI4YTAxMTEtZThlNi00YzI2LWE5MWMtNTA2OWNiYzZiMWNhIiwiYXpzIjoibGF1bmNoIG9ubGluZV9hY2Nlc3MgcGF0aWVudFwvUGF0aWVudC5yZWFkIHBhdGllbnRcL0NvbmRpdGlvbi5yZWFkIHBhdGllbnRcL0VuY291bnRlci5yZWFkIHBhdGllbnRcL01lZGljYXRpb25BZG1pbmlzdHJhdGlvbi5yZWFkIHBhdGllbnRcL01lZGljYXRpb25PcmRlci5yZWFkIHBhdGllbnRcL01lZGljYXRpb25TdGF0ZW1lbnQucmVhZCBwYXRpZW50XC9PYnNlcnZhdGlvbi5yZWFkIHBhdGllbnRcL0ltbXVuaXphdGlvbi5yZWFkIHBhdGllbnRcL0RpYWdub3N0aWNSZXBvcnQucmVhZCIsInVzZXIiOiI0NDY0MDA3IiwicGF0aWVudCI6IjQzNDIwMDkifSwiYXpwIjoiZDAxMjk1ZWYtNTA1My00MzMzLWI0MGYtZmQ0NDA2MmM4OWY5IiwiaXNzIjoiaHR0cHM6XC9cL2F1dGhvcml6YXRpb24uc2FuZGJveGNlcm5lci5jb21cLyIsImV4cCI6MTU5NTMwNjQwMCwiaWF0IjoxNTk1MzA1ODAwLCJqdGkiOiI1YzM4ZDgwYy0xYmY3LTQyMzgtYjExNi0wNzkyYTY2MjY4NmIiLCJ1cm46Y2VybmVyOmF1dGhvcml6YXRpb246Y2xhaW1zOnZlcnNpb246MSI6eyJ2ZXIiOiIxLjAiLCJwcm9maWxlcyI6eyJtaWxsZW5uaXVtLXYxIjp7InBlcnNvbm5lbCI6IjQ0NjQwMDciLCJlbmNvdW50ZXIiOiI0MDI3OTEyIn0sInNtYXJ0LXYxIjp7InBhdGllbnRzIjpbIjQzNDIwMDkiXSwiYXpzIjoibGF1bmNoIG9ubGluZV9hY2Nlc3MgcGF0aWVudFwvUGF0aWVudC5yZWFkIHBhdGllbnRcL0NvbmRpdGlvbi5yZWFkIHBhdGllbnRcL0VuY291bnRlci5yZWFkIHBhdGllbnRcL01lZGljYXRpb25BZG1pbmlzdHJhdGlvbi5yZWFkIHBhdGllbnRcL01lZGljYXRpb25PcmRlci5yZWFkIHBhdGllbnRcL01lZGljYXRpb25TdGF0ZW1lbnQucmVhZCBwYXRpZW50XC9PYnNlcnZhdGlvbi5yZWFkIHBhdGllbnRcL0ltbXVuaXphdGlvbi5yZWFkIHBhdGllbnRcL0RpYWdub3N0aWNSZXBvcnQucmVhZCJ9fSwiY2xpZW50Ijp7Im5hbWUiOiJlQ1JOb3ctU01BUlQgb24gRkhJUiIsImlkIjoiZDAxMjk1ZWYtNTA1My00MzMzLWI0MGYtZmQ0NDA2MmM4OWY5In0sInVzZXIiOnsicHJpbmNpcGFsIjoicG9ydGFsIiwicGVyc29uYSI6InByb3ZpZGVyIiwiaWRzcCI6IjBiOGEwMTExLWU4ZTYtNGMyNi1hOTFjLTUwNjljYmM2YjFjYSIsInNlc3Npb25JZCI6ImE0NmU1NzFhLTA1NDItNGVhZC1hOTgwLTc4YzBjN2VmM2Y5NiIsInByaW5jaXBhbFR5cGUiOiJ1c2VybmFtZSIsInByaW5jaXBhbFVyaSI6Imh0dHBzOlwvXC9taWxsZW5uaWEuc2FuZGJveGNlcm5lci5jb21cL2luc3RhbmNlXC8wYjhhMDExMS1lOGU2LTRjMjYtYTkxYy01MDY5Y2JjNmIxY2FcL3ByaW5jaXBhbFwvMDAwMC4wMDAwLjAwNDQuMUQ4NyIsImlkc3BVcmkiOiJodHRwczpcL1wvbWlsbGVubmlhLnNhbmRib3hjZXJuZXIuY29tXC9hY2NvdW50c1wvZmhpcnBsYXkudGVtcF9yaG8uY2VybmVyYXNwLmNvbVwvMGI4YTAxMTEtZThlNi00YzI2LWE5MWMtNTA2OWNiYzZiMWNhXC9sb2dpbiJ9LCJ0ZW5hbnQiOiIwYjhhMDExMS1lOGU2LTRjMjYtYTkxYy01MDY5Y2JjNmIxY2EifX0.WQ6EXmPLiZkqUsbL8Y7AOk0buPV6ZULV_9BRZs4lw5Wx-MrQnOmikFJ0pSYaNVCzdUH_9o6bhvGzM59DsyF1iw");
		launchDetails.setUserId("4464007");
		launchDetails.setExpiry(570);
		launchDetails.setScope("launch,online_access,offline_access,user/Patient.read,user/Condition.read,user/Encounter.read,user/MedicationAdministration.read,user/MedicationOrder.read,user/MedicationStatement.read,user/Observation.read,user/Immunization.read,user/DiagnosticReport.read,user/Practitioner.read,patient/Patient.read,patient/Condition.read,patient/Encounter.read,patient/MedicationAdministration.read,patient/MedicationOrder.read,patient/MedicationStatement.read,patient/Observation.read,patient/Immunization.read,patient/DiagnosticReport.read");
		Date date = new Date();
		date.setDate(2020-07-21);
		date.setTime(04-30-00);
		launchDetails.setLastUpdated(date);
		Date date1 = new Date();
		date.setDate(2018-10-31);
		date.setTime(14-00-00);
		launchDetails.setStartDate(date1);
		Date date2 = new Date();
		date.setDate(2020-07-22);
		date.setTime(05-34-07);
		launchDetails.setEndDate(date2);
		launchDetails.setRefreshToken("eyJpZCI6ImEzZTBiM2RiLThlODYtNGU0Ni1hNDkzLWZjZjk5MDRhNzgyMSIsInNlY3JldCI6ImNhZTQzZmJkLTdjNmEtNDVhYS05MDM1LWRjZWIyYTdlMjgzYyIsInZlciI6IjEuMCIsInR5cGUiOiJvbmxpbmVfYWNjZXNzIiwicHJvZmlsZSI6InNtYXJ0LXYxIn0=");
		launchDetails.setLaunchPatientId("4342009");
		launchDetails.setFhirVersion("DSTU2");
		launchDetails.setEncounterId("4027912");
		launchDetails.setStatus("{\"patientId\":\"4342009\",\"encounterId\":\"4027912\",\"matchTriggerStatus\":{\"actionId\":\"match-trigger\",\"jobStatus\":\"COMPLETED\",\"triggerMatchStatus\":false,\"matchedCodes\":[]},\"createEicrStatus\":{\"actionId\":\"create-eicr\",\"jobStatus\":\"COMPLETED\",\"eicrCreated\":false,\"eICRId\":\"0\"},\"periodicUpdateStatus\":[{\"actionId\":\"periodic-update-eicr\",\"jobStatus\":\"NOT_STARTED\",\"eicrUpdated\":false,\"eICRId\":\"\"},{\"actionId\":\"periodic-update-eicr\",\"jobStatus\":\"NOT_STARTED\",\"eicrUpdated\":false,\"eICRId\":\"\"},{\"actionId\":\"periodic-update-eicr\",\"jobStatus\":\"NOT_STARTED\",\"eicrUpdated\":false,\"eICRId\":\"\"}],\"periodicUpdateJobStatus\":\"SCHEDULED\",\"closeOutEicrStatus\":{\"actionId\":\"close-out-eicr\",\"jobStatus\":\"COMPLETED\",\"eicrClosed\":false,\"eICRId\":\"0\"},\"validateEicrStatus\":[],\"submitEicrStatus\":[],\"rrStatus\":[],\"eicrsReadyForSubmission\":[],\"eicrsReadyForValidation\":[0],\"eicrsForRRCheck\":[]}");
		launchDetails.setAssigningAuthorityId("2.16.840.1.113883.1.1.1.1");
		launchDetails.setId(4342009+4027912);
		launchDetails.setVersionNumber("1");
		launchDetails.setDirectHost("ett.healthit.gov");
		launchDetails.setDirectUser("test@ett.healthit.gov");
		launchDetails.setDirectPwd("12345");
		launchDetails.setDirectRecipient("connectathon@aimsplatform.sandbox.dmhisp.com");
		launchDetails.setIsCovid(true);
		launchDetails.setLaunchId(null);
		launchDetails.setLaunchState(0);
		launchDetails.setRedirectURI(null);
		launchDetails.setAuthorizationCode(null);
		
		WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;
		
		createtEicrAction.execute(launchDetails, launchType);
	}
	/*
	 * In case invalid Object passed to Execute method,
	 */
	@Test(expected = RuntimeException.class)
	public void whenUnableToReadExecutionStateThrowJsonProcessingException() throws Exception {
		ObjectMapper mapper = new ObjectMapper();
		WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;
		
		createtEicrAction.execute(mapper, launchType);
		
	}
	
}

