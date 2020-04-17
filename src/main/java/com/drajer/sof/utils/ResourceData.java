package com.drajer.sof.utils;

import org.hl7.fhir.instance.model.api.IBaseResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.drajer.sof.model.LaunchDetails;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.interceptor.BearerTokenAuthInterceptor;

@Component
public class ResourceData {

	private static final String DSTU2 = "DSTU2";
	private static final String DSTU2_1 = "DSTU2_1";
	private static final String DSTU3 = "DSTU3";
	private static final String R4 = "R4";

	private final Logger logger = LoggerFactory.getLogger(ResourceData.class);

	/**
	 * Get FhirContext appropriate to fhirVersion
	 * 
	 * @param fhirVersion The FHIR Version to use, either as a fhir version or a
	 *                    package name.
	 * @return The appropriate FhirContext to use for the server
	 */
	public FhirContext getFhirContext(String fhirVersion) {
		switch (fhirVersion) {
		case DSTU2:
			return FhirContext.forDstu2();
		case DSTU2_1:
			return FhirContext.forDstu2_1();
		case DSTU3:
			return FhirContext.forDstu3();
		case R4:
			return FhirContext.forR4();
		default:
			return FhirContext.forDstu2();
		}
	}

	/**
	 * Creates a GenericClient with standard intercepters used throughout the
	 * services.
	 *
	 * @param url          the base URL of the FHIR server to connect to
	 * @param access_token the name of the key to use to generate the token
	 * @return a Generic Client
	 */
	protected IGenericClient createClient(FhirContext context, String url, String access_token) {
		logger.info("Initializing the Client");
		IGenericClient client = context.newRestfulGenericClient(url);
		// client.setLogRequestAndResponse(true);
		context.getRestfulClientFactory().setSocketTimeout(30 * 1000);
		client.registerInterceptor(new BearerTokenAuthInterceptor(access_token));
		logger.info("Initialized the Client");
		return client;
	}

	protected void getResouceById(LaunchDetails authDetails, IGenericClient genericClient, FhirContext context, String resourceName,
			String resourceId) throws Exception {
		try {
			logger.info("Getting " + resourceName + " data");
			IBaseResource resource = genericClient.read().resource(resourceName).withId(resourceId).execute();
			logger.info(resourceName + ":::::::::::::::::" + context.newJsonParser().encodeResourceToString(resource));
		} catch (Exception e) {
			throw new Exception("Error in getting " + resourceName + " resource by Patient Id: " + resourceId);
		}
	}

	protected void getResourceByPatientId(LaunchDetails authDetails, IGenericClient genericClient, FhirContext context,
			String resourceName) throws Exception {
		IBaseResource resource = null;
		String url = authDetails.getEhrServerURL() + "/" + resourceName + "?patient=" + authDetails.getLaunchPatientId();
		logger.info("Invoking url:::::::::::::::"+url);
		try {
			logger.info("Getting " + resourceName + " data using Patient Id: " + authDetails.getLaunchPatientId());
			if(authDetails.getFhirVersion().equalsIgnoreCase(DSTU2)) {
				resource = genericClient.search().byUrl(url).returnBundle(Bundle.class).execute();	
				Bundle bundle = (Bundle) resource;
				logger.info("Total No of " + resourceName + " received:::::::::::::::::" + bundle.getEntry().size());
			} else if(authDetails.getFhirVersion().equalsIgnoreCase(R4)) {
				resource = genericClient.search().byUrl(url).returnBundle(org.hl7.fhir.r4.model.Bundle.class).execute();
				org.hl7.fhir.r4.model.Bundle bundle = (org.hl7.fhir.r4.model.Bundle) resource;
				logger.info("Total No of " + resourceName + " received:::::::::::::::::" + bundle.getEntry().size());
			}
		} catch (Exception e) {
			throw new Exception(
					"Error in getting " + resourceName + " resource by Patient Id: " + authDetails.getLaunchPatientId());
		}
	}

}
