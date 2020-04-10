package com.drajer.plandefinition.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.interceptor.BearerTokenAuthInterceptor;

@Configuration
@ComponentScan(basePackages = {"com.drajer.fhirclient"})
public class SpringConfiguration {
	
	public static final String ersdFhirBaseServer = "https://ersd.aimsplatform.org/api/fhir";
	
	public static final String authorizationToken = "d94874a5b6d848ae921e75b9bf202feb97905791ff890a6e189614053a8032c6f298662299dea42df6cef59fde";
	
	public static FhirContext ctx = FhirContext.forR4();
	
	@Bean(name="esrdGenericClient")
	public IGenericClient getEsrdFhirContext() {
		BearerTokenAuthInterceptor authInterceptor = new BearerTokenAuthInterceptor(authorizationToken);
		IGenericClient genericClient = ctx.newRestfulGenericClient(ersdFhirBaseServer);
		genericClient.registerInterceptor(authInterceptor);
		return genericClient;
	}
	
	@Bean(name="jsonParser")
	public IParser getEsrdJsonParser() {
		return ctx.newJsonParser().setPrettyPrint(true);
	}
	
	

}
