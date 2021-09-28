package com.drajer.test;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.interceptor.LoggingInterceptor;
import com.github.tomakehurst.wiremock.client.MappingBuilder;
import com.github.tomakehurst.wiremock.client.WireMock;
import java.util.List;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CapabilityStatement;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.Resource;
import org.junit.Before;
import org.junit.jupiter.api.AfterAll;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class WireMockQuery extends BaseIntegrationTest {

  private static final Logger logger = LoggerFactory.getLogger(WireMockQuery.class);

  private static FhirContext FHIR_CONTEXT = FhirContext.forCached(FhirVersionEnum.R4);
  private static IParser FHIR_PARSER = FHIR_CONTEXT.newJsonParser().setPrettyPrint(true);

  WireMock wireMock;

  @Before
  public void start() {
    if (wireMockServer.isRunning()) {
      wireMockServer.stop();
      logger.debug("WireMock server stopped.");
    }
    wireMockServer.start();
    logger.debug("WireMock server started.");
    WireMock.configureFor("localhost", wireMockHttpPort);
    wireMock = new WireMock("localhost", wireMockHttpPort);

    mockFhirRead("/fhir/metadata", getCapabilityStatement());
  }

  @AfterAll
  public void stop() {
    if (wireMock != null) {
      wireMockServer.stop();
      logger.debug("WireMock server stopped.");
    } else {
      logger.debug("WireMock server never started.");
    }
  }

  public FhirContext getFhirContext() {
    return FHIR_CONTEXT;
  }

  public IParser getFhirParser() {
    return FHIR_PARSER;
  }

  public IGenericClient newClient() {
    IGenericClient client =
        getFhirContext()
            .newRestfulGenericClient(String.format("http://localhost:%d/", wireMockHttpPort));

    LoggingInterceptor logger = new LoggingInterceptor();
    logger.setLogRequestSummary(true);
    logger.setLogResponseBody(true);
    client.registerInterceptor(logger);

    return client;
  }

  public void mockFhirRead(Resource resource) {
    String resourcePath = "/" + resource.fhirType() + "/" + resource.getId();
    mockFhirInteraction(resourcePath, resource);
  }

  public void mockFhirRead(String path, Resource resource) {
    mockFhirRead(path, resource, 200);
  }

  public void mockFhirRead(String path, Resource resource, int statusCode) {
    MappingBuilder builder = get(urlEqualTo(path));
    mockFhirInteraction(builder, resource, statusCode);
  }

  public void mockFhirSearch(String path, List<Resource> resources) {
    MappingBuilder builder = get(urlEqualTo(path));
    mockFhirInteraction(builder, makeBundle(resources));
  }

  public void mockFhirSearch(String path, Resource... resources) {
    MappingBuilder builder = get(urlEqualTo(path));
    mockFhirInteraction(builder, makeBundle(resources));
  }

  public void mockFhirPost(String path, Resource resource) {
    mockFhirInteraction(post(urlEqualTo(path)), resource, 200);
  }

  public void mockFhirInteraction(String path, Resource resource) {
    mockFhirRead(path, resource, 200);
  }

  public void mockFhirInteraction(MappingBuilder builder, Resource resource) {
    mockFhirInteraction(builder, resource, 200);
  }

  public void mockFhirInteraction(MappingBuilder builder, Resource resource, int statusCode) {
    String body = null;
    if (resource != null) {
      body = getFhirParser().encodeResourceToString(resource);
    }

    stubFor(
        builder.willReturn(
            aResponse()
                .withStatus(statusCode)
                .withHeader("Content-Type", "application/json")
                .withBody(body)));
  }

  public void mockTokenResponse(String path, String body) {
    MappingBuilder builder = post(urlEqualTo(path));
    stubFor(
        builder.willReturn(
            aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(body)));
  }

  public void mockProcessMessageBundle(Bundle bundle) {
    // $process-message-bundle
    String path = "/fhir/process-message-bundle";
    MappingBuilder builder = post(urlEqualTo(path));
    stubFor(
        builder.willReturn(
            aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(getFhirParser().encodeResourceToString(bundle))));
  }

  public CapabilityStatement getCapabilityStatement() {
    CapabilityStatement metadata = new CapabilityStatement();
    metadata.setFhirVersion(Enumerations.FHIRVersion._4_0_1);
    return metadata;
  }

  public Bundle makeBundle(List<? extends Resource> resources) {
    return makeBundle(resources.toArray(new Resource[resources.size()]));
  }

  public Bundle makeBundle(Resource... resources) {
    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.SEARCHSET);
    bundle.setTotal(resources != null ? resources.length : 0);
    if (resources != null) {
      for (Resource l : resources) {
        bundle.addEntry().setResource(l).setFullUrl("/" + l.fhirType() + "/" + l.getId());
      }
    }
    return bundle;
  }
}
