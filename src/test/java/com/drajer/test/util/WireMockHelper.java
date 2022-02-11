package com.drajer.test.util;

import static com.github.tomakehurst.wiremock.client.WireMock.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.context.api.BundleInclusionRule;
import ca.uhn.fhir.model.valueset.BundleTypeEnum;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.api.BundleLinks;
import ca.uhn.fhir.rest.api.IVersionSpecificBundleFactory;
import com.drajer.test.model.StubVO;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.MappingBuilder;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WireMockHelper {
  private String baseUrl = "/FHIR";
  private int wireMockPort;
  private FhirContext fhirContext;

  WireMockServer wireMockServer;

  private static final Logger logger = LoggerFactory.getLogger(WireMockHelper.class);

  public WireMockHelper(WireMockServer wireMockSvr, int port, FhirContext fhirContext) {
    this.wireMockServer = wireMockSvr;
    this.wireMockPort = port;
    this.fhirContext = fhirContext;
  }

  public WireMockHelper(WireMockServer wireMockSvr, int port) {
    this(wireMockSvr, port, FhirContext.forCached(FhirVersionEnum.R4));
  }

  public void stubResources(Map<String, ?> stubMapping) {

    Set<String> stubResources = stubMapping.keySet();

    for (String resource : stubResources) {

      stubMapping.get(resource);
      List<LinkedHashMap> stubVOs = (List<LinkedHashMap>) stubMapping.get(resource);

      for (LinkedHashMap map : stubVOs) {
        StubVO vo = new StubVO(map.get("params"), (String) map.get("responseFilePath"));
        callStubber(resource, vo);
      }
    }
  }

  private void callStubber(String resource, StubVO stubVO) {
    boolean isQueryParam = stubVO.getParams().getClass().getSimpleName().equals("LinkedHashMap");

    if (isQueryParam) {
      StringBuilder params = new StringBuilder("");
      Map<String, String> paramMap = (Map<String, String>) stubVO.getParams();
      Set<String> pramKeys = paramMap.keySet();
      int size = pramKeys.size();
      for (String key : pramKeys) {
        params.append(key).append("=").append(paramMap.get(key));
        size--;
        if (size > 0) params.append("&");
      }
      stubResource(resource, true, params.toString(), stubVO.getResponseFilePath());
    } else {
      stubResource(resource, false, (String) stubVO.getParams(), stubVO.getResponseFilePath());
    }
  }

  private void stubResource(
      String resourceName, boolean isQueryParam, String params, String responseFilePath) {
    String url = "";
    String param = "";

    if (isQueryParam) param = "?" + params;
    else param = "/" + params;

    url = baseUrl + "/" + resourceName + param;

    String responseStr = TestUtils.getFileContentAsString(responseFilePath);

    // Mapping for our resources
    logger.info("Creating wiremock stub for uri: {}", url);
    wireMockServer.stubFor(
        get(urlEqualTo(url))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withBody(responseStr)
                    .withHeader("Content-Type", "application/fhir+json; charset=utf-8")));
  }

  public void stubAuthAndMetadata(Map<String, ?> otherMappings) {

    Set<String> mappingKey = otherMappings.keySet();
    for (String key : mappingKey) {
      String url = "";
      if (key.equalsIgnoreCase("default")) {
        wireMockServer.stubFor(
            any(urlPathMatching(baseUrl + "/.*"))
                .atPriority(10)
                .willReturn(
                    aResponse()
                        .withStatus(404)
                        .withBody(TestUtils.getFileContentAsString((String) otherMappings.get(key)))
                        .withHeader("Content-Type", "application/fhir+json; charset=utf-8")));
        logger.info("Stub Created for default: /FHIR/.*");
      } else if (key.equalsIgnoreCase("metadata")) {
        url = baseUrl + "/" + key;
        // TODO - Change to pull token url from client detail instead of hard coding.
        String tokenUrl = "http://localhost:" + wireMockPort + "/FHIR/token";
        String response = TestUtils.getFileContentAsString((String) otherMappings.get(key));
        response = response.replace("Replace this with mock URL for test", tokenUrl);
        wireMockServer.stubFor(
            get(urlEqualTo(url))
                .atPriority(1)
                .willReturn(
                    aResponse()
                        .withStatus(200)
                        .withBody(response)
                        .withHeader("Content-Type", "application/fhir+json; charset=utf-8")));
        logger.info("Stub Created for Metadata uri: {}", url);
      } else if (key.equalsIgnoreCase("token")) {
        url = baseUrl + "/" + key;
        wireMockServer.stubFor(
            post(urlEqualTo(url))
                .atPriority(1)
                .willReturn(
                    aResponse()
                        .withStatus(200)
                        .withBody(TestUtils.getFileContentAsString((String) otherMappings.get(key)))
                        .withHeader("Content-Type", "application/fhir+json; charset=utf-8")));
        logger.info("Stub Created for AccessToken uri: {}", url);
      }
    }
  }

  public void mockFhirRead(IBaseResource resource) {
    String resourcePath = "/" + resource.fhirType() + "/" + resource.getIdElement().getIdPart();
    mockFhirInteraction(resourcePath, resource);
  }

  public void mockFhirRead(String path, IBaseResource resource) {
    mockFhirRead(path, resource, 200);
  }

  public void mockFhirRead(String path, IBaseResource resource, int statusCode) {
    MappingBuilder builder = get(urlEqualTo(path));
    mockFhirInteraction(builder, resource, statusCode);
  }

  public void mockFhirSearch(String path, List<IBaseResource> resources) {
    MappingBuilder builder = get(urlEqualTo(path));
    mockFhirInteraction(builder, makeBundle(resources));
  }

  public void mockFhirSearch(String path, IBaseResource... resources) {
    MappingBuilder builder = get(urlEqualTo(path));
    mockFhirInteraction(builder, makeBundle(resources));
  }

  public void mockFhirPost(String path, IBaseResource resource) {
    mockFhirInteraction(post(urlEqualTo(path)), resource, 200);
  }

  public void mockFhirInteraction(String path, IBaseResource resource) {
    mockFhirRead(path, resource, 200);
  }

  public void mockFhirInteraction(MappingBuilder builder, IBaseResource resource) {
    mockFhirInteraction(builder, resource, 200);
  }

  public void mockFhirInteraction(MappingBuilder builder, IBaseResource resource, int statusCode) {
    String body = null;
    if (resource != null) {
      body = this.getFhirParser().encodeResourceToString(resource);
    }

    wireMockServer.stubFor(
        builder.willReturn(
            aResponse()
                .withStatus(statusCode)
                .withHeader("Content-Type", "application/json")
                .withBody(body)));
  }

  public void mockTokenResponse(String path, String body) {
    MappingBuilder builder = post(urlEqualTo(path));
    wireMockServer.stubFor(
        builder.willReturn(
            aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(body)));
  }

  public void mockProcessMessageBundle(IBaseBundle bundle) {
    String path = "/fhir/$process-message";
    MappingBuilder builder = post(urlEqualTo(path));
    wireMockServer.stubFor(
        builder.willReturn(
            aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(getFhirParser().encodeResourceToString(bundle))));
  }

  public IBaseBundle makeBundle(List<? extends IBaseResource> resources) {
    return makeBundle(resources.toArray(new IBaseResource[resources.size()]));
  }

  public IBaseBundle makeBundle(IBaseResource... resources) {
    IVersionSpecificBundleFactory bf = this.fhirContext.newBundleFactory();
    bf.addRootPropertiesToBundle(
        UUID.randomUUID().toString(),
        new BundleLinks(
            "http://localhost:" + wireMockPort + "/" + baseUrl,
            null,
            true,
            BundleTypeEnum.SEARCHSET),
        resources.length,
        null);
    bf.addResourcesToBundle(
        Arrays.asList(resources),
        BundleTypeEnum.SEARCHSET,
        "http://localhost:" + wireMockPort + "/" + baseUrl,
        BundleInclusionRule.BASED_ON_INCLUDES,
        null);
    return (IBaseBundle) bf.getResourceBundle();
  }

  protected IParser getFhirParser() {
    return this.fhirContext.newJsonParser();
  }
}
