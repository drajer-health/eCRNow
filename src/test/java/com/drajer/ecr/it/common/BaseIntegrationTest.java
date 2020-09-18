package com.drajer.ecr.it.common;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.any;
import static com.github.tomakehurst.wiremock.client.WireMock.configureFor;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.UrlMatchingStrategy;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.commons.io.IOUtils;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.junit.After;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpHeaders;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@ContextConfiguration(classes = SpringConfiguration.class)
@AutoConfigureMockMvc
@Transactional
@ActiveProfiles("test")
public abstract class BaseIntegrationTest {

  private static final Logger logger = LoggerFactory.getLogger(BaseIntegrationTest.class);

  @LocalServerPort protected int port;

  @Autowired protected MockMvc mockMvc;

  @Autowired protected SessionFactory sessionFactory;
  protected Session session = null;
  protected Transaction tx = null;

  protected TestRestTemplate restTemplate = new TestRestTemplate();
  protected HttpHeaders headers = new HttpHeaders();

  protected static ObjectMapper mapper = new ObjectMapper();

  protected static final String URL = "http://localhost:";
  protected String baseUrl = "/FHIR";

  protected int wireMockHttpPort;
  protected int wireMockHttpsPort;

  protected static String testSaveLaunchData;
  protected static String launchDetailString;
  protected static int savedLaunchId;
  protected static int testLaunchDetailsId;

  protected static int savedClientId;
  protected static String clientDetailString;
  protected static String testSaveClientData;

  protected static String systemLaunchInputData;

  protected static int testClientDetailsId;

  protected static List<ClientDetails> deleteClientList = new ArrayList<>();

  protected static List<LaunchDetails> deleteLaunchList = new ArrayList<>();

  protected ClassLoader classLoader = this.getClass().getClassLoader();

  WireMockServer wireMockServer;

  @Before
  public void setUp() throws IOException {

    session = sessionFactory.openSession();

    setUpWireMockServer();
    stubAuthAndMetadata();
    stubDefaultResponse();
  }

  @After
  public void tearDown() {

    session.close();
    wireMockServer.stop();
  }

  /*
   * Usage: ResourceName eg Patient, Encounter isQueryParam true for queryparam
   * params key value pairs isR4 true for r4, false for dstu2
   * responseFilePath-filepath+filename
   */
  protected void stubResource(
      String resourceName, boolean isQueryParam, String params, String responseFilePath)
      throws IOException {
    String url = "";
    String param = "";

    if (isQueryParam) param = "?" + params;
    else param = "/" + params;

    url = baseUrl + "/" + resourceName + param;
    // url=URLEncoder.encode( url, "UTF-8" );

    String responseStr = getFileContentAsString(responseFilePath);

    // Mapping for our resources
    logger.info("Creating wiremock stub for uri: " + url);
    stubFor(
        get(urlEqualTo(url))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withBody(responseStr)
                    .withHeader("Content-Type", "application/fhir+json; charset=utf-8")));
  }

  private void stubDefaultResponse() {
    // DefaultMapping
    UrlMatchingStrategy urlMatchingStrategy = new UrlMatchingStrategy();
    urlMatchingStrategy.setUrlPattern(baseUrl + "/.*");
    stubFor(
        any(urlMatchingStrategy)
            .atPriority(10)
            .willReturn(
                aResponse()
                    .withStatus(404)
                    .withBody(getFileContentAsString("R4/DefaultResponse/NoDataFound_Default.json"))
                    .withHeader("Content-Type", "application/fhir+json; charset=utf-8")));
    logger.info("Stub Created for default: " + "/FHIR/.*");
  }

  // the file should be in test/resources folder, or the folderpath with filename
  protected String getFileContentAsString(String fileName) {
    String fileContent = "";
    InputStream stream = classLoader.getResourceAsStream(fileName);
    StringWriter writer = new StringWriter();
    try {
      IOUtils.copy(stream, writer, StandardCharsets.UTF_8);
      fileContent = writer.toString();
      stream.close();
      writer.close();
    } catch (Exception e) {
      logger.error("File not found::" + fileName);
    }

    return fileContent;
  }

  protected Document getExpectedXml(String expectedXml)
      throws ParserConfigurationException, SAXException, IOException {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document document = builder.parse(classLoader.getResourceAsStream(expectedXml));
    return document;
  }

  protected void setUpWireMockServer() {
    wireMockHttpPort = port + 1;
    wireMockHttpsPort = port + 2;
    try {
      wireMockServer = new WireMockServer(wireMockHttpPort, wireMockHttpsPort);

      wireMockServer.start();
      configureFor("localhost", wireMockHttpPort);
      logger.info("Initialized wiremock on http server: " + wireMockHttpPort);
    } catch (Exception e) {
      logger.error("Error in initializing wiremock" + e);
    }
  }

  protected void getSystemLaunchInputData(String systemLaunchFile) throws IOException {
    systemLaunchInputData = getFileContentAsString(systemLaunchFile);
    systemLaunchInputData = systemLaunchInputData.replace("port", "" + wireMockHttpPort);
  }

  protected void createTestLaunchDetailsInDB(String launchDetailsFile) throws IOException {
    launchDetailString = getFileContentAsString(launchDetailsFile);

    testLaunchDetailsId =
        (int) session.save(mapper.readValue(launchDetailString, LaunchDetails.class));

    LaunchDetails launchDetailsToBeDeleted =
        (LaunchDetails) session.get(LaunchDetails.class, testLaunchDetailsId);
    deleteLaunchList.add(launchDetailsToBeDeleted);
  }

  protected void createTestClientDetailsInDB(String clientDetailsFile) throws IOException {

    // clientDetailString = testDataGenerator.getTestDataAsString(0);
    clientDetailString = getFileContentAsString(clientDetailsFile);
    ClientDetails clientDetails = mapper.readValue(clientDetailString, ClientDetails.class);

    String fhirServerBaseURL =
        clientDetails.getFhirServerBaseURL().replace("port", "" + wireMockHttpPort);
    clientDetails.setFhirServerBaseURL(fhirServerBaseURL);
    String tokenURL = clientDetails.getTokenURL().replace("port", "" + wireMockHttpPort);
    clientDetails.setTokenURL(tokenURL);

    testClientDetailsId = (int) session.save(clientDetails);

    ClientDetails clientDetailsToBeDeleted =
        (ClientDetails) session.get(ClientDetails.class, testClientDetailsId);
    deleteClientList.add(clientDetailsToBeDeleted);
  }

  protected void dataCleanup() {
    for (LaunchDetails launchDetails : deleteLaunchList) {
      session.load(LaunchDetails.class, launchDetails.getId());
      session.delete(launchDetails);
    }
    deleteLaunchList.clear();

    for (ClientDetails clientDetails : deleteClientList) {
      session.load(ClientDetails.class, clientDetails.getId());
      session.delete(clientDetails);
    }
    deleteClientList.clear();
  }

  protected String createURLWithPort(String uri) {
    return URL + port + uri;
  }

  private void stubAuthAndMetadata() throws IOException {
    stubFor(
        get(urlEqualTo(baseUrl + "/metadata"))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withBody(getFileContentAsString("R4/Misc/MetaData_r4.json"))
                    .withHeader("Content-Type", "application/fhir+json; charset=utf-8")));
    logger.info("Stub Created for Metadata uri: " + baseUrl + "/metadata");

    stubFor(
        post(urlEqualTo(baseUrl + "/token"))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withBody(getFileContentAsString("R4/Misc/AccessToken.json"))
                    .withHeader("Content-Type", "application/json")));
    logger.info("Stub Created for AsscessToken uri: " + baseUrl + "/token");
  }
}
