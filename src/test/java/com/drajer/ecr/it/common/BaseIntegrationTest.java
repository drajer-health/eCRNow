package com.drajer.ecr.it.common;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.sof.model.ClientDetails;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.junit.After;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.rules.SpringClassRule;
import org.springframework.test.context.junit4.rules.SpringMethodRule;
import org.springframework.transaction.annotation.Transactional;

@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@ContextConfiguration(classes = SpringConfiguration.class)
@DirtiesContext(classMode = ClassMode.BEFORE_EACH_TEST_METHOD)
@AutoConfigureMockMvc
@Transactional
@ActiveProfiles("test")
public abstract class BaseIntegrationTest {

  private static final Logger logger = LoggerFactory.getLogger(BaseIntegrationTest.class);

  @ClassRule public static final SpringClassRule SPRING_CLASS_RULE = new SpringClassRule();

  @Rule public final SpringMethodRule springMethodRule = new SpringMethodRule();

  @LocalServerPort protected int port;

  @Autowired protected SessionFactory sessionFactory;
  protected Session session = null;
  protected Transaction tx = null;

  protected TestRestTemplate restTemplate = new TestRestTemplate();
  protected HttpHeaders headers = new HttpHeaders();

  protected static ObjectMapper mapper = new ObjectMapper();

  protected static final String URL = "http://localhost:";
  protected String baseUrl = "/FHIR";

  protected ClassLoader classLoader = this.getClass().getClassLoader();

  protected int wireMockHttpPort;

  protected ClientDetails clientDetails;

  @Before
  public void setUp() throws IOException {
    wireMockHttpPort = port + 1;
    session = sessionFactory.openSession();
  }

  @After
  public void tearDown() {
    if (session != null) {
      session.close();
    }
  }

  protected String getSystemLaunchInputData(String systemLaunchFile) throws IOException {
    String systemLaunchInputData = TestUtils.getFileContentAsString(systemLaunchFile);
    systemLaunchInputData = systemLaunchInputData.replace("port", "" + wireMockHttpPort);
    return systemLaunchInputData;
  }

  protected void createClientDetails(String clientDetailsFile) throws IOException {

    String clientDetailString = TestUtils.getFileContentAsString(clientDetailsFile);
    clientDetailString = clientDetailString.replace("port", "" + wireMockHttpPort);

    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> entity = new HttpEntity<String>(clientDetailString, headers);
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort("/api/clientDetails"), HttpMethod.POST, entity, String.class);
    clientDetails = mapper.readValue(response.getBody(), ClientDetails.class);

    assertEquals("Failed to save client details", HttpStatus.OK, response.getStatusCode());
  }

  protected String createURLWithPort(String uri) {
    return URL + port + uri;
  }
}
