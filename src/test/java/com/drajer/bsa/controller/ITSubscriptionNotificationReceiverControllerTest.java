package com.drajer.bsa.controller;

import static com.github.tomakehurst.wiremock.client.WireMock.findAll;
import static com.github.tomakehurst.wiremock.client.WireMock.postRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;

import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.scheduler.ScheduleJobConfiguration;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.test.WireMockQuery;
import com.drajer.test.util.EicrValidation;
import com.drajer.test.util.TestDataGenerator;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.github.tomakehurst.wiremock.verification.LoggedRequest;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.ParserConfigurationException;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Parameters.ParametersParameterComponent;
import org.hl7.fhir.r4.model.Resource;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.test.context.ContextConfiguration;
import org.xml.sax.SAXException;

@ContextConfiguration(classes = ScheduleJobConfiguration.class)
@RunWith(Parameterized.class)
public class ITSubscriptionNotificationReceiverControllerTest extends WireMockQuery {

  protected String testCaseId;
  protected Map<String, String> testData;
  protected List<Map<String, String>> fieldsToValidate;

  public ITSubscriptionNotificationReceiverControllerTest(
      String testCaseId, Map<String, String> testData, List<Map<String, String>> validateFields) {
    this.testCaseId = testCaseId;
    this.testData = testData;
    this.fieldsToValidate = validateFields;
  }

  private Logger logger =
      LoggerFactory.getLogger(ITSubscriptionNotificationReceiverControllerTest.class);

  @Autowired SubscriptionNotificationReceiverController notificationController;

  @Autowired ApplicationUtils ap;

  @Autowired HealthcareSettingsDao hsDao;

  @Value("${test.diabetes.kar.directory}")
  String karDirectory;

  private ClassLoader classLoader = getClass().getClassLoader();

  @Parameterized.Parameters(name = "{0}")
  public static Collection<Object[]> data() {

    List<TestDataGenerator> testDataGenerator = new ArrayList<>();
    testDataGenerator.add(
        new TestDataGenerator("test-yaml/denom-CMS122-ObservationEICRValidation.yaml"));

    int totalTestCount = 0;
    for (TestDataGenerator testData : testDataGenerator) {
      totalTestCount = totalTestCount + testData.getAllTestCases().size();
    }

    Object[][] data = new Object[totalTestCount][3];

    int count = 0;
    for (TestDataGenerator testData : testDataGenerator) {
      Set<String> testCaseSet = testData.getAllTestCases();
      for (String testCase : testCaseSet) {
        data[count][0] = testCase;
        data[count][1] = testData.getTestCaseByID(testCase).getTestData();
        data[count][2] = testData.getValidate(testCase);
        count++;
      }
    }
    return Arrays.asList(data);
  }

  @Before
  public void setupNotificationMocking() throws IOException {
    logger.info("Executing Test: {}", testCaseId);
    setupHealthCareSettings();
    mockAccessToken();
    File bsaFile = new File(classLoader.getResource("Bsa").getPath());
    for (File file : bsaFile.listFiles()) {
      if (file.isDirectory() && file.getName().equals("Scenarios")) {
        mockScenarios(file.listFiles());
      }
    }
  }

  // BloodPressure

  @Test
  public void getNotificationContextBPReportDenomExcInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/bp-report-denom-exc-in/bp-report-denom-exc-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextBPReportIpInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/bp-report-ip-in/bp-report-ip-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextBPReportIpOutTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/bp-report-ip-out/bp-report-ip-out-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextBPReportNumerInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/bp-report-numer-in/bp-report-numer-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextBPReportNumerOutTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/bp-report-numer-out/bp-report-numer-out-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextBPTriggerConditionInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/bp-trigger-condition-in/bp-trigger-condition-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextBPTriggerConditionMissingTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/bp-trigger-condition-missing/bp-trigger-condition-missing-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextBPTriggerConditionOutTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/bp-trigger-condition-out/bp-trigger-condition-out-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextBPTriggerInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/bp-trigger-in/bp-trigger-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextBPTriggerOutTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/bp-trigger-out/bp-trigger-out-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDenomEXM165Test() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/denom-EXM165/denom-EXM165-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextNumerEXM165Test() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/ControllingBloodPressure/numer-EXM165/numer-EXM165-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  // Diabetes

  @Test
  public void getNotificationContextDiabetesNumerCMS122Test() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/numer-CMS122-Patient/numer-CMS122-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesNumerCMS122_2Test() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/numer-CMS122-2-Patient/numer-CMS122-2-notification-bundle.json");

    // mockProcessMessageBundle(getBundle("Bsa/Scenarios/Diabetes/numer-CMS122-2-Patient/eicr-bundle.json"));

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesDenomCMS122Test() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/denom-CMS122-Patient/denom-CMS122-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesDenomCMS122_3Test() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/denom-3-CMS122-Patient/denom-3-CMS122-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));

    String processMessageUrl = "/fhir/$process-message-bundle";

    List<LoggedRequest> requests = findAll(postRequestedFor(urlEqualTo(processMessageUrl)));
    for (LoggedRequest request : requests) {
      Bundle bundle = null;
      Parameters parameters =
          getFhirParser().parseResource(Parameters.class, request.getBodyAsString());
      for (ParametersParameterComponent parameter : parameters.getParameter()) {
        if (parameter.hasResource() && parameter.getResource().fhirType().equals("Bundle")) {
          bundle = (Bundle) parameter.getResource();
          Eicr eicr = new Eicr();
          eicr.setEicrData(getFhirContext().newXmlParser().encodeResourceToString(bundle));
          try {
            EicrValidation.validateEicrDocument(eicr.getEicrData(), testData, fieldsToValidate);
          } catch (ParserConfigurationException | SAXException | IOException e) {
            fail("Invalid Eicr");
          }
        }
      }
    }
  }

  @Test
  public void getNotificationContextDiabetesDenomExclCMS122Test() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/denomexcl-CMS122-Patient/denomexcl-CMS122-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesReportDenomExcInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/diabetes-report-denom-exc-in/report-denom-exc-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesReportIpInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/diabetes-report-ip-in/report-ip-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesReportIpOutTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/diabetes-report-ip-out/report-ip-out-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesReportNumerInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/diabetes-report-numer-in/report-numer-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesReportNumerOutTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/diabetes-report-numer-out/report-numer-out-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesTriggerConditionInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/diabetes-trigger-condition-in/trigger-condition-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesTriggerConditionMissingTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/diabetes-trigger-condition-missing/trigger-condition-missing-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesTriggerConditionOutTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/diabetes-trigger-condition-out/trigger-condition-out-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesTriggerInTest() {
    Bundle bund =
        getBundle("Bsa/Scenarios/Diabetes/diabetes-trigger-in/trigger-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextDiabetesTriggerOutTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/Diabetes/diabetes-trigger-out/trigger-out-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  // SeenPatients

  @Test
  public void getNotificationContextIpSeenPatientsTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/SeenPatients/ip-SeenPatients/ip-SeenPatients-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextSeenPatientsNoTriggerInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/SeenPatients/seen-patients-no-trigger-in/seen-patients-no-trigger-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextSeenPatientsReportedIpMpInHighTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/SeenPatients/seen-patients-reported-ip-mp-in-high/seen-patients-reported-ip-mp-in-high-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextSeenPatientsReportedIpMpInLowTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/SeenPatients/seen-patients-reported-ip-mp-in-low/seen-patients-reported-ip-mp-in-low-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextSeenPatientsReportedIpMpInMidTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/SeenPatients/seen-patients-reported-ip-mp-in-mid/seen-patients-reported-ip-mp-in-mid-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextSeenPatientsReportedIpMpOutPostTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/SeenPatients/seen-patients-reported-ip-mp-out-post/seen-patients-reported-ip-mp-out-post-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextSeenPatientsReportedIpMpOutPriorTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/SeenPatients/seen-patients-reported-ip-mp-out-prior/seen-patients-reported-ip-mp-out-prior-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextSeenPatientsSDEInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/SeenPatients/seen-patients-sde-in/seen-patients-sde-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  @Test
  public void getNotificationContextSeenPatientsTriggerInTest() {
    Bundle bund =
        getBundle(
            "Bsa/Scenarios/SeenPatients/seen-patients-trigger-in/seen-patients-trigger-in-notification-bundle.json");

    notificationController.processNotification(
        getFhirParser().encodeResourceToString(bund),
        mock(HttpServletRequest.class),
        mock(HttpServletResponse.class));
  }

  Map<String, List<Resource>> resourceMap = new HashMap<String, List<Resource>>();

  private void mockScenarios(File[] listFiles) {
    mockScenarios(listFiles, true);
  }

  private void mockScenarios(File[] scenarios, boolean isTopLevel) {
    for (File file : scenarios) {
      if (file.isDirectory()) {
        mockScenarios(file.listFiles(), false);
        if (isTopLevel) {
          try {
            mockSearchQuery();
          } catch (Exception e) {
            logger.debug(" Unable to mock Search Query: ::::{}", e);
          }
          resourceMap.clear();
        }
      } else if (file.isFile()) {
        try {
          mockResourceQuery(file);
        } catch (Exception e) {
          logger.debug(" Unable to mock Resource Query: ::::{}", e);
        }
      } else {
        logger.debug("Scenario file not found: " + file.getAbsolutePath());
      }
    }
  }

  private void mockResourceQuery(File resourceFile) {
    String resourceAbsolutePath = resourceFile.getAbsolutePath();
    IBaseResource resourceBase = ap.readResourceFromFile(resourceAbsolutePath);
    if (resourceBase == null || !(resourceBase instanceof Resource)) {
      logger.debug("Resource not found.");
    } else {
      Resource resource = (Resource) resourceBase;
      if (!resourceMap.containsKey(resource.fhirType())) {
        resourceMap.put(resource.fhirType(), Collections.singletonList(resource));
      } else {
        List<Resource> resourceList = new ArrayList<Resource>();
        resourceList.addAll(resourceMap.get(resource.fhirType()));
        resourceList.add(resource);
        resourceMap.put(resource.fhirType(), resourceList);
      }
      String resourceType = resource.fhirType();
      String id = resource.getIdElement().getIdPart();
      String queryString = String.format("/fhir/%s/%s", resourceType, id);
      mockFhirRead(queryString, resource);
    }
  }

  private void mockSearchQuery() {
    List<String> patientIds = new ArrayList<String>();
    List<Resource> patients = resourceMap.get("Patient");
    if (patients != null && !patients.isEmpty()) {
      patients.forEach(patient -> patientIds.add(patient.getIdElement().getIdPart()));
    }
    if (!patientIds.isEmpty()) {
      for (String patientId : patientIds) {
        resourceMap
            .entrySet()
            .forEach(
                entry -> {
                  List<Resource> resources = new ArrayList<Resource>();
                  entry
                      .getValue()
                      .forEach(
                          resource -> {
                            if (getFhirParser()
                                .encodeResourceToString(resource)
                                .contains(patientId)) {
                              resources.add(resource);
                            }
                          });
                  String mockQueryString =
                      String.format("/fhir/%s?patient=%s", entry.getKey(), patientId);
                  mockFhirSearch(mockQueryString, resources);
                });
      }
    }
  }

  private void setupHealthCareSettings() {
    String healthCareSettings = "Bsa/HealthCareSettings.json";
    File healthCareSettingsFile = new File(classLoader.getResource(healthCareSettings).getFile());
    HealthcareSetting hcs = null;
    try {
      hcs = mapper.readValue(healthCareSettingsFile, HealthcareSetting.class);
    } catch (JsonParseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (JsonMappingException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    if (hcs == null) {
      logger.debug("Health care settings not found: " + healthCareSettings);
    }

    hsDao.saveOrUpdate(hcs);
  }

  private void mockAccessToken() {
    String accessToken = "cb81ec9fa7d7605a060ffc756fc7d130";
    String expireTime = "3600";
    mockTokenResponse(
        "/token",
        String.format(
            "{ \"access_token\": \"%s\", \n\"expires_in\": \"%s\" }", accessToken, expireTime));
  }

  private Bundle getBundle(String notificationBundle) {
    File notificationFile = new File(classLoader.getResource(notificationBundle).getFile());
    String absolutePath = notificationFile.getAbsolutePath();
    return ap.readBundleFromFile(absolutePath);
  }
}
