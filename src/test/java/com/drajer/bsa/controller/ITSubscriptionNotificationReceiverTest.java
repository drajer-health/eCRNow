package com.drajer.bsa.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.scheduler.ScheduleJobConfiguration;
import com.drajer.bsa.service.SubscriptionNotificationReceiver;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.test.BaseIntegrationTest;
import com.drajer.test.util.WireMockHelper;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.github.tomakehurst.wiremock.verification.LoggedRequest;

import static com.github.tomakehurst.wiremock.client.WireMock.*;

import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CapabilityStatement;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.MeasureReport;
import org.hl7.fhir.r4.model.MeasureReport.MeasureReportGroupPopulationComponent;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.util.BundleUtil;

@ContextConfiguration(classes = ScheduleJobConfiguration.class)
@RunWith(Parameterized.class)
@TestPropertySource(properties = { "ignore.timers=true", "kar.directory=src/test/resources/Bsa/Scenarios/kars/diabetes",
    "report-submission.endpoint=http://localhost:9010/fhir", "report-validator.endpoint=" })
public class ITSubscriptionNotificationReceiverTest extends BaseIntegrationTest {
  protected FhirContext fhirContext = FhirContext.forCached(FhirVersionEnum.R4);

  private TestCaseInfo testCaseInfo;

  public ITSubscriptionNotificationReceiverTest(TestCaseInfo testCaseInfo) {
    this.testCaseInfo = testCaseInfo;
  }

  private static Logger logger = LoggerFactory.getLogger(ITSubscriptionNotificationReceiverTest.class);

  @Autowired
  SubscriptionNotificationReceiver notificationReceiver;

  @Autowired
  ApplicationUtils ap;

  @Autowired
  HealthcareSettingsDao hsDao;

  private ClassLoader classLoader = getClass().getClassLoader();

  private WireMockHelper stubHelper;

  // This generates a list of "TestCaseInfos" that describe the scenario
  // Comment out lines that don't work to temporarily disable test cases.
  // for eCSD tests the main points are
  // 1. Whether or not a reporting Bundle is generated (i.e. initial-pop = true)
  // 2. The Bundle contains a Measure report (TODO: validate that the other resources are present)
  // 3. The MeasureReport has the correct info for the test case
  @Parameters(name = "{0}")
  public static Collection<TestCaseInfo> data() {
    return Arrays.asList(new TestCaseInfo("ChronicDSDiabetesPoorControl", "denom-3-CMS122-Patient", true, 1, 1, 0, 0),
        new TestCaseInfo("ChronicDSDiabetesPoorControl", "denom-CMS122-Patient", true, 1, 1, 0, 0));
  }

  @Before
  public void setupNotificationMocking() throws IOException {
    this.wireMockServer.resetAll();
    stubHelper = new WireMockHelper(wireMockServer, wireMockHttpPort);
    logger.info("Creating WireMock stubs..");
    stubHelper.mockFhirRead("/fhir/metadata", getCapabilityStatement());
    logger.info("Set up Test: {}", this.testCaseInfo.getName());
    setupHealthCareSettings();
    mockAccessToken();
    mockScenarioFolder(new File(classLoader
        .getResource("Bsa/Scenarios/" + this.testCaseInfo.getPlanDef() + "/" + this.testCaseInfo.getName()).getPath()));
  }

  @Test
  public void testScenarioAndValidate() throws InterruptedException {
    logger.info("Executing Test: {}", this.testCaseInfo.getName());
    
    Bundle bundle = getNotificationBundle(this.testCaseInfo.getPlanDef(), this.testCaseInfo.getName());
    this.stubHelper.mockProcessMessageBundle(bundle);

    removeFiles(this.testCaseInfo.getName(), this.testCaseInfo.getPlanDef());

    notificationReceiver.processNotification(bundle, mock(HttpServletRequest.class), mock(HttpServletResponse.class));

    // TODO: We need a processing complete signal that's not the file output.
    // Some tests should never generate a report
    // this.testCaseInfo.getShouldTrigger()
    while (true) {
      if (processingComplete(this.testCaseInfo.getName(), this.testCaseInfo.getPlanDef())) {
        break;
      } else {
        Thread.sleep(1000);
      }
    }

    MeasureReport report = this.getMeasureReport();
    validatePopulation(report, "initial-population", this.testCaseInfo.getInitialPopulation());
    validatePopulation(report, "denominator", this.testCaseInfo.getDenominator());
    validatePopulation(report, "numerator", this.testCaseInfo.getNumerator());
    Bundle eICR = this.getEicrBundle(this.testCaseInfo.getPlanDef());

    validateBundle(eICR);
  }

  File getMeasureReportFile() {
    return new File("target/output/karsMeasureReport_null.json");
  }

  File getEICRFile(String planDef) {
    return new File("target/output/karsBundle_eicr-report-" + planDef + ".json");
  }

  File getNotificationBundleFile(String patientId) {
    return new File("target/output/karsBundle_" + patientId + "-notification-bundle.json");
  }

  protected Boolean processingComplete(String patientId, String planDef) {
    File eICRFile = this.getEICRFile(planDef);
    if (eICRFile.exists()) {
      return true;
    }

    String processMessageUrl = "/fhir/$process-message";
    List<LoggedRequest> requests = wireMockServer.findAll(postRequestedFor(urlEqualTo(processMessageUrl)));

    if (requests.size() > 0) {
      return true;
    }

    return false;
  }

  protected void removeFiles(String patientId, String planDef) {
    File mr = this.getMeasureReportFile();
    if (mr.exists()) {
      mr.delete();
    }
    File notificationBundle = this.getNotificationBundleFile(patientId);
    if (notificationBundle.exists()) {
      notificationBundle.delete();
    }
    File eICRReport = this.getEICRFile(planDef);

    if (eICRReport.exists()) {
      eICRReport.delete();
    }
  }

  Bundle getEicrBundle(String planDef) {
    File eICRReport = new File("target/output/karsBundle_eicr-report-" + planDef + ".json");

    if (eICRReport.exists()) {
      try (FileInputStream fis = new FileInputStream(eICRReport)) {
        return (Bundle) this.fhirContext.newJsonParser().parseResource(fis);
      } catch (Exception e) {
        return null;
      }
    }

    return null;
  }

  MeasureReport getMeasureReport() {
    File mr = new File("target/output/karsMeasureReport_null.json");

    if (mr.exists()) {
      try (FileInputStream fis = new FileInputStream(mr)) {
        return (MeasureReport) this.fhirContext.newJsonParser().parseResource(fis);
      } catch (Exception e) {
        return null;
      }
    }

    return null;
  }

  private void mockScenarioFolder(File scenario) {
    if (scenario.isFile()) {
      throw new IllegalArgumentException(String.format("The scenario must be a folder: %s"));
    }

    Map<String, List<IBaseResource>> resourceMap = readScenarioFolder(scenario);
    if (!resourceMap.containsKey("Patient") || resourceMap.get("Patient").isEmpty()) {
      throw new IllegalArgumentException(
          String.format("No patient resource found for scenario: %s", scenario.getName()));
    }

    if (resourceMap.get("Patient").size() > 1) {
      throw new IllegalArgumentException(
          String.format("Multiple patient resources found for scenario: %s", scenario.getName()));
    }

    String patientId = resourceMap.get("Patient").get(0).getIdElement().getIdPart();

    for (Map.Entry<String, List<IBaseResource>> entry : resourceMap.entrySet()) {
      // Mock a search for all resources of a given type.
      String mockQueryString = String.format("/fhir/%s?patient=%s", entry.getKey(), patientId);
      stubHelper.mockFhirSearch(mockQueryString, entry.getValue());
      for (IBaseResource r : entry.getValue()) {
        // Mock a read for a specific instance of a resource
        String id = r.getIdElement().getIdPart();
        String queryString = String.format("/fhir/%s/%s", entry.getKey(), id);
        stubHelper.mockFhirRead(queryString, r);
      }
    }
  }

  private Map<String, List<IBaseResource>> readScenarioFolder(File scenario) {
    // TODO: Pass in the correct version
    IParser parser = FhirContext.forCached(FhirVersionEnum.R4).newJsonParser();

    HashMap<String, List<IBaseResource>> resources = new HashMap<>();

    // Filter subdirectories for now..
    List<File> files = Arrays.asList(scenario.listFiles()).stream().filter(x -> x.isFile())
        .collect(Collectors.toList());

    for (File f : files) {
      try {
        IBaseResource r = parser.parseResource(new FileInputStream(f));
        List<IBaseResource> resourceList = resources.computeIfAbsent(r.fhirType(), k -> new ArrayList<>());
        resourceList.add(r);
      } catch (Exception e) {
        logger.warn("Error reading resource: {}", f.getName());
      }
    }

    return resources;
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
    stubHelper.mockTokenResponse("/token",
        String.format("{ \"access_token\": \"%s\", \n\"expires_in\": \"%s\" }", accessToken, expireTime));
  }

  private Bundle getNotificationBundle(String planDef, String testCase) {
    java.net.URL url = ITSubscriptionNotificationReceiverTest.class.getClassLoader().getResource("Bsa/Scenarios/" + planDef + "/" + testCase);
    File rootFile = new File(url.getPath());

    File[] bundles = rootFile.listFiles((FilenameFilter)new WildcardFileFilter("*notification-bundle.json"));
    if (bundles == null || bundles.length == 0) {
      throw new RuntimeException("Did not find a notification bundle for test case: " + this.testCaseInfo.getName());
    }

    if (bundles.length > 1) {
      throw new RuntimeException("Found multiple notification bundles for test case: " + this.testCaseInfo.getName());
    }

    String absolutePath =  bundles[0].getAbsolutePath();
    return ap.readBundleFromFile(absolutePath);
  }

  public CapabilityStatement getCapabilityStatement() {
    CapabilityStatement metadata = new CapabilityStatement();
    metadata.setFhirVersion(Enumerations.FHIRVersion._4_0_1);
    return metadata;
  }

  protected void validatePopulation(MeasureReport report, String population, int count) {
    Optional<MeasureReportGroupPopulationComponent> pgc = report.getGroup().get(0).getPopulation().stream()
        .filter(x -> x.getCode().getCodingFirstRep().getCode().equals(population)).findFirst();
    if (!pgc.isPresent()) {
      throw new RuntimeException(String.format("MeasureReport missing {} population", population));
    }

    assertEquals(count, pgc.get().getCount());
  }

  protected void validateBundle(Bundle bundle) {
    List<MeasureReport> mrs = BundleUtil.toListOfResourcesOfType(this.fhirContext, bundle, MeasureReport.class);
    assertEquals("Did not find expected MeasureReport", 1, mrs.size());
  }

  // @Test
  // public void getNotificationContextDiabetesDenomExclCMS122Test() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/Diabetes/denomexcl-CMS122-Patient/denomexcl-CMS122-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesReportDenomExcInTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/Diabetes/diabetes-report-denom-exc-in/report-denom-exc-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesReportIpInTest() {
  // Bundle bundle =
  // getBundle("Bsa/Scenarios/Diabetes/diabetes-report-ip-in/report-ip-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesReportIpOutTest() {
  // Bundle bundle =
  // getBundle("Bsa/Scenarios/Diabetes/diabetes-report-ip-out/report-ip-out-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesReportNumerInTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/Diabetes/diabetes-report-numer-in/report-numer-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesReportNumerOutTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/Diabetes/diabetes-report-numer-out/report-numer-out-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesTriggerConditionInTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/Diabetes/diabetes-trigger-condition-in/trigger-condition-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesTriggerConditionMissingTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/Diabetes/diabetes-trigger-condition-missing/trigger-condition-missing-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesTriggerConditionOutTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/Diabetes/diabetes-trigger-condition-out/trigger-condition-out-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesTriggerInTest() {
  // Bundle bundle =
  // getBundle("Bsa/Scenarios/Diabetes/diabetes-trigger-in/trigger-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesTriggerOutTest() {
  // Bundle bundle =
  // getBundle("Bsa/Scenarios/Diabetes/diabetes-trigger-out/trigger-out-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // // SeenPatients

  // @Test
  // public void getNotificationContextIpSeenPatientsTest() {
  // Bundle bundle =
  // getBundle("Bsa/Scenarios/SeenPatients/ip-SeenPatients/ip-SeenPatients-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextSeenPatientsNoTriggerInTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/SeenPatients/seen-patients-no-trigger-in/seen-patients-no-trigger-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextSeenPatientsReportedIpMpInHighTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/SeenPatients/seen-patients-reported-ip-mp-in-high/seen-patients-reported-ip-mp-in-high-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextSeenPatientsReportedIpMpInLowTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/SeenPatients/seen-patients-reported-ip-mp-in-low/seen-patients-reported-ip-mp-in-low-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextSeenPatientsReportedIpMpInMidTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/SeenPatients/seen-patients-reported-ip-mp-in-mid/seen-patients-reported-ip-mp-in-mid-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextSeenPatientsReportedIpMpOutPostTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/SeenPatients/seen-patients-reported-ip-mp-out-post/seen-patients-reported-ip-mp-out-post-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextSeenPatientsReportedIpMpOutPriorTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/SeenPatients/seen-patients-reported-ip-mp-out-prior/seen-patients-reported-ip-mp-out-prior-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextSeenPatientsSDEInTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/SeenPatients/seen-patients-sde-in/seen-patients-sde-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextSeenPatientsTriggerInTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/SeenPatients/seen-patients-trigger-in/seen-patients-trigger-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // BloodPressure

  // @Test
  // public void getNotificationContextBPReportDenomExcInTest() {
  // mockScenarioFolder(
  // new
  // File(classLoader.getResource("Bsa/Scenarios/ControllingBloodPressure/bp-report-denom-exc-in").getPath()));
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/bp-report-denom-exc-in/bp-report-denom-exc-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextBPReportIpInTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/bp-report-ip-in/bp-report-ip-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextBPReportIpOutTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/bp-report-ip-out/bp-report-ip-out-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextBPReportNumerInTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/bp-report-numer-in/bp-report-numer-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextBPReportNumerOutTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/bp-report-numer-out/bp-report-numer-out-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextBPTriggerConditionInTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/bp-trigger-condition-in/bp-trigger-condition-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextBPTriggerConditionMissingTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/bp-trigger-condition-missing/bp-trigger-condition-missing-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextBPTriggerConditionOutTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/bp-trigger-condition-out/bp-trigger-condition-out-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextBPTriggerInTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/bp-trigger-in/bp-trigger-in-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextBPTriggerOutTest() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/bp-trigger-out/bp-trigger-out-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDenomEXM165Test() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/denom-EXM165/denom-EXM165-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextNumerEXM165Test() {
  // Bundle bundle = getBundle(
  // "Bsa/Scenarios/ControllingBloodPressure/numer-EXM165/numer-EXM165-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // // Diabetes

  // @Test
  // public void getNotificationContextDiabetesNumerCMS122Test() {
  // Bundle bundle =
  // getBundle("Bsa/Scenarios/Diabetes/numer-CMS122-Patient/numer-CMS122-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesNumerCMS122_2Test() {
  // Bundle bundle =
  // getBundle("Bsa/Scenarios/Diabetes/numer-CMS122-2-Patient/numer-CMS122-2-notification-bundle.json");

  // //
  // mockProcessMessageBundle(getBundle("Bsa/Scenarios/Diabetes/numer-CMS122-2-Patient/eicr-bundle.json"));

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }

  // @Test
  // public void getNotificationContextDiabetesDenomCMS122Test() {
  // Bundle bundle =
  // getBundle("Bsa/Scenarios/Diabetes/denom-CMS122-Patient/denom-CMS122-notification-bundle.json");

  // notificationReceiver.processNotification(bundle,
  // mock(HttpServletRequest.class), mock(HttpServletResponse.class));
  // }
}
