package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.ehr.service.impl.EhrFhirR4QueryServiceImpl;
import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.BsaTypes.OutputContentType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.OperationOutcome;
import org.hl7.fhir.r4.model.OperationOutcome.IssueSeverity;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.UriType;
import org.junit.Before;
import org.junit.Test;
import org.springframework.core.io.ClassPathResource;
import org.springframework.web.client.RestTemplate;

public class ValidateReportTest {

  private static final String XSD_PATH =
      "src/test/resources/AppData/Schema/infrastructure/cda/CDA_SDTC.xsd";
  private static final String R11_SCHEMATRON_PATH =
      "src/test/resources/AppData/schematrons/CdaR11/CDAR2_IG_PHCASERPT_R2_STU1.1_SCHEMATRON.sch";
  private static final String R31_SCHEMATRON_PATH =
      "src/test/resources/AppData/schematrons/CdaR31/CDAR2_IG_PHCASERPT_R2_D3_SCHEMATRON.sch";

  private final FhirContext r4Context = FhirContext.forR4();
  private KarProcessingData karProcessingData;
  private EhrQueryService ehrQueryService;
  private ValidateReport validateReport;

  @Before
  public void setUp() {
    validateReport = new ValidateReport();
    ehrQueryService = new EhrFhirR4QueryServiceImpl();
    karProcessingData = new KarProcessingData();

    NotificationContext notificationContext = getNotificationContext();
    karProcessingData.setNotificationContext(notificationContext);
    karProcessingData.setHealthcareSetting(getHealthcareSetting());
    karProcessingData.setxRequestId("32");
    karProcessingData.setxCorrelationId("corr-1");
    karProcessingData.setNotificationBundle(
        (Bundle)
            r4Context.newJsonParser().parseResource(notificationContext.getNotificationData()));
    karProcessingData.setKar(getKnowledgeArtifact());
    karProcessingData.setExecutionSequenceId("seq-1");

    ActionRepo.getInstance().setXsdSchemasLocation(XSD_PATH);

    validateReport.setActionId("validate-action");
    validateReport.setJsonParser(r4Context.newJsonParser());
    validateReport.setRestTemplate(new RestTemplate());
    validateReport.setEicrR11SchematronPath(R11_SCHEMATRON_PATH);
    validateReport.setEicrR31SchematronPath(R31_SCHEMATRON_PATH);
    validateReport.setInputData(getDataRequirements());
    validateReport.setOutputData(getDataRequirements());
    validateReport.setInputDataIdToRelatedDataIdMap(getInputRelatedData());
  }

  @Test
  public void testProcessAndValidateCdaOutputs() {
    applyOutputFormat(OutputContentType.CDA_R11);
    validateReport.setValidateEicrR11Data(true);
    validateReport.setValidateEicrR31Data(false);

    List<TimingSchedule> timing = new ArrayList<>();
    timing.add(new TimingSchedule());
    validateReport.setTimingData(timing);
    validateReport.setIgnoreTimers(false);

    BsaActionStatus scheduled = validateReport.process(karProcessingData, ehrQueryService);
    assertEquals(BsaActionStatusType.SCHEDULED, scheduled.getActionStatus());

    validateReport.setTimingData(new ArrayList<>());
    validateReport.setIgnoreTimers(true);
    karProcessingData.setSubmittedCdaData(TestUtils.getFileContentAsString("DSTU2/Misc/CDA.xml"));

    BsaActionStatus completed = validateReport.process(karProcessingData, ehrQueryService);
    assertEquals(BsaActionStatusType.COMPLETED, completed.getActionStatus());
    assertTrue(karProcessingData.getActionStatus().containsKey("seq-1"));

    boolean r11Result =
        validateReport.validateCdaOutput(
            karProcessingData, new ValidateReportStatus(), OutputContentType.CDA_R11);
    assertTrue(r11Result);

    boolean r31Result =
        validateReport.validateCdaOutput(
            karProcessingData, new ValidateReportStatus(), OutputContentType.CDA_R31);
    assertTrue(r31Result);

    assertEquals(R11_SCHEMATRON_PATH, validateReport.getEicrR11SchematronPath());
    assertEquals(R31_SCHEMATRON_PATH, validateReport.getEicrR31SchematronPath());
    assertTrue(validateReport.getValidateEicrR11Data());
  }

  @Test
  public void testValidateFhirOutputWithValidatorResponses() throws Exception {
    applyOutputFormat(OutputContentType.FHIR);
    populateActionOutputDataById();
    validateReport.setValidateEicrFhirData(true);

    String successResponse = buildOutcomeJson(IssueSeverity.INFORMATION);
    String errorResponse = buildOutcomeJson(IssueSeverity.ERROR);
    HttpServer server = startValidatorServer(successResponse, errorResponse);
    try {
      String baseUrl = "http://localhost:" + server.getAddress().getPort();
      validateReport.setValidatorEndpoint(baseUrl + "/success");
      BsaActionStatus completed = validateReport.process(karProcessingData, ehrQueryService);
      assertEquals(BsaActionStatusType.COMPLETED, completed.getActionStatus());
      assertTrue(karProcessingData.getActionOutputDataById().containsKey("patient"));
      assertEquals(2, karProcessingData.getActionOutputData().get("validate-action").size());

      validateReport.setValidatorEndpoint(baseUrl + "/error");
      ValidateReportStatus status = new ValidateReportStatus();
      validateReport.validateFhirOutput(karProcessingData, status);
      assertFalse(BsaActionStatusType.FAILED.equals(status.getActionStatus()));
      assertTrue(validateReport.getValidateEicrFhirData());
      assertEquals(baseUrl + "/error", validateReport.getValidatorEndpoint());
    } finally {
      server.stop(0);
    }
  }

  private void populateActionOutputDataById() {
    HashMap<String, Set<org.hl7.fhir.r4.model.Resource>> outputDataById = new HashMap<>();
    Set<org.hl7.fhir.r4.model.Resource> patientSet = new HashSet<>();
    Patient patient = TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    if (!patient.hasId()) {
      patient.setId("patient-1");
    }
    patientSet.add(patient);
    outputDataById.put("patient", patientSet);

    Set<org.hl7.fhir.r4.model.Resource> encounterSet = new HashSet<>();
    Encounter encounter =
        TestUtils.loadResourceDataFromFile(Encounter.class, "R4/Encounter/Encounter_97953900.json");
    if (!encounter.hasId()) {
      encounter.setId("encounter-1");
    }
    encounterSet.add(encounter);
    outputDataById.put("encounter", encounterSet);

    karProcessingData.setActionOutputDataById(outputDataById);
  }

  private List<DataRequirement> getDataRequirements() {
    List<DataRequirement> dataRequirements = new ArrayList<>();
    DataRequirement patient = new DataRequirement();
    patient.setType("Patient");
    patient.setId("patient");
    DataRequirement encounter = new DataRequirement();
    encounter.setType("Encounter");
    encounter.setId("encounter");
    dataRequirements.add(patient);
    dataRequirements.add(encounter);
    return dataRequirements;
  }

  private HashMap<String, String> getInputRelatedData() {
    HashMap<String, String> related = new HashMap<>();
    related.put("patient", "patient");
    related.put("encounter", "encounter");
    return related;
  }

  private void applyOutputFormat(OutputContentType outputFormat) {
    KnowledgeArtifactStatus status = getKnowledgeArtifactStatus(outputFormat);
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.addArtifactStatus(status);
    karProcessingData.getHealthcareSetting().setKars(kars);
    karProcessingData.setKarStatus(status);
  }

  private KnowledgeArtifactStatus getKnowledgeArtifactStatus(OutputContentType outputFormat) {
    KnowledgeArtifactStatus status = new KnowledgeArtifactStatus();
    status.setId(1);
    status.setIsActive(true);
    status.setOutputFormat(outputFormat);
    status.setKarVersion("1.0.0");
    status.setKarId("rctc-release-2023-02-03-Bundle-rctc");
    status.setVersionUniqueKarId(karProcessingData.getKar().getVersionUniqueId());
    status.setLastActivationDate(new Date());
    status.setSubscriptionsEnabled(false);
    status.setCovidOnly(false);
    return status;
  }

  private HealthcareSetting getHealthcareSetting() {
    return (HealthcareSetting)
        TestUtils.getResourceAsObject("Bsa/HealthCareSettings.json", HealthcareSetting.class);
  }

  private KnowledgeArtifact getKnowledgeArtifact() {
    KnowledgeArtifact knowledgeArtifact = new KnowledgeArtifact();
    knowledgeArtifact.setKarId("rctc-release-2023-02-03-Bundle-rctc");
    knowledgeArtifact.setKarVersion("1.0.0");
    Set<UriType> receiverAddresses = new HashSet<>();
    receiverAddresses.add(new UriType("http://receiver1.example.com"));
    receiverAddresses.add(new UriType("http://receiver2.example.com"));
    knowledgeArtifact.setReceiverAddresses(receiverAddresses);
    return knowledgeArtifact;
  }

  private NotificationContext getNotificationContext() {
    NotificationContext context =
        TestUtils.readFileContents(
            "Bsa/NotificationContext/NotificationContext.json",
            new TypeReference<NotificationContext>() {});
    Bundle notificationBundle =
        loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");
    context.setNotificationData(
        r4Context.newJsonParser().encodeResourceToString(notificationBundle));
    context.setNotificationResourceType("Encounter");
    return context;
  }

  private Bundle loadBundleFromFile(String filename) {
    try (java.io.InputStream inputStream = new ClassPathResource(filename).getInputStream()) {
      return r4Context.newJsonParser().parseResource(Bundle.class, inputStream);
    } catch (Exception e) {
      return null;
    }
  }

  private HttpServer startValidatorServer(String successBody, String errorBody) throws IOException {
    HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
    server.createContext(
        "/success",
        exchange -> {
          byte[] bytes = successBody.getBytes(StandardCharsets.UTF_8);
          exchange.getResponseHeaders().set("Content-Type", "application/json");
          exchange.sendResponseHeaders(200, bytes.length);
          try (OutputStream outputStream = exchange.getResponseBody()) {
            outputStream.write(bytes);
          }
        });
    server.createContext(
        "/error",
        exchange -> {
          byte[] bytes = errorBody.getBytes(StandardCharsets.UTF_8);
          exchange.getResponseHeaders().set("Content-Type", "application/json");
          exchange.sendResponseHeaders(200, bytes.length);
          try (OutputStream outputStream = exchange.getResponseBody()) {
            outputStream.write(bytes);
          }
        });
    server.start();
    return server;
  }

  private String buildOutcomeJson(IssueSeverity severity) {
    OperationOutcome outcome = new OperationOutcome();
    outcome.addIssue().setSeverity(severity).setDiagnostics("validation response");
    return r4Context.newJsonParser().encodeResourceToString(outcome);
  }
}
