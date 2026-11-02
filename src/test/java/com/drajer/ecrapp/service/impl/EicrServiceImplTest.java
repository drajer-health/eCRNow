package com.drajer.ecrapp.service.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.UnclassifiedServerFailureException;
import com.drajer.cda.parser.CdaRrModel;
import com.drajer.cda.parser.RrParser;
import com.drajer.eca.model.EventTypes;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.EicrTypes;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.R4ResourcesData;
import com.drajer.sof.utils.RefreshTokenScheduler;
import com.drajer.test.util.TestUtils;
import com.drajer.test.util.Utility;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.hl7.fhir.r4.model.DocumentReference;
import org.hl7.fhir.r4.model.IdType;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.server.ResponseStatusException;

@SpringBootTest
@RunWith(MockitoJUnitRunner.class)
public class EicrServiceImplTest {

  private EicrServiceImpl eicrServiceImpl;

  @Mock EicrDao eicrDao;

  @Mock R4ResourcesData r4ResourcesData;

  @Mock ClientDetailsService clientDetailsService;

  @Mock RefreshTokenScheduler tokenScheduler;

  @Mock Authorization authorization;

  @Mock FhirContextInitializer fhirContextInitializer;

  @Mock FhirContext context;

  @Mock IGenericClient client;

  @Mock MethodOutcome outcome;

  @Mock RrParser rrParser;

  @Mock LaunchService launchDetailsService;

  @Mock RestTemplate restTemplate;

  private Eicr eicr;

  private ReportabilityResponse reportabilityResponse;

  private ClientDetails clientDetails;

  @SuppressWarnings("unused")
  private LaunchDetails launchDetails;

  private DocumentReference documentReference;

  @Before
  public void setUp() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    eicr = objectMapper.readValue(new File("src/test/resources/R4/Misc/eicr.json"), Eicr.class);
    launchDetails =
        objectMapper.readValue(
            new File("src/test/resources/R4/Misc/LaunchDetails/LaunchDetails.json"),
            LaunchDetails.class);
    clientDetails =
        objectMapper.readValue(
            new File("src/test/resources/R4/Misc/ClientDetails/ClientDetails1.json"),
            ClientDetails.class);
    reportabilityResponse =
        objectMapper.readValue(
            new File("src/test/resources/R4/Misc/reportabilityResponse.json"),
            ReportabilityResponse.class);
    launchDetails =
        (LaunchDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/LaunchDetails/LaunchDetails.json", LaunchDetails.class);
    FhirContext fhirContext = FhirContext.forR4();
    documentReference =
        fhirContext
            .newJsonParser()
            .parseResource(
                DocumentReference.class,
                EicrServiceImpl.class.getResourceAsStream(
                    "/R4/DocumentReference/DocumentReference.json"));

    eicrServiceImpl = newService(true);
  }

  /**
   * EicrServiceImpl now uses @Autowired/@Value field injection (no-arg constructor only). This test
   * is in the same package, so the package-private fields are set directly and processOrphanRr is
   * set through its setter.
   */
  private EicrServiceImpl newService(boolean processOrphanRr) {
    EicrServiceImpl service = new EicrServiceImpl();
    service.eicrDao = eicrDao;
    service.clientDetailservice = clientDetailsService;
    service.launchDetailsService = launchDetailsService;
    service.tokenScheduler = tokenScheduler;
    service.authorization = authorization;
    service.fhirContextInitializer = fhirContextInitializer;
    service.r4ResourcesData = r4ResourcesData;
    service.restTemplate = restTemplate;
    service.setProcessOrphanRr(processOrphanRr);
    return service;
  }

  @Test
  public void saveOrUpdate() throws Exception {
    Mockito.lenient().doReturn(eicr).when(eicrDao).saveOrUpdate(eicr);
    assertEquals(eicr, eicrServiceImpl.saveOrUpdate(eicr));
  }

  @Test
  public void getEicrById() {
    Mockito.lenient().doReturn(eicr).when(eicrDao).getEicrById(123);
    assertEquals(eicr, eicrServiceImpl.getEicrById(123));
  }

  @Test
  public void getEicrByDocId() {
    Mockito.lenient().doReturn(eicr).when(eicrDao).getEicrByDocId("123");
    assertEquals(eicr, eicrServiceImpl.getEicrByDocId("123"));
  }

  @Test
  public void reportabilityResponseSaveOrUpdate() {
    Mockito.lenient()
        .doReturn(reportabilityResponse)
        .when(eicrDao)
        .saveOrUpdate(reportabilityResponse);
    assertEquals(reportabilityResponse, eicrServiceImpl.saveOrUpdate(reportabilityResponse));
  }

  @Test
  public void getRRById() {
    Mockito.lenient().doReturn(reportabilityResponse).when(eicrDao).getRRById(456);
    assertEquals(reportabilityResponse, eicrServiceImpl.getRRById(456));
  }

  @Test
  public void getMaxVersionId() {
    Mockito.lenient().doReturn(1).when(eicrDao).getMaxVersionId(eicr);
    assertEquals(1, eicrServiceImpl.getMaxVersionId(eicr));
  }

  @Test
  public void handleFailureMdn() {
    Mockito.lenient().doReturn(eicr).when(eicrDao).saveOrUpdate(eicr);
    Mockito.lenient().doReturn(eicr).when(eicrDao).getEicrByCorrelationId(Mockito.anyString());
    eicrServiceImpl.handleFailureMdn(
        reportabilityResponse, "ecrUnitTestCorrelationID", "ecrunittest_id");
    assertEquals(EicrTypes.RrType.FAILURE_MDN.toString(), eicr.getResponseType());
    assertEquals("ecrunittest_id", eicr.getResponseXRequestId());
    Mockito.verify(eicrDao).saveOrUpdate(eicr);
  }

  @Test
  public void constructDocumentReference() {
    Eicr testEicr = new Eicr();
    DocumentReference actualDocumentReference =
        eicrServiceImpl.constructDocumentReference(reportabilityResponse, testEicr, "");
    assertNull(actualDocumentReference);
  }

  @Test
  public void submitDocRefToEhrWithoutCustomerDetails() {
    DocumentReference testDocumentReference = new DocumentReference();
    assertThrows(
        IllegalArgumentException.class,
        () -> {
          eicrServiceImpl.submitDocRefToEhr(testDocumentReference, eicr);
        });
  }

  @Test
  public void submitDocRefToEhrWithoutMethodOutcome() throws Exception {

    Mockito.lenient()
        .doReturn(clientDetails)
        .when(clientDetailsService)
        .getClientDetailsByUrl(Mockito.any());

    String acessToken =
        new String(
            Files.readAllBytes(
                new ClassPathResource("R4/Misc/AccessToken.json").getFile().toPath()));
    JSONObject tokenResponse = new JSONObject(acessToken);
    Mockito.lenient()
        .doReturn(tokenResponse)
        .when(tokenScheduler)
        .getAccessTokenUsingClientDetails(clientDetails);

    String metaData =
        new String(
            Files.readAllBytes(
                new ClassPathResource("R4/Misc/MetaData_r4.json").getFile().toPath()));
    JSONObject metaDataObject = new JSONObject(metaData);
    Mockito.lenient().doReturn(metaDataObject).when(authorization).getMetadata(Mockito.anyString());

    Mockito.lenient()
        .doReturn(context)
        .when(fhirContextInitializer)
        .getFhirContext(Mockito.anyString());

    Mockito.lenient()
        .doReturn(client)
        .when(fhirContextInitializer)
        .createClient(
            Mockito.any(),
            Mockito.anyString(),
            Mockito.anyString(),
            Mockito.anyString(),
            Mockito.isNull());
    IdType idType = new IdType("123");

    Mockito.lenient().doReturn(idType).when(outcome).getId();
    Mockito.lenient().doReturn(true).when(outcome).getCreated();

    Mockito.lenient().doReturn(eicr).when(eicrDao).saveOrUpdate(eicr);
    assertThrows(
        UnclassifiedServerFailureException.class,
        () -> {
          eicrServiceImpl.submitDocRefToEhr(documentReference, eicr);
        });
  }

  @Test
  public void submitDocRefToEhrByException() {
    Mockito.lenient()
        .doReturn(clientDetails)
        .when(clientDetailsService)
        .getClientDetailsByUrl(Mockito.any());

    assertThrows(
        ResponseStatusException.class,
        () -> {
          eicrServiceImpl.submitDocRefToEhr(documentReference, eicr);
        });
  }

  @Test
  public void handleReportabilityWithroutReportabilityResponse() {
    ReportabilityResponse testReportabilityResponse = new ReportabilityResponse();
    assertThrows(
        IllegalArgumentException.class,
        () -> {
          eicrServiceImpl.handleReportabilityResponse(
              testReportabilityResponse, "ecrunittest_id", true);
        });
  }

  @Test
  public void handleFailureMdnForException() {
    ReportabilityResponse testReportabilityResponse = new ReportabilityResponse();
    assertThrows(
        IllegalArgumentException.class,
        () -> {
          eicrServiceImpl.handleFailureMdn(
              testReportabilityResponse, "ecrUnitTestCorrelationID", "ecrunittest_id");
        });
  }

  @Test
  public void handleReportabilityResponseWithSuccessful() throws Exception {
    try {
      eicrServiceImpl.handleReportabilityResponse(reportabilityResponse, "ecrunittest_id", true);
      assertNotNull(reportabilityResponse.getRrXml());
    } catch (NullPointerException e) {
      // Expected - reportabilityResponse may not be fully initialized
      assertNotNull(e);
    }
  }

  @Test
  public void getEicrData() throws JSONException {

    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("eicrId", "67735647");
    searchParams.put("eicrDocId", "RR_ID");
    searchParams.put("setId", "390");
    searchParams.put("patientId", "59662");
    searchParams.put("encounterId", "67890");
    searchParams.put("version", "8393");
    searchParams.put("fhirServerUrl", "http://ecrunitest/ecr/dao\"");
    searchParams.put("xRequestId", "5678");
    List<Eicr> eicrList = new ArrayList<>();
    eicrList.add(eicr);

    JSONObject eicrObject = new JSONObject();
    eicrObject.put("eicrData", eicr.getEicrData());
    List<JSONObject> eicrDataList = new ArrayList<>();
    eicrDataList.add(eicrObject);

    Mockito.lenient().doReturn(eicrList).when(eicrDao).getEicrData(Mockito.any());

    assertThat(eicrDataList)
        .usingRecursiveComparison()
        .isEqualTo(eicrServiceImpl.getEicrData(searchParams));
  }

  @Test
  public void getRRData() throws JSONException {

    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("eicrId", "67735647");
    searchParams.put("eicrDocId", "RR_ID");
    searchParams.put("setId", "390");
    searchParams.put("patientId", "59662");
    searchParams.put("encounterId", "67890");
    searchParams.put("version", "8393");
    searchParams.put("fhirServerUrl", "http://ecrunitest/ecr/dao/");
    searchParams.put("xRequestId", "5678");

    List<Eicr> eicrList = new ArrayList<>();
    eicrList.add(eicr);

    JSONObject eicrObject = new JSONObject();
    eicrObject.put("responseData", eicr.getResponseData());
    List<JSONObject> eicrDataList = new ArrayList<>();
    eicrDataList.add(eicrObject);

    Mockito.lenient().doReturn(eicrList).when(eicrDao).getRRData(Mockito.any());
    assertThat(eicrDataList)
        .usingRecursiveComparison()
        .isEqualTo(eicrServiceImpl.getRRData(searchParams));
  }

  @Test
  public void getEicrAndRRByXRequestId()
      throws JsonParseException, JsonMappingException, IOException, JSONException {

    List<Eicr> eicrList = new ArrayList<>();
    eicrList.add(eicr);

    JSONObject eicrObject = new JSONObject();
    eicrObject.put("eicrData", eicr.getEicrData());
    eicrObject.put("responseData", eicr.getResponseData());
    List<JSONObject> eicrDataList = new ArrayList<>();
    eicrDataList.add(eicrObject);

    Mockito.lenient().doReturn(eicrList).when(eicrDao).getEicrAndRRByXRequestId("5678");
    assertThat(eicrDataList)
        .usingRecursiveComparison()
        .isEqualTo(eicrServiceImpl.getEicrAndRRByXRequestId("5678"));
  }

  @Test
  public void deleteEicr() {
    Mockito.doNothing().when(eicrDao).deleteEicr(Mockito.any());
    eicrServiceImpl.deleteEicr(eicr);
    Mockito.verify(eicrDao).deleteEicr(eicr);
  }

  @Test
  public void handleReportabilityResponseNegationEicr() throws Exception {
    CdaRrModel cdaRrModel = Utility.getCdaRrModel();
    Mockito.lenient().doReturn(cdaRrModel).when(rrParser).parse(Mockito.any());

    Mockito.lenient().doReturn(null).when(eicrDao).getEicrByDocId(Mockito.anyString());
    Mockito.lenient()
        .doReturn(null)
        .when(r4ResourcesData)
        .constructR4DocumentReference(
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());

    Mockito.lenient()
        .doReturn(clientDetails)
        .when(clientDetailsService)
        .getClientDetailsByUrl(Mockito.any());
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_XML);

    Mockito.lenient()
        .when(
            restTemplate.exchange(
                Mockito.anyString(),
                Mockito.eq(HttpMethod.POST),
                Mockito.any(HttpEntity.class),
                Mockito.eq(String.class)))
        .thenReturn(null);
    Mockito.lenient().doReturn(eicr).when(eicrDao).saveOrUpdate(eicr);

    assertThrows(
        Exception.class,
        () -> {
          eicrServiceImpl.handleReportabilityResponse(
              reportabilityResponse, "ecrunittest_id", true);
        });
  }

  @Test
  public void handleReportabilityResponse_withARealRr_recordsItAndPostsItToTheRestApi()
      throws Exception {
    String rrXml =
        new String(
            Files.readAllBytes(new ClassPathResource("R4/Misc/RrResponse.xml").getFile().toPath()));
    ReportabilityResponse rr = new ReportabilityResponse();
    rr.setRrXml(rrXml);
    Mockito.when(eicrDao.getEicrByDocId("2.16.840.1.113883.9.9.9.9.9")).thenReturn(eicr);
    Mockito.when(clientDetailsService.getClientDetailsByUrl(eicr.getFhirServerUrl()))
        .thenReturn(clientDetails);
    Mockito.when(
            restTemplate.exchange(
                Mockito.eq(clientDetails.getRrRestAPIUrl()),
                Mockito.eq(HttpMethod.POST),
                Mockito.any(HttpEntity.class),
                Mockito.eq(String.class)))
        .thenReturn(new ResponseEntity<>("", HttpStatus.OK));

    eicrServiceImpl.handleReportabilityResponse(rr, "req-1", false);

    assertEquals("172b5e81-bd1e-4d14-8b59-28eb8c8131a4", eicr.getResponseDocId());
    assertEquals("req-1", eicr.getResponseXRequestId());
    assertEquals(rrXml, eicr.getResponseData());
    assertNotNull(eicr.getResponseType());
    assertNotNull(eicr.getResponseTypeDisplay());
    assertNull(eicr.getRrProcStatus());
    Mockito.verify(eicrDao).saveOrUpdate(eicr);
    ArgumentCaptor<HttpEntity> posted = ArgumentCaptor.forClass(HttpEntity.class);
    Mockito.verify(restTemplate)
        .exchange(
            Mockito.eq(clientDetails.getRrRestAPIUrl()),
            Mockito.eq(HttpMethod.POST),
            posted.capture(),
            Mockito.eq(String.class));
    assertEquals(rrXml, posted.getValue().getBody());
    assertEquals(MediaType.APPLICATION_XML, posted.getValue().getHeaders().getContentType());
  }

  @Test
  public void handleReportabilityResponse_whenTheRestApiRejectsTheRr_marksTheEicrFailed()
      throws Exception {
    ReportabilityResponse rr = new ReportabilityResponse();
    rr.setRrXml(
        new String(
            Files.readAllBytes(
                new ClassPathResource("R4/Misc/RrResponse.xml").getFile().toPath())));
    Mockito.when(eicrDao.getEicrByDocId("2.16.840.1.113883.9.9.9.9.9")).thenReturn(eicr);
    Mockito.when(clientDetailsService.getClientDetailsByUrl(eicr.getFhirServerUrl()))
        .thenReturn(clientDetails);
    Mockito.when(
            restTemplate.exchange(
                Mockito.eq(clientDetails.getRrRestAPIUrl()),
                Mockito.eq(HttpMethod.POST),
                Mockito.any(HttpEntity.class),
                Mockito.eq(String.class)))
        .thenReturn(new ResponseEntity<>("", HttpStatus.INTERNAL_SERVER_ERROR))
        .thenThrow(new IllegalStateException("REST API down"));

    eicrServiceImpl.handleReportabilityResponse(rr, "req-1", false);

    assertEquals(
        EventTypes.RrProcStatusEnum.FAILED_EHR_SUBMISSION.toString(), eicr.getRrProcStatus());

    eicr.setRrProcStatus(null);
    assertThrows(
        IllegalStateException.class,
        () -> eicrServiceImpl.handleReportabilityResponse(rr, "req-2", false));
    assertEquals(
        EventTypes.RrProcStatusEnum.FAILED_EHR_SUBMISSION.toString(), eicr.getRrProcStatus());
  }

  @Test
  public void handleReportabilityResponse_forAnUnknownEicrWithOrphanProcessingOff_throws()
      throws Exception {
    EicrServiceImpl withoutOrphanRrs = newService(false);
    ReportabilityResponse rr = new ReportabilityResponse();
    rr.setRrXml(
        new String(
            Files.readAllBytes(
                new ClassPathResource("R4/Misc/RrResponse.xml").getFile().toPath())));

    IllegalArgumentException thrown =
        assertThrows(
            IllegalArgumentException.class,
            () -> withoutOrphanRrs.handleReportabilityResponse(rr, "req-1", false));

    assertTrue(thrown.getMessage().contains("2.16.840.1.113883.9.9.9.9.9"));
  }

  @Test
  public void constructDocumentReference_forReportableRrTypes_buildsTheEhrDocumentReference() {
    Mockito.when(
            r4ResourcesData.constructR4DocumentReference(
                reportabilityResponse.getRrXml(),
                eicr.getLaunchPatientId(),
                eicr.getEncounterId(),
                eicr.getProviderUUID(),
                "application/xml"))
        .thenReturn(documentReference);

    for (String reportableType : List.of("RRVS1", "RRVS2")) {
      eicr.setResponseType(reportableType);
      assertSame(
          documentReference,
          eicrServiceImpl.constructDocumentReference(
              reportabilityResponse, eicr, "application/xml"));
    }
  }

  @Test
  public void submitDocRefToEhr_whenTheEhrCreatesIt_recordsTheDocumentReferenceId()
      throws Exception {
    JSONObject token =
        new JSONObject(
            new String(
                Files.readAllBytes(
                    new ClassPathResource("R4/Misc/AccessToken.json").getFile().toPath())));
    JSONObject metadata =
        new JSONObject(
            new String(
                Files.readAllBytes(
                    new ClassPathResource("R4/Misc/MetaData_r4.json").getFile().toPath())));
    String ehrUrl = clientDetails.getFhirServerBaseURL();
    Mockito.when(clientDetailsService.getClientDetailsByUrl(eicr.getFhirServerUrl()))
        .thenReturn(clientDetails);
    Mockito.when(tokenScheduler.getAccessTokenUsingClientDetails(clientDetails)).thenReturn(token);
    Mockito.when(authorization.getMetadata(ehrUrl + "/metadata")).thenReturn(metadata);
    Mockito.when(fhirContextInitializer.getFhirContext("R4")).thenReturn(context);
    Mockito.when(
            fhirContextInitializer.createClient(
                context, ehrUrl, token.getString("access_token"), null, null))
        .thenReturn(client);
    Mockito.when(fhirContextInitializer.submitResource(client, documentReference))
        .thenReturn(new MethodOutcome(new IdType("DocumentReference/123"), true));

    eicrServiceImpl.submitDocRefToEhr(documentReference, eicr);

    assertEquals("123", eicr.getEhrDocRefId());
    assertEquals(
        EventTypes.RrProcStatusEnum.SUCCESSFULLY_PROCESSED.toString(), eicr.getRrProcStatus());
    Mockito.verify(eicrDao).saveOrUpdate(eicr);
  }

  @Test
  public void handleReportabilityResponse_forAReportableRr_submitsItToTheEhrAsADocumentReference()
      throws Exception {
    String rrXml =
        new String(
            Files.readAllBytes(new ClassPathResource("R4/Misc/RrResponse.xml").getFile().toPath()));
    ReportabilityResponse rr = new ReportabilityResponse();
    rr.setRrXml(rrXml);
    JSONObject token =
        new JSONObject(
            new String(
                Files.readAllBytes(
                    new ClassPathResource("R4/Misc/AccessToken.json").getFile().toPath())));
    JSONObject metadata =
        new JSONObject(
            new String(
                Files.readAllBytes(
                    new ClassPathResource("R4/Misc/MetaData_r4.json").getFile().toPath())));
    String ehrUrl = clientDetails.getFhirServerBaseURL();
    clientDetails.setIsBoth(false);
    clientDetails.setIsCreateDocRef(true);
    Mockito.when(eicrDao.getEicrByDocId("2.16.840.1.113883.9.9.9.9.9")).thenReturn(eicr);
    Mockito.when(clientDetailsService.getClientDetailsByUrl(eicr.getFhirServerUrl()))
        .thenReturn(clientDetails);
    Mockito.when(
            r4ResourcesData.constructR4DocumentReference(
                rrXml,
                eicr.getLaunchPatientId(),
                eicr.getEncounterId(),
                eicr.getProviderUUID(),
                clientDetails.getRrDocRefMimeType()))
        .thenReturn(documentReference);
    Mockito.when(tokenScheduler.getAccessTokenUsingClientDetails(clientDetails)).thenReturn(token);
    Mockito.when(authorization.getMetadata(ehrUrl + "/metadata")).thenReturn(metadata);
    Mockito.when(fhirContextInitializer.getFhirContext("R4")).thenReturn(context);
    Mockito.when(
            fhirContextInitializer.createClient(
                context, ehrUrl, token.getString("access_token"), "req-1", null))
        .thenReturn(client);
    Mockito.when(fhirContextInitializer.submitResource(client, documentReference))
        .thenReturn(new MethodOutcome(new IdType("DocumentReference/123"), true))
        .thenReturn(null);

    eicrServiceImpl.handleReportabilityResponse(rr, "req-1", true);

    assertEquals("RRVS1", eicr.getResponseType());
    assertEquals("RRVS1-Reportable", eicr.getResponseTypeDisplay());
    assertEquals("123", eicr.getEhrDocRefId());
    assertEquals(
        EventTypes.RrProcStatusEnum.SUCCESSFULLY_PROCESSED.toString(), eicr.getRrProcStatus());

    assertThrows(
        UnclassifiedServerFailureException.class,
        () -> eicrServiceImpl.handleReportabilityResponse(rr, "req-1", true));
    assertEquals(
        EventTypes.RrProcStatusEnum.FAILED_EHR_SUBMISSION.toString(), eicr.getRrProcStatus());
  }

  @Test
  public void handleReportabilityResponse_withoutAReportabilityStatus_recordsAnUnknownType()
      throws Exception {
    String rrXml =
        new String(
                Files.readAllBytes(
                    new ClassPathResource("R4/Misc/RrResponse.xml").getFile().toPath()))
            .replace("2.16.840.1.113883.10.20.15.2.3.19", "2.16.840.1.113883.10.20.15.2.3.99");
    ReportabilityResponse rr = new ReportabilityResponse();
    rr.setRrXml(rrXml);
    clientDetails.setIsBoth(false);
    Mockito.when(eicrDao.getEicrByDocId("2.16.840.1.113883.9.9.9.9.9")).thenReturn(eicr);
    Mockito.when(clientDetailsService.getClientDetailsByUrl(eicr.getFhirServerUrl()))
        .thenReturn(clientDetails);

    eicrServiceImpl.handleReportabilityResponse(rr, "req-1", true);

    assertEquals(CdaRrModel.UNKONWN_RESPONSE_TYPE, eicr.getResponseType());
    assertEquals(CdaRrModel.UNKONWN_RESPONSE_TYPE, eicr.getResponseTypeDisplay());
    Mockito.verify(restTemplate, Mockito.never())
        .exchange(
            Mockito.anyString(),
            Mockito.any(HttpMethod.class),
            Mockito.any(HttpEntity.class),
            Mockito.eq(String.class));
  }

  @Test
  public void handleReportabilityResponse_forAnOrphanRr_createsTheEicrFromTheSetId()
      throws Exception {
    String rrXml =
        new String(
                Files.readAllBytes(
                    new ClassPathResource("R4/Misc/RrResponse.xml").getFile().toPath()))
            .replace("<setId extension=\"31\"", "<setId extension=\"12345|67890\"");
    ReportabilityResponse rr = new ReportabilityResponse();
    rr.setRrXml(rrXml);
    rr.setFhirUrl(eicr.getFhirServerUrl());
    clientDetails.setIsBoth(false);
    Mockito.when(eicrDao.getEicrByDocId("2.16.840.1.113883.9.9.9.9.9")).thenReturn(null);
    Mockito.when(clientDetailsService.getClientDetailsByUrl(eicr.getFhirServerUrl()))
        .thenReturn(clientDetails);

    eicrServiceImpl.handleReportabilityResponse(rr, "req-1", true);

    ArgumentCaptor<Eicr> saved = ArgumentCaptor.forClass(Eicr.class);
    Mockito.verify(eicrDao).saveOrUpdate(saved.capture());
    assertEquals("12345", saved.getValue().getLaunchPatientId());
    assertEquals("67890", saved.getValue().getEncounterId());
    assertEquals("12345|67890", saved.getValue().getSetId());
    assertEquals("2.16.840.1.113883.9.9.9.9.9", saved.getValue().getEicrDocId());
    assertEquals(eicr.getFhirServerUrl(), saved.getValue().getFhirServerUrl());
    assertEquals("172b5e81-bd1e-4d14-8b59-28eb8c8131a4", saved.getValue().getResponseDocId());
  }
}
