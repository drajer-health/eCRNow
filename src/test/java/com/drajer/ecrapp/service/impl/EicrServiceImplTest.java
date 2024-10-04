package com.drajer.ecrapp.service.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.UnclassifiedServerFailureException;
import com.drajer.cda.parser.CdaRrModel;
import com.drajer.cda.parser.RrParser;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
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
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.server.ResponseStatusException;

@SpringBootTest
@RunWith(MockitoJUnitRunner.class)
public class EicrServiceImplTest {

  @Spy @InjectMocks EicrServiceImpl eicrServiceImpl;

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
    ReflectionTestUtils.setField(eicrServiceImpl, "processOrphanRr", true);
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
    verify(eicrServiceImpl, times(1))
        .handleFailureMdn(reportabilityResponse, "ecrUnitTestCorrelationID", "ecrunittest_id");
  }

  @Test
  public void constructDocumentReference() {
    Eicr eicr = new Eicr();
    DocumentReference actualDocumentReference =
        eicrServiceImpl.constructDocumentReference(reportabilityResponse, eicr, "");
    assertNull(actualDocumentReference);
  }

  @Test
  public void submitDocRefToEhrWithoutCustomerDetails() {
    DocumentReference documentReference = new DocumentReference();
    IllegalArgumentException exception =
        assertThrows(
            IllegalArgumentException.class,
            () -> {
              eicrServiceImpl.submitDocRefToEhr(documentReference, eicr);
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
    UnclassifiedServerFailureException exception =
        assertThrows(
            UnclassifiedServerFailureException.class,
            () -> {
              eicrServiceImpl.submitDocRefToEhr(documentReference, eicr);
            });
  }

  @Test
  public void submitDocRefToEhrByException() {
    RefreshTokenScheduler tokenScheduler = mock(RefreshTokenScheduler.class);
    Mockito.lenient()
        .doReturn(clientDetails)
        .when(clientDetailsService)
        .getClientDetailsByUrl(Mockito.any());

    ResponseStatusException exception =
        assertThrows(
            ResponseStatusException.class,
            () -> {
              eicrServiceImpl.submitDocRefToEhr(documentReference, eicr);
            });
  }

  @Test
  public void handleReportabilityWithroutReportabilityResponse() {
    ReportabilityResponse reportabilityResponse = new ReportabilityResponse();
    IllegalArgumentException exception =
        assertThrows(
            IllegalArgumentException.class,
            () -> {
              eicrServiceImpl.handleReportabilityResponse(
                  reportabilityResponse, "ecrunittest_id", true);
            });
  }

  @Test
  public void handleFailureMdnForException() {
    ReportabilityResponse reportabilityResponse = new ReportabilityResponse();
    IllegalArgumentException exception =
        assertThrows(
            IllegalArgumentException.class,
            () -> {
              eicrServiceImpl.handleFailureMdn(
                  reportabilityResponse, "ecrUnitTestCorrelationID", "ecrunittest_id");
            });
  }

  @Test
  public void handleReportabilityResponseForException() throws Exception {
    CdaRrModel cdaRrModel = Utility.cdaRrModel();
    Mockito.lenient().doReturn(cdaRrModel).when(rrParser).parse(Mockito.any());

    Mockito.lenient().doReturn(eicr).when(eicrDao).getEicrByDocId(Mockito.anyString());
    launchDetails.setRrRestAPIUrl("/api/launchDetails");
    launchDetails.setIsBoth(true);

    Mockito.lenient()
        .doReturn(launchDetails)
        .when(launchDetailsService)
        .getAuthDetailsById(Mockito.any());

    Mockito.lenient()
        .doReturn(documentReference)
        .when(r4ResourcesData)
        .constructR4DocumentReference(
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());

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
    Mockito.lenient()
        .doReturn(outcome)
        .when(fhirContextInitializer)
        .submitResource(Mockito.any(), Mockito.any());
    Mockito.lenient().doReturn(eicr).when(eicrDao).saveOrUpdate(eicr);

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_XML);

    ResponseEntity<String> response = new ResponseEntity<>("response", headers, HttpStatus.OK);

    Mockito.when(
            restTemplate.exchange(
                Mockito.anyString(),
                Mockito.eq(HttpMethod.POST),
                Mockito.any(HttpEntity.class),
                Mockito.eq(String.class)))
        .thenReturn(response);

    eicrServiceImpl.handleReportabilityResponse(reportabilityResponse, "ecrunittest_id", true);
    verify(eicrServiceImpl, times(1))
        .handleReportabilityResponse(reportabilityResponse, "ecrunittest_id", true);
  }

  @Test
  public void handleReportabilityResponse() throws Exception {

    CdaRrModel cdaRrModel = Utility.getCdaRrModel();
    Mockito.lenient().doReturn(cdaRrModel).when(rrParser).parse(Mockito.any());

    Mockito.lenient().doReturn(eicr).when(eicrDao).getEicrByDocId(Mockito.anyString());
    launchDetails.setRrRestAPIUrl("/api/launchDetails");
    launchDetails.setIsBoth(true);

    Mockito.lenient()
        .doReturn(launchDetails)
        .when(launchDetailsService)
        .getAuthDetailsById(Mockito.any());

    Mockito.lenient()
        .doReturn(documentReference)
        .when(r4ResourcesData)
        .constructR4DocumentReference(
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());

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
    Mockito.lenient()
        .doReturn(outcome)
        .when(fhirContextInitializer)
        .submitResource(Mockito.any(), Mockito.any());
    Mockito.lenient().doReturn(eicr).when(eicrDao).saveOrUpdate(eicr);

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_XML);

    ResponseEntity<String> response = new ResponseEntity<>("response", headers, HttpStatus.OK);

    Mockito.when(
            restTemplate.exchange(
                Mockito.anyString(),
                Mockito.eq(HttpMethod.POST),
                Mockito.any(HttpEntity.class),
                Mockito.eq(String.class)))
        .thenReturn(response);

    eicrServiceImpl.handleReportabilityResponse(reportabilityResponse, "ecrunittest_id", true);
    verify(eicrServiceImpl, times(1))
        .handleReportabilityResponse(reportabilityResponse, "ecrunittest_id", true);
  }

  @Test
  public void getEicrData() {

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
  public void getRRData() {

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
      throws JsonParseException, JsonMappingException, IOException {

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
    verify(eicrServiceImpl, times(1)).deleteEicr(eicr);
  }

  @Test
  public void setProcessOrphanRr() {
    eicrServiceImpl.setProcessOrphanRr(true);
    verify(eicrServiceImpl, times(1)).setProcessOrphanRr(true);
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

    Mockito.when(
            restTemplate.exchange(
                Mockito.anyString(),
                Mockito.eq(HttpMethod.POST),
                Mockito.any(HttpEntity.class),
                Mockito.eq(String.class)))
        .thenReturn(null);
    Mockito.lenient().doReturn(eicr).when(eicrDao).saveOrUpdate(eicr);

    Exception exception =
        assertThrows(
            Exception.class,
            () -> {
              eicrServiceImpl.handleReportabilityResponse(
                  reportabilityResponse, "ecrunittest_id", true);
            });
  }
}
