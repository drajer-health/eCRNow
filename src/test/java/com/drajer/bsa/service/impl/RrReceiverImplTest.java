package com.drajer.bsa.service.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.never;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.UnclassifiedServerFailureException;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.cda.parser.CdaIi;
import com.drajer.cda.parser.CdaParserConstants;
import com.drajer.cda.parser.CdaRrModel;
import com.drajer.cda.parser.RrParser;
import com.drajer.ecrapp.model.EicrTypes;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.sof.utils.FhirContextInitializer;
import java.util.List;
import java.util.UUID;
import org.hl7.fhir.instance.model.api.IIdType;
import org.hl7.fhir.r4.model.DocumentReference;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ResponseStatusException;

public class RrReceiverImplTest {
  @Mock private PublicHealthMessagesDao phDao;
  @Mock private HealthcareSettingsDao hsDao;
  @Mock private RrParser rrParser;
  @Mock private EhrQueryService ehrService;
  @Mock private FhirContextInitializer fhirContextInitializer;
  @Mock private NotificationContextDao ncDao;

  private PublicHealthMessage phm;
  private HealthcareSetting hs;
  private CdaRrModel rrModel;
  private DocumentReference docRef;
  private JSONObject tokenResponse;
  private MethodOutcome methodOutcome;

  @Mock private Logger logger = LoggerFactory.getLogger(RrReceiverImpl.class);
  @InjectMocks private RrReceiverImpl rrReceiver;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    phm = new PublicHealthMessage();
    phm.setNotificationId(UUID.randomUUID().toString());
    phm.setxRequestId("testRequestId");
    phm.setSubmittedDataId("testEICRDataId");
    phm.setSubmittedVersionNumber(Integer.parseInt("1"));

    hs = new HealthcareSetting();
    hs.setFhirServerBaseURL("http://fhirserver.com");

    rrModel = new CdaRrModel();

    docRef = new DocumentReference();
    tokenResponse = new JSONObject();
    tokenResponse.put("access_token", "mockToken");

    methodOutcome = new MethodOutcome();
    methodOutcome.setCreated(true);
    IIdType mockId = mock(IIdType.class);
    when(mockId.getIdPart()).thenReturn("12345"); // Ensure getIdPart() doesn't return null
    methodOutcome.setId(mockId);
  }

  @Test
  public void testHandleFailureMdn_Success() {

    String xCorrelationId = "test-correlation-id";
    String xRequestId = "test-request-id";
    ReportabilityResponse data = new ReportabilityResponse();
    data.setRrXml("<xml>test</xml>");
    PublicHealthMessage phm = new PublicHealthMessage();
    when(phDao.getByCorrelationId(xCorrelationId)).thenReturn(phm);

    rrReceiver.handleFailureMdn(data, xCorrelationId, xRequestId);

    assertEquals(EicrTypes.RrType.FAILURE_MDN.toString(), phm.getResponseMessageType());
    assertEquals("<xml>test</xml>", phm.getFailureResponseData());
    assertNull(phm.getCdaResponseData());
    assertNull(phm.getResponseDataId());
    assertNull(phm.getResponseReceivedTime());
    assertNull(phm.getResponseProcessingInstruction());
    assertNull(phm.getResponseProcessingStatus());
    assertNull(phm.getResponseEhrDocRefId());
    verify(phDao, times(1)).saveOrUpdate(phm);
  }

  @Test
  public void testHandleFailureMdn_PublicHealthMessageNotFound1() {
    String xCorrelationId = "test-correlation-id";
    String xRequestId = "test-request-id";
    ReportabilityResponse data = new ReportabilityResponse();
    data.setRrXml("<xml>test</xml>");
    when(phDao.getByCorrelationId(xCorrelationId)).thenReturn(null);

    IllegalArgumentException exception =
        assertThrows(
            IllegalArgumentException.class,
            () -> {
              rrReceiver.handleFailureMdn(data, xCorrelationId, xRequestId);
            });
    assertEquals(
        "Unable to find Public Health Message for Correlation Id: test-correlation-id",
        exception.getMessage());

    assertNull(phm.getFailureResponseData());
    assertNull(phm.getResponseMessageType());
    verify(phDao, never()).saveOrUpdate(any());
  }

  @Test
  public void testHandleReportabilityResponse_Success() {
    String xRequestId = "test-request-id";
    ReportabilityResponse data = new ReportabilityResponse();
    data.setRrXml("<xml>test</xml>");

    CdaRrModel rrModel = mock(CdaRrModel.class);
    CdaIi rrDocId = mock(CdaIi.class);
    CdaIi eicrDocId = mock(CdaIi.class);

    when(rrParser.parse(data.getRrXml())).thenReturn(rrModel);
    when(rrModel.getRrDocId()).thenReturn(rrDocId);
    when(rrModel.getEicrDocId()).thenReturn(eicrDocId);
    when(rrDocId.getRootValue()).thenReturn("rr-doc-id");
    when(eicrDocId.getRootValue()).thenReturn("eicr-doc-id");

    PublicHealthMessage phm = new PublicHealthMessage();
    when(phDao.getBySubmittedDataId(eicrDocId.getRootValue())).thenReturn(phm);

    HealthcareSetting hs = new HealthcareSetting();
    hs.setCreateDocRefForResponse(true);
    hs.setHandOffResponseToRestApi("https");
    when(hsDao.getHealthcareSettingByUrl(phm.getFhirServerBaseUrl())).thenReturn(hs);

    rrReceiver.handleReportabilityResponse(data, xRequestId);

    assertEquals(EicrTypes.RrType.REPORTABILITY_RESPONSE.toString(), phm.getResponseMessageType());
    assertEquals("<xml>test</xml>", phm.getCdaResponseData());
    assertEquals("rr-doc-id", phm.getResponseDataId());
    assertNotNull(phm.getResponseReceivedTime());
    verify(phDao, times(1)).saveOrUpdate(phm);
  }

  @Test
  public void testHandleReportabilityResponse_MissingRrDocId() {
    String xRequestId = "test-request-id";
    ReportabilityResponse data = new ReportabilityResponse();
    data.setRrXml("<xml>test</xml>");

    CdaRrModel rrModel = mock(CdaRrModel.class);
    CdaIi rrDocId = mock(CdaIi.class);
    CdaIi eicrDocId = mock(CdaIi.class);

    when(rrParser.parse(data.getRrXml())).thenReturn(rrModel);
    when(rrModel.getRrDocId()).thenReturn(rrDocId);
    when(rrModel.getEicrDocId()).thenReturn(eicrDocId);
    when(rrDocId.getRootValue()).thenReturn(null);

    IllegalArgumentException exception =
        assertThrows(
            IllegalArgumentException.class,
            () -> {
              rrReceiver.handleReportabilityResponse(data, xRequestId);
            });
    assertEquals("Reportability response is missing RR_Doc_Id", exception.getMessage());
  }

  @Test
  public void testHandleReportabilityResponse_PublicHealthMessageNotFound() {
    String xRequestId = "test-request-id";
    ReportabilityResponse data = new ReportabilityResponse();
    data.setRrXml("<xml>test</xml>");

    CdaRrModel rrModel = mock(CdaRrModel.class);
    CdaIi rrDocId = mock(CdaIi.class);
    CdaIi eicrDocId = mock(CdaIi.class);

    when(rrParser.parse(data.getRrXml())).thenReturn(rrModel);
    when(rrModel.getRrDocId()).thenReturn(rrDocId);
    when(rrModel.getEicrDocId()).thenReturn(eicrDocId);
    when(rrDocId.getRootValue()).thenReturn("rr-doc-id");
    when(eicrDocId.getRootValue()).thenReturn("eicr-doc-id");

    when(phDao.getBySubmittedDataId(eicrDocId.getRootValue())).thenReturn(null);

    IllegalArgumentException exception =
        assertThrows(
            IllegalArgumentException.class,
            () -> {
              rrReceiver.handleReportabilityResponse(data, xRequestId);
            });
    assertEquals(
        "Unable to find PublicHealthMessage (Eicr) for Doc Id: {} rr-doc-id",
        exception.getMessage());
  }

  @Test
  public void testHandleReportabilityResponse_HealthcareSettingNotFound1() {
    String xRequestId = "test-request-id";
    ReportabilityResponse data = new ReportabilityResponse();
    data.setRrXml("<xml>test</xml>");

    CdaRrModel rrModel = mock(CdaRrModel.class);
    CdaIi rrDocId = mock(CdaIi.class);
    CdaIi eicrDocId = mock(CdaIi.class);

    when(rrParser.parse(data.getRrXml())).thenReturn(rrModel);
    when(rrModel.getRrDocId()).thenReturn(rrDocId);
    when(rrModel.getEicrDocId()).thenReturn(eicrDocId);
    when(rrDocId.getRootValue()).thenReturn("rr-doc-id");
    when(eicrDocId.getRootValue()).thenReturn("eicr-doc-id");

    PublicHealthMessage phm = new PublicHealthMessage();
    when(phDao.getBySubmittedDataId(eicrDocId.getRootValue())).thenReturn(phm);

    when(hsDao.getHealthcareSettingByUrl(phm.getFhirServerBaseUrl())).thenReturn(null);

    rrReceiver.handleReportabilityResponse(data, xRequestId);

    assertEquals(
        EicrTypes.RrProcessingStatus.HEALTHCARE_SETTING_NOT_FOUND_FOR_RR.toString(),
        phm.getResponseProcessingStatus());

    assertEquals("rr-doc-id", phm.getResponseDataId());
    assertNotNull(phm.getResponseProcessingStatus());
  }

  @Test
  public void testHandleReportabilityResponse_EmptyRrXml() {
    String xRequestId = "test-request-id";
    ReportabilityResponse data = new ReportabilityResponse();
    data.setRrXml("");

    ch.qos.logback.classic.Logger logger =
        (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(RrReceiverImpl.class);
    ListAppender<ILoggingEvent> listAppender = new ListAppender<>();
    listAppender.start();
    logger.addAppender(listAppender);

    rrReceiver.handleReportabilityResponse(data, xRequestId);

    List<ILoggingEvent> logsList = listAppender.list;
    assertTrue(
        logsList
            .stream()
            .anyMatch(
                event ->
                    event
                        .getFormattedMessage()
                        .contains("Received empty RR in request: " + xRequestId)));
  }

  @Test
  public void testHandleReportabilityResponse_MissingEicrDocId() {
    String xRequestId = "test-request-id";
    ReportabilityResponse data = new ReportabilityResponse();
    data.setRrXml("<xml>test</xml>");

    CdaRrModel rrModel = mock(CdaRrModel.class);
    CdaIi rrDocId = mock(CdaIi.class);
    CdaIi eicrDocId = mock(CdaIi.class);

    when(rrParser.parse(data.getRrXml())).thenReturn(rrModel);
    when(rrModel.getRrDocId()).thenReturn(rrDocId);
    when(rrModel.getEicrDocId()).thenReturn(eicrDocId);
    when(eicrDocId.getRootValue()).thenReturn(null);

    IllegalArgumentException exception =
        assertThrows(
            IllegalArgumentException.class,
            () -> {
              rrReceiver.handleReportabilityResponse(data, xRequestId);
            });
    assertEquals("Reportability response is missing RR_Doc_Id", exception.getMessage());
  }

  @Test
  public void testHandleReportabilityResponse_HealthcareSettingNotFound() {
    String xRequestId = "test-request-id";
    ReportabilityResponse data = new ReportabilityResponse();
    data.setRrXml("<xml>test</xml>");

    CdaRrModel rrModel = mock(CdaRrModel.class);
    CdaIi rrDocId = mock(CdaIi.class);
    CdaIi eicrDocId = mock(CdaIi.class);

    when(rrParser.parse(data.getRrXml())).thenReturn(rrModel);
    when(rrModel.getRrDocId()).thenReturn(rrDocId);
    when(rrModel.getEicrDocId()).thenReturn(eicrDocId);
    when(rrDocId.getRootValue()).thenReturn("rr-doc-id");
    when(eicrDocId.getRootValue()).thenReturn("eicr-doc-id");

    PublicHealthMessage phm = new PublicHealthMessage();
    when(phDao.getBySubmittedDataId(eicrDocId.getRootValue())).thenReturn(phm);

    when(hsDao.getHealthcareSettingByUrl(phm.getFhirServerBaseUrl())).thenReturn(null);

    rrReceiver.handleReportabilityResponse(data, xRequestId);

    assertEquals(
        EicrTypes.RrProcessingStatus.HEALTHCARE_SETTING_NOT_FOUND_FOR_RR.toString(),
        phm.getResponseProcessingStatus());
  }

  @Test(expected = ResponseStatusException.class)
  public void testPostDocRefToEhr_Unauthorized() {

    when(ehrService.getAuthorizationToken(hs)).thenReturn(null);

    rrReceiver.postDocRefToEhr(docRef, phm, hs, rrModel);
  }

  @Test(expected = UnclassifiedServerFailureException.class)
  public void testPostDocRefToEhr_FailurePosting() {

    tokenResponse.put("access_token", "mockAccessToken");

    when(ehrService.getAuthorizationToken(hs)).thenReturn(tokenResponse);
    when(ncDao.getNotificationContextById(any())).thenReturn(new NotificationContext());

    when(fhirContextInitializer.getFhirContext(anyString())).thenReturn(mock(FhirContext.class));
    when(fhirContextInitializer.createClient(
            any(), anyString(), anyString(), anyString(), anyString()))
        .thenReturn(mock(IGenericClient.class));

    MethodOutcome methodOutcome = new MethodOutcome();
    methodOutcome.setCreated(false);
    when(fhirContextInitializer.submitResource(any(), eq(docRef))).thenReturn(methodOutcome);

    rrReceiver.postDocRefToEhr(docRef, phm, hs, rrModel);
  }

  @Test
  public void testPostDocRefToEhr_Success() {

    when(ehrService.getAuthorizationToken(hs)).thenReturn(tokenResponse);
    when(ncDao.getNotificationContextById(any(UUID.class))).thenReturn(new NotificationContext());
    when(fhirContextInitializer.getFhirContext(anyString())).thenReturn(mock(FhirContext.class));
    when(fhirContextInitializer.createClient(
            any(), anyString(), anyString(), anyString(), anyString()))
        .thenReturn(mock(IGenericClient.class));
    when(fhirContextInitializer.submitResource(any(), any())).thenReturn(methodOutcome);

    try {
      rrReceiver.postDocRefToEhr(docRef, phm, hs, rrModel);
    } catch (Exception e) {
      fail("Exception should not be thrown");
    }

    verify(ehrService).getAuthorizationToken(hs);
    verify(fhirContextInitializer).submitResource(any(), any());

    assertEquals("mockToken", tokenResponse.getString("access_token"));
  }

  @Test
  public void testConstructDocumentReference_Success1() {
    PublicHealthMessage phm = new PublicHealthMessage();
    phm.setResponseProcessingInstruction(EicrTypes.ReportabilityType.RRVS1.toString());
    phm.setPatientId("patient123");
    phm.setEncounterId("encounter123");

    HealthcareSetting hs = new HealthcareSetting();
    hs.setDefaultProviderId("provider123");
    hs.setDocRefMimeType("application/xml");

    CdaRrModel rrModel = new CdaRrModel();
    String rrXml = "<xml>test</xml>";

    DocumentReference expectedDocRef = new DocumentReference();

    when(ehrService.constructR4DocumentReference(
            rrXml,
            phm.getPatientId(),
            phm.getEncounterId(),
            hs.getDefaultProviderId(),
            hs.getDocRefMimeType(),
            CdaParserConstants.RR_DOC_DISPLAY_NAME,
            CdaParserConstants.RR_DOC_CODE,
            CdaParserConstants.RR_DOC_DISPLAY_NAME,
            CdaParserConstants.RR_DOC_CODE_SYSTEM))
        .thenReturn(expectedDocRef);

    DocumentReference result = rrReceiver.constructDocumentReference(phm, rrModel, hs, rrXml);

    assertNotNull(result);
    assertEquals(expectedDocRef, result);

    verify(ehrService)
        .constructR4DocumentReference(
            rrXml,
            phm.getPatientId(),
            phm.getEncounterId(),
            hs.getDefaultProviderId(),
            hs.getDocRefMimeType(),
            CdaParserConstants.RR_DOC_DISPLAY_NAME,
            CdaParserConstants.RR_DOC_CODE,
            CdaParserConstants.RR_DOC_DISPLAY_NAME,
            CdaParserConstants.RR_DOC_CODE_SYSTEM);
  }

  @Test
  public void testConstructDocumentReference_NullResult1() {
    PublicHealthMessage phm = new PublicHealthMessage();
    phm.setResponseProcessingInstruction("NonMatchingInstruction");

    HealthcareSetting hs = new HealthcareSetting();
    CdaRrModel rrModel = new CdaRrModel();
    String rrXml = "<xml>test</xml>";

    DocumentReference result = rrReceiver.constructDocumentReference(phm, rrModel, hs, rrXml);

    assertNull(result);

    verify(ehrService, never())
        .constructR4DocumentReference(
            any(), any(), any(), any(), any(), any(), any(), any(), any());
  }
}
