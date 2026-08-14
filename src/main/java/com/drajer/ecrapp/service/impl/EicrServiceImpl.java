package com.drajer.ecrapp.service.impl;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.UnclassifiedServerFailureException;
import com.drajer.cda.parser.CdaIi;
import com.drajer.cda.parser.CdaRrModel;
import com.drajer.cda.parser.RrParser;
import com.drajer.eca.model.EventTypes;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.EicrTypes;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.ecrapp.service.EicrRRService;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.R4ResourcesData;
import com.drajer.sof.utils.RefreshTokenScheduler;
import jakarta.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.hl7.fhir.r4.model.DocumentReference;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.server.ResponseStatusException;

@Service
@Transactional
public class EicrServiceImpl implements EicrRRService {

  private final Logger logger = LoggerFactory.getLogger(EicrServiceImpl.class);

  private static final String ACCESS_TOKEN = "access_token";
  private static final String FHIR_VERSION = "fhirVersion";

  private final EicrDao eicrDao;
  private final ClientDetailsService clientDetailservice;
  private final LaunchService launchDetailsService;
  private final RefreshTokenScheduler tokenScheduler;
  private final Authorization authorization;
  private final FhirContextInitializer fhirContextInitializer;
  private final R4ResourcesData r4ResourcesData;
  private final RrParser rrParser;
  private final RestTemplate restTemplate;
  private final Boolean processOrphanRr;

  /**
   * Instantiates a new EICR service implementation.
   *
   * @param eicrDao the EICR DAO
   * @param clientDetailservice the client details service
   * @param launchDetailsService the launch details service
   * @param tokenScheduler the refresh token scheduler
   * @param authorization the authorization utility
   * @param fhirContextInitializer the FHIR context initializer
   * @param r4ResourcesData the R4 resources data
   * @param restTemplate the REST template
   * @param processOrphanRr the process orphan RR flag from properties
   */
  @Autowired
  public EicrServiceImpl(
      EicrDao eicrDao,
      ClientDetailsService clientDetailservice,
      LaunchService launchDetailsService,
      RefreshTokenScheduler tokenScheduler,
      Authorization authorization,
      FhirContextInitializer fhirContextInitializer,
      R4ResourcesData r4ResourcesData,
      RestTemplate restTemplate,
      @Value("${ecr.rr.processorphanrr:false}") Boolean processOrphanRr) {
    this.eicrDao = eicrDao;
    this.clientDetailservice = clientDetailservice;
    this.launchDetailsService = launchDetailsService;
    this.tokenScheduler = tokenScheduler;
    this.authorization = authorization;
    this.fhirContextInitializer = fhirContextInitializer;
    this.r4ResourcesData = r4ResourcesData;
    this.rrParser = new RrParser();
    this.restTemplate = restTemplate;
    this.processOrphanRr = processOrphanRr;
  }

  public Eicr saveOrUpdate(Eicr eicr) {
    eicrDao.saveOrUpdate(eicr);
    return eicr;
  }

  public Eicr getEicrById(Integer id) {
    return eicrDao.getEicrById(id);
  }

  public Eicr getEicrByDocId(String docId) {
    return eicrDao.getEicrByDocId(docId);
  }

  public ReportabilityResponse saveOrUpdate(ReportabilityResponse rr) {
    eicrDao.saveOrUpdate(rr);
    return rr;
  }

  public ReportabilityResponse getRRById(Integer id) {
    return eicrDao.getRRById(id);
  }

  public Integer getMaxVersionId(Eicr eicr) {
    return eicrDao.getMaxVersionId(eicr);
  }

  public void handleFailureMdn(
      ReportabilityResponse data, String xCorrelationId, String xRequestId) {

    logger.debug(" Start processing MDN");

    Eicr ecr = eicrDao.getEicrByCorrelationId(xCorrelationId);

    if (ecr != null) {

      logger.info(
          " Found the Eicr for correlation Id: {}", StringEscapeUtils.escapeJava(xCorrelationId));
      ecr.setResponseType(EicrTypes.RrType.FAILURE_MDN.toString());
      ecr.setResponseXRequestId(xRequestId);
      ecr.setResponseData(data.getRrXml());

      saveOrUpdate(ecr);

    } else {
      String errorMsg =
          "Unable to find Eicr for Correlation Id: " + StringEscapeUtils.escapeJava(xCorrelationId);
      logger.error(errorMsg);
      throw new IllegalArgumentException(errorMsg);
      // Create an Error Table and add it to error table for future administration.
    }
  }

  public void handleReportabilityResponse(
      ReportabilityResponse data, String xRequestId, boolean saveToEhr) {
    logger.debug(" Start processing RR");

    if (data.getRrXml() == null || data.getRrXml().isEmpty()) {
      String errorMsg = "Received empty RR in request: " + StringEscapeUtils.escapeJava(xRequestId);
      logger.error(errorMsg);
      throw new IllegalArgumentException(errorMsg);
    }

    logger.debug("Reportability Response: {}", data.getRrXml());
    final CdaRrModel cdaRrModel = rrParser.parse(data.getRrXml());
    validateReportabilityResponse(cdaRrModel);

    final CdaIi rrDocId = cdaRrModel.getRrDocId();
    final CdaIi eicrDocId = cdaRrModel.getEicrDocId();

    logger.info(
        "Processing RR_DOC_ID {} of type {} for EICR_DOC_ID {}",
        rrDocId.getRootValue(),
        cdaRrModel.getReportableType(),
        eicrDocId.getRootValue());

    Eicr ecr = eicrDao.getEicrByDocId(eicrDocId.getRootValue());
    if (ecr == null) {
      ecr = createOrphanEicrIfNeeded(cdaRrModel, eicrDocId, data);
    }

    if (ecr != null) {
      processReportabilityResponse(ecr, cdaRrModel, rrDocId, data, xRequestId, saveToEhr);
    } else {
      String errorMsg =
          "Unable to find Eicr for EICR_DOC_ID: "
              + StringEscapeUtils.escapeJava(eicrDocId.getRootValue());
      logger.error(errorMsg);
      throw new IllegalArgumentException(errorMsg);
    }
  }

  private void validateReportabilityResponse(CdaRrModel cdaRrModel) {
    final CdaIi rrDocId = cdaRrModel.getRrDocId();
    if (rrDocId == null || StringUtils.isBlank(rrDocId.getRootValue())) {
      throw new IllegalArgumentException("Reportability response is missing RR_Doc_Id");
    }

    final CdaIi eicrDocId = cdaRrModel.getEicrDocId();
    if (eicrDocId == null || StringUtils.isBlank(eicrDocId.getRootValue())) {
      throw new IllegalArgumentException("Reportability response is missing EICR_Doc_Id");
    }
  }

  private Eicr createOrphanEicrIfNeeded(
      CdaRrModel cdaRrModel, CdaIi eicrDocId, ReportabilityResponse data) {
    if (!Boolean.TRUE.equals(processOrphanRr)) {
      return null;
    }

    logger.info("processOrphanRr is true, continue processing RR");
    String patientId = cdaRrModel.getPatId();
    String encounterId = cdaRrModel.getEnctId();

    if (StringUtils.isBlank(patientId) || StringUtils.isBlank(encounterId)) {
      return null;
    }

    Eicr ecr = new Eicr();
    ecr.setLaunchPatientId(patientId);
    ecr.setEncounterId(encounterId);
    ecr.setEicrDocId(eicrDocId.getRootValue());
    ecr.setSetId(patientId + "|" + encounterId);
    ecr.setFhirServerUrl(data.getFhirUrl());
    return ecr;
  }

  private void processReportabilityResponse(
      Eicr ecr,
      CdaRrModel cdaRrModel,
      CdaIi rrDocId,
      ReportabilityResponse data,
      String xRequestId,
      boolean saveToEhr) {
    ClientDetails clientDetails = clientDetailservice.getClientDetailsByUrl(ecr.getFhirServerUrl());

    logger.info(" Found the ecr for doc Id = {}", rrDocId.getRootValue());
    ecr.setResponseType(EicrTypes.RrType.REPORTABLE.toString());
    ecr.setResponseDocId(rrDocId.getRootValue());
    ecr.setResponseXRequestId(xRequestId);
    ecr.setResponseData(data.getRrXml());

    setResponseTypeInformation(ecr, cdaRrModel);
    submitDocumentReferenceIfNeeded(ecr, data, clientDetails, saveToEhr);
    submitToRestApiIfNeeded(ecr, data, clientDetails);

    saveOrUpdate(ecr);
  }

  private void setResponseTypeInformation(Eicr ecr, CdaRrModel cdaRrModel) {
    // Set response type
    if (cdaRrModel.getReportableType() != null) {
      ecr.setResponseType(cdaRrModel.getReportableType());
    } else {
      ecr.setResponseType(CdaRrModel.UNKONWN_RESPONSE_TYPE);
    }

    // Set response type display
    if (cdaRrModel.getReportableType() != null && cdaRrModel.getReportableStatus() != null) {
      ecr.setResponseTypeDisplay(
          cdaRrModel.getReportableType() + "-" + cdaRrModel.getReportableStatus().getDisplayName());
    } else if (cdaRrModel.getReportableType() != null) {
      ecr.setResponseTypeDisplay(cdaRrModel.getReportableType());
    } else if (cdaRrModel.getReportableStatus() != null) {
      ecr.setResponseTypeDisplay(cdaRrModel.getReportableStatus().getDisplayName());
    } else {
      ecr.setResponseTypeDisplay(CdaRrModel.UNKONWN_RESPONSE_TYPE);
    }
  }

  private void submitDocumentReferenceIfNeeded(
      Eicr ecr, ReportabilityResponse data, ClientDetails clientDetails, boolean saveToEhr) {
    if (!Boolean.TRUE.equals(clientDetails.getIsCreateDocRef())
        && !Boolean.TRUE.equals(clientDetails.getIsBoth())) {
      return;
    }

    if (!saveToEhr) {
      return;
    }

    try {
      logger.info(" RR Xml and eCR is present hence create a document reference ");
      DocumentReference docRef =
          constructDocumentReference(data, ecr, clientDetails.getRrDocRefMimeType());

      if (docRef != null) {
        logger.info(" Document Reference created successfully, submitting to Ehr ");
        submitDocRefToEhr(docRef, ecr);
      }
    } catch (Exception e) {
      logger.error(
          " Error submitting Document Reference to EHR due to exception: {}", e.getMessage());
      ecr.setRrProcStatus(EventTypes.RrProcStatusEnum.FAILED_EHR_SUBMISSION.toString());
      saveOrUpdate(ecr);
      throw e;
    }
  }

  private void submitToRestApiIfNeeded(
      Eicr ecr, ReportabilityResponse data, ClientDetails clientDetails) {
    if (!Boolean.TRUE.equals(clientDetails.getIsInvokeRestAPI())
        && !Boolean.TRUE.equals(clientDetails.getIsBoth())) {
      return;
    }

    try {
      logger.info("Submit RR Xml to Rest API endpoint");
      boolean responseStatus = submitRRXmlToRestAPI(data.getRrXml(), ecr, clientDetails);
      if (!responseStatus) {
        ecr.setRrProcStatus(EventTypes.RrProcStatusEnum.FAILED_EHR_SUBMISSION.toString());
        saveOrUpdate(ecr);
      }
    } catch (Exception e) {
      logger.error(
          " Error submitting RR Xml to Rest API endpoint due to exception: {}", e.getMessage());
      ecr.setRrProcStatus(EventTypes.RrProcStatusEnum.FAILED_EHR_SUBMISSION.toString());
      saveOrUpdate(ecr);
      throw e;
    }
  }

  private boolean submitRRXmlToRestAPI(String rrXml, Eicr ecr, ClientDetails clientDetails) {
    logger.info("Eicr in submitRRXmlToRestAPI:{}", ecr);
    boolean isSubmitSuccess = false;
    //    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_XML);
    HttpEntity<String> request = new HttpEntity<>(rrXml, headers);
    ResponseEntity<?> response =
        restTemplate.exchange(
            clientDetails.getRrRestAPIUrl(), HttpMethod.POST, request, String.class);
    if (response.getStatusCode().is2xxSuccessful()) {
      isSubmitSuccess = true;
    }
    return isSubmitSuccess;
  }

  public void submitDocRefToEhr(DocumentReference docRef, Eicr ecr) {

    // Get ClientDetails using the FHIR Server URL
    ClientDetails clientDetails = clientDetailservice.getClientDetailsByUrl(ecr.getFhirServerUrl());

    // Get the AccessToken using the Client Details and read the Metadata
    // Information to know
    // about the FHIR Server Version.
    if (clientDetails != null) {

      logger.info(" Found the Ehr Server Url ");

      final String fhirServerURL = clientDetails.getFhirServerBaseURL();
      JSONObject tokenResponse = tokenScheduler.getAccessTokenUsingClientDetails(clientDetails);
      if (tokenResponse == null) {
        throw new ResponseStatusException(
            HttpStatus.UNAUTHORIZED, "Error in getting Authorization");
      }
      String accessToken = tokenResponse.getString(ACCESS_TOKEN);

      String fhirVersion = "";
      JSONObject object = authorization.getMetadata(fhirServerURL + "/metadata");

      if (object != null) {

        logger.info("Reading Metadata information from server ");
        if (object.getString(FHIR_VERSION).contains("1.")) {
          fhirVersion = FhirVersionEnum.DSTU2.toString();
        }
        if (object.getString(FHIR_VERSION).contains("4.")) {
          fhirVersion = FhirVersionEnum.R4.toString();
        }
      }

      // Initialize the FHIR Context based on FHIR Version
      FhirContext context = fhirContextInitializer.getFhirContext(fhirVersion);

      // Initialize the Client
      IGenericClient client =
          fhirContextInitializer.createClient(
              context, fhirServerURL, accessToken, ecr.getResponseXRequestId(), null);

      MethodOutcome outcome = fhirContextInitializer.submitResource(client, docRef);

      if (outcome != null && outcome.getCreated()) {
        logger.info(
            "Successfully posted RR: {} for EICR: {} version: {} to EHR with DocRefId: {}",
            ecr.getResponseDocId(),
            ecr.getEicrDocId(),
            ecr.getDocVersion(),
            outcome.getId().getIdPart());
        // Update the EHR Doc Ref Id in the eICR table if it was submitted successfully.
        ecr.setEhrDocRefId(outcome.getId().getIdPart());
        ecr.setRrProcStatus(EventTypes.RrProcStatusEnum.SUCCESSFULLY_PROCESSED.toString());
        saveOrUpdate(ecr);

      } else {
        String errorMsg = "Unable to post RR response to FHIR server: " + ecr.getFhirServerUrl();
        logger.error(errorMsg);
        throw new UnclassifiedServerFailureException(500, errorMsg);
      }

    } else {
      String errorMsg = "Unrecognized Fhir Server Url: " + ecr.getFhirServerUrl();
      logger.error(errorMsg);
      throw new IllegalArgumentException(errorMsg);
    }
  }

  public DocumentReference constructDocumentReference(
      ReportabilityResponse data, Eicr ecr, String rrDocRefMimeType) {

    if (ecr.getResponseType() != null
        && (ecr.getResponseType().equals(EicrTypes.ReportabilityType.RRVS1.toString())
            || ecr.getResponseType().equals(EicrTypes.ReportabilityType.RRVS2.toString()))) {
      return r4ResourcesData.constructR4DocumentReference(
          data.getRrXml(),
          ecr.getLaunchPatientId(),
          ecr.getEncounterId(),
          ecr.getProviderUUID(),
          rrDocRefMimeType);
    } else {
      logger.info("Not posting RR to EHR as it is of type {}", ecr.getResponseType());
      return null;
    }
  }

  public List<JSONObject> getEicrData(Map<String, String> searchParams) {
    List<Eicr> eicrData = eicrDao.getEicrData(searchParams);
    List<JSONObject> eicrDataList = new ArrayList<>();
    for (Eicr eicr : eicrData) {
      JSONObject eicrObject = new JSONObject();
      eicrObject.put("eicrData", eicr.getEicrData());
      eicrDataList.add(eicrObject);
    }
    return eicrDataList;
  }

  public List<JSONObject> getRRData(Map<String, String> searchParams) {
    List<Eicr> rrData = eicrDao.getRRData(searchParams);
    List<JSONObject> rrDataList = new ArrayList<>();
    for (Eicr eicr : rrData) {
      JSONObject eicrObject = new JSONObject();
      eicrObject.put("responseData", eicr.getResponseData());
      rrDataList.add(eicrObject);
    }
    return rrDataList;
  }

  public List<JSONObject> getEicrAndRRByXRequestId(String xRequestId) {
    List<Eicr> eicrList = eicrDao.getEicrAndRRByXRequestId(xRequestId);
    List<JSONObject> eicrDataList = new ArrayList<>();
    for (Eicr eicr : eicrList) {
      JSONObject eicrObject = new JSONObject();
      eicrObject.put("eicrData", eicr.getEicrData());
      eicrObject.put("responseData", eicr.getResponseData());
      eicrDataList.add(eicrObject);
    }
    return eicrDataList;
  }

  public void deleteEicr(Eicr eicr) {
    eicrDao.deleteEicr(eicr);
  }
}
