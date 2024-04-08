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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.transaction.Transactional;
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

  @Autowired EicrDao eicrDao;

  @Autowired ClientDetailsService clientDetailservice;

  @Autowired LaunchService launchDetailsService;

  @Autowired RefreshTokenScheduler tokenScheduler;

  @Autowired Authorization authorization;

  @Autowired FhirContextInitializer fhirContextInitializer;

  @Autowired R4ResourcesData r4ResourcesData;

  RrParser rrParser;

  @Autowired RestTemplate restTemplate;

  @Value("${ecr.rr.processorphanrr:false}")
  private Boolean processOrphanRr;

  public EicrServiceImpl() {
    rrParser = new RrParser();
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

  public void setProcessOrphanRr(Boolean processOrphanRr) {
    this.processOrphanRr = processOrphanRr;
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

    if (data.getRrXml() != null && !data.getRrXml().isEmpty()) {

      logger.debug("Reportability Response: {}", data.getRrXml());

      final CdaRrModel cdaRrModel = rrParser.parse(data.getRrXml());
      final CdaIi rrDocId = cdaRrModel.getRrDocId();
      final CdaIi eicrDocId = cdaRrModel.getEicrDocId();

      if (rrDocId == null || StringUtils.isBlank(rrDocId.getRootValue())) {
        throw new IllegalArgumentException("Reportability response is missing RR_Doc_Id");
      }

      if (eicrDocId == null || StringUtils.isBlank(eicrDocId.getRootValue())) {
        throw new IllegalArgumentException("Reportability response is missing EICR_Doc_Id");
      }

      logger.info(
          "Processing RR_DOC_ID {} of type {} for EICR_DOC_ID {}",
          rrDocId.getRootValue(),
          cdaRrModel.getReportableType(),
          eicrDocId.getRootValue());
      Eicr ecr = eicrDao.getEicrByDocId(eicrDocId.getRootValue());

      if (ecr == null && Boolean.TRUE.equals(processOrphanRr)) {
        logger.info("processOrphanRr is true, continue processing RR");
        String patientId = cdaRrModel.getPatId();
        String encounterId = cdaRrModel.getEnctId();
        if (!StringUtils.isBlank(patientId) && !StringUtils.isBlank(encounterId)) {
          ecr = new Eicr();
          ecr.setLaunchPatientId(patientId);
          ecr.setEncounterId(encounterId);
          ecr.setEicrDocId(eicrDocId.getRootValue());
          ecr.setSetId(patientId + "|" + encounterId);
          ecr.setFhirServerUrl(data.getFhirUrl());
        }
      }

      if (ecr != null) {

        ClientDetails clientDetails =
            clientDetailservice.getClientDetailsByUrl(ecr.getFhirServerUrl());

        logger.info(" Found the ecr for doc Id = {}", eicrDocId.getRootValue());
        ecr.setResponseType(EicrTypes.RrType.REPORTABLE.toString());
        ecr.setResponseDocId(rrDocId.getRootValue());
        ecr.setResponseXRequestId(xRequestId);
        ecr.setResponseData(data.getRrXml());

        if (cdaRrModel.getReportableType() != null)
          ecr.setResponseType(cdaRrModel.getReportableType());
        else ecr.setResponseType(CdaRrModel.UNKONWN_RESPONSE_TYPE);

        if (cdaRrModel.getReportableType() != null && cdaRrModel.getReportableStatus() != null)
          ecr.setResponseTypeDisplay(
              cdaRrModel.getReportableType()
                  + "-"
                  + cdaRrModel.getReportableStatus().getDisplayName());
        else if (cdaRrModel.getReportableType() != null)
          ecr.setResponseTypeDisplay(cdaRrModel.getReportableType());
        else if (cdaRrModel.getReportableStatus() != null)
          ecr.setResponseTypeDisplay(cdaRrModel.getReportableStatus().getDisplayName());
        else ecr.setResponseTypeDisplay(CdaRrModel.UNKONWN_RESPONSE_TYPE);

        if (Boolean.TRUE.equals(clientDetails.getIsCreateDocRef())
            || Boolean.TRUE.equals(clientDetails.getIsBoth())) {
          if (saveToEhr) {
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
                  " Error submitting Document Reference to EHR due to exception: {}",
                  e.getMessage());
              // Save the fact that we could not submit the message to the EHR.
              ecr.setRrProcStatus(EventTypes.RrProcStatusEnum.FAILED_EHR_SUBMISSION.toString());
              saveOrUpdate(ecr);
              throw e;
            }
          }
        }

        if (Boolean.TRUE.equals(clientDetails.getIsInvokeRestAPI())
            || Boolean.TRUE.equals(clientDetails.getIsBoth())) {
          try {
            logger.info("Submit RR Xml to Rest API endpoint");
            boolean responseStatus = submitRRXmlToRestAPI(data.getRrXml(), ecr, clientDetails);
            if (!responseStatus) {
              ecr.setRrProcStatus(EventTypes.RrProcStatusEnum.FAILED_EHR_SUBMISSION.toString());
              saveOrUpdate(ecr);
            }
          } catch (Exception e) {
            logger.error(
                " Error submitting RR Xml to Rest API endpoint due to exception: {}",
                e.getMessage());
            // Save the fact that we could not submit the message to the EHR.
            ecr.setRrProcStatus(EventTypes.RrProcStatusEnum.FAILED_EHR_SUBMISSION.toString());
            saveOrUpdate(ecr);
            throw e;
          }
        }

        saveOrUpdate(ecr);

      } else {
        String errorMsg =
            "Unable to find Eicr for EICR_DOC_ID: "
                + StringEscapeUtils.escapeJava(eicrDocId.getRootValue());
        logger.error(errorMsg);
        throw new IllegalArgumentException(errorMsg);
      }
    } else {
      String errorMsg = "Received empty RR in request: " + StringEscapeUtils.escapeJava(xRequestId);
      logger.error(errorMsg);
      throw new IllegalArgumentException(errorMsg);
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

    final String fhirServerURL = ecr.getFhirServerUrl();

    // Get ClientDetails using the FHIR Server URL
    ClientDetails clientDetails = clientDetailservice.getClientDetailsByUrl(fhirServerURL);

    // Get the AccessToken using the Client Details and read the Metadata
    // Information to know
    // about the FHIR Server Version.
    if (clientDetails != null) {

      logger.info(" Found the Ehr Server Url ");

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
