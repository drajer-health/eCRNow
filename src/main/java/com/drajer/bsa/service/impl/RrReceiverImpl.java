package com.drajer.bsa.service.impl;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.UnclassifiedServerFailureException;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.bsa.service.RrReceiver;
import com.drajer.cda.parser.CdaIi;
import com.drajer.cda.parser.CdaParserConstants;
import com.drajer.cda.parser.CdaRrModel;
import com.drajer.cda.parser.RrParser;
import com.drajer.ecrapp.model.EicrTypes;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.sof.utils.FhirContextInitializer;
import java.time.Instant;
import java.util.Date;
import java.util.UUID;
import javax.transaction.Transactional;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.hl7.fhir.r4.model.DocumentReference;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.server.ResponseStatusException;

/**
 * The implementation class which is used to process RRs received from PHAs.
 *
 * @author nbashyam
 */
@Service
@Transactional
public class RrReceiverImpl implements RrReceiver {

  private final Logger logger = LoggerFactory.getLogger(RrReceiverImpl.class);

  private static final String ACCESS_TOKEN = "access_token";

  @Autowired HealthcareSettingsDao hsDao;

  @Autowired PublicHealthMessagesDao phDao;

  @Autowired RrParser rrParser;

  @Autowired EhrQueryService ehrService;

  @Autowired FhirContextInitializer fhirContextInitializer;

  @Autowired NotificationContextDao ncDao;

  /**
   * The method is used to handle a failure MDN that is received from the Direct channel.
   *
   * @param data - The RR data received which is nothing but the Failure MDN data.
   * @param xCorrelationId - The CorrelationId to correlate the eICR with the Failure MDN.
   * @param xRequestId - The RequestId to correlate with the eICR with the Failure MDN.
   * @return
   */
  @Override
  public void handleFailureMdn(
      ReportabilityResponse data, String xCorrelationId, String xRequestId) {

    logger.info(" Start processing Failure MDN instead of RR");

    PublicHealthMessage phm = phDao.getByCorrelationId(xCorrelationId);

    if (phm != null) {

      logger.info(
          " Found the Eicr for correlation Id: {}", StringEscapeUtils.escapeJava(xCorrelationId));
      phm.setResponseMessageType(EicrTypes.RrType.FAILURE_MDN.toString());
      // phm.setxRequestId(xRequestId);
      phm.setFailureResponseData(data.getRrXml());

      phDao.saveOrUpdate(phm);

    } else {
      String errorMsg =
          "Unable to find Public Health Message for Correlation Id: "
              + StringEscapeUtils.escapeJava(xCorrelationId);
      logger.error(errorMsg);
      throw new IllegalArgumentException(errorMsg);

      // Create an Error Table and add it to error table for future administration.
    }
  }

  @Override
  public void handleReportabilityResponse(ReportabilityResponse data, String xRequestId) {

    logger.info(" Start processing RR");

    if (data.getRrXml() != null && !data.getRrXml().isEmpty()) {

      logger.debug("Reportability Response: {}", data.getRrXml());

      final CdaRrModel rrModel = rrParser.parse(data.getRrXml());
      final CdaIi rrDocId = rrModel.getRrDocId();
      final CdaIi eicrDocId = rrModel.getEicrDocId();

      if (rrDocId == null || StringUtils.isBlank(rrDocId.getRootValue())) {
        throw new IllegalArgumentException("Reportability response is missing RR_Doc_Id");
      }

      if (eicrDocId == null || StringUtils.isBlank(eicrDocId.getRootValue())) {
        throw new IllegalArgumentException("Reportability response is missing EICR_Doc_Id");
      }

      logger.info(
          "Processing RR_DOC_ID {} of type {} for EICR_DOC_ID {}",
          rrDocId.getRootValue(),
          rrModel.getReportableType(),
          eicrDocId.getRootValue());
      PublicHealthMessage phm = phDao.getBySubmittedDataId(eicrDocId.getRootValue());

      if (phm != null) {

        logger.info(" Found the ecr for doc Id = {}", eicrDocId.getRootValue());

        phm.setCdaResponseData(data.getRrXml());
        // phm.setxRequestId(xRequestId);
        phm.setResponseDataId(rrDocId.getRootValue());
        phm.setResponseMessageType(EicrTypes.RrType.REPORTABILITY_RESPONSE.toString());
        phm.setResponseReceivedTime(Date.from(Instant.now()));

        if (rrModel.getReportableStatus() != null
            && rrModel.getReportableStatus().getCode() != null) {

          // Set the RRVS1 (Reportable) , RRVS2 (May be Reportable),
          // RRVS3 (Not Reportable), RRVS4 (No Rule Met) attribute to push to EHR.
          phm.setResponseProcessingInstruction(rrModel.getReportableStatus().getCode());
        } else phm.setResponseProcessingInstruction(EicrTypes.ReportabilityType.UNKNOWN.toString());

        // Check where the response needs to be delivered.
        HealthcareSetting hs = hsDao.getHealthcareSettingByUrl(phm.getFhirServerBaseUrl());

        if (hs != null) {

          // Default to Success.
          phm.setResponseProcessingStatus(EicrTypes.RrProcessingStatus.SUCCESS.toString());

          try {
            // Check and create Document Reference
            if (Boolean.TRUE.equals(hs.getCreateDocRefForResponse())) {

              createAndSubmitDocRefToEhr(phm, rrModel, hs, data.getRrXml());
            }

          } catch (Exception e) {
            logger.error(
                " Error submitting RR Xml to EHR as Document Reference due to exception: {}",
                e.getMessage());

            phm.setResponseProcessingStatus(
                EicrTypes.RrProcessingStatus.FAILED_RR_SUBMISSION_TO_EHR.toString());
          }

          try {

            // Check and handoff to REST API if needed
            if (hs.getHandOffResponseToRestApi() != null
                && !hs.getHandOffResponseToRestApi().isEmpty()) {
              logger.info("getHandOffResponseToRestApi is not empty");

              if (!submitResponseToRestApi(phm, rrModel, hs, data.getRrXml())) {

                phm.setResponseProcessingStatus(
                    EicrTypes.RrProcessingStatus.FAILED_RR_SUBMISSION_TO_REST_API.toString());
              }
            }

          } catch (Exception e) {
            logger.error(
                " Error submitting RR Xml to EHR or Rest API endpoint due to exception: {}",
                e.getMessage());

            phm.setResponseProcessingStatus(
                EicrTypes.RrProcessingStatus.FAILED_RR_SUBMISSION_TO_REST_API.toString());
          }

        } else {
          logger.error(
              "Healthcare Setting not found for {}, hence cannot continue processing.",
              phm.getFhirServerBaseUrl());

          phm.setResponseProcessingStatus(
              EicrTypes.RrProcessingStatus.HEALTHCARE_SETTING_NOT_FOUND_FOR_RR.toString());
        }

        // Save the state, no matter what so that they can be reprocessed.
        phDao.saveOrUpdate(phm);

      } else {
        String errorMsg =
            "Unable to find PublicHealthMessage (Eicr) for Doc Id: {} " + rrDocId.getRootValue();
        logger.error(errorMsg);

        // Throw to see if we can succeed again, maybe we should store orphan messages in a
        // different table in future.
        throw new IllegalArgumentException(errorMsg);
      }
    } else {
      String errorMsg = "Received empty RR in request: " + xRequestId;
      logger.error(errorMsg);
    }
  }

  private DocumentReference createAndSubmitDocRefToEhr(
      PublicHealthMessage phm, CdaRrModel rrModel, HealthcareSetting hs, String rrXml) {

    DocumentReference docRef = constructDocumentReference(phm, rrModel, hs, rrXml);

    if (docRef != null) {

      postDocRefToEhr(docRef, phm, hs, rrModel);
    } else {

      logger.error(" Unable to submit document reference as it was not created ");
      phm.setResponseProcessingStatus(
          EicrTypes.RrProcessingStatus.FAILED_RR_CONVERSION_TO_DOCREF.toString());
    }

    return docRef;
  }

  private boolean submitResponseToRestApi(
      PublicHealthMessage phm, CdaRrModel rrModel, HealthcareSetting hs, String rrXml) {
    logger.info("Public Health Message:{}", phm);
    logger.info("Cda Rr Model:{}", rrModel);

    boolean isSubmitSuccess = false;
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_XML);

    HttpEntity<String> request = new HttpEntity<>(rrXml, headers);

    ResponseEntity<?> response =
        restTemplate.exchange(
            hs.getHandOffResponseToRestApi(), HttpMethod.POST, request, String.class);

    if (response.getStatusCode().is2xxSuccessful()) {
      isSubmitSuccess = true;
    }

    return isSubmitSuccess;
  }

  public void postDocRefToEhr(
      DocumentReference docRef, PublicHealthMessage phm, HealthcareSetting hs, CdaRrModel rrModel) {

    // Get the AccessToken using the HealthcareSetting
    JSONObject tokenResponse = ehrService.getAuthorizationToken(hs);
    String ehrContext = null;
    if (phm != null) {
      NotificationContext nc =
          ncDao.getNotificationContextById(UUID.fromString(phm.getNotificationId()));
      if (nc != null) ehrContext = nc.getEhrLaunchContext();
    }

    if (tokenResponse != null) {

      String accessToken = tokenResponse.getString(ACCESS_TOKEN);

      String fhirVersion = FhirVersionEnum.R4.toString();

      // Initialize the FHIR Context based on FHIR Version
      FhirContext context = fhirContextInitializer.getFhirContext(fhirVersion);

      // Initialize the Client
      IGenericClient client =
          fhirContextInitializer.createClient(
              context, hs.getFhirServerBaseURL(), accessToken, phm.getxRequestId(), ehrContext);

      MethodOutcome outcome = fhirContextInitializer.submitResource(client, docRef);
      if (outcome != null && outcome.getCreated()) {
        logger.info(
            "Successfully posted RR: {} for EICR: {} version: {} to EHR with DocRefId: {}",
            rrModel.getRrDocId(),
            phm.getSubmittedDataId(),
            phm.getSubmittedVersionNumber(),
            outcome.getId().getIdPart());

        // Update the EHR Doc Ref Id in the eICR table if it was submitted successfully.
        phm.setResponseEhrDocRefId(outcome.getId().getIdPart());

      } else {
        String errorMsg =
            "Unable to post RR response to FHIR server: "
                + StringEscapeUtils.escapeJava(hs.getFhirServerBaseURL());
        logger.error(errorMsg);
        throw new UnclassifiedServerFailureException(500, errorMsg);
      }

    } else {

      throw new ResponseStatusException(
          HttpStatus.UNAUTHORIZED, "Error in getting Authorization token");
    }
  }

  public DocumentReference constructDocumentReference(
      PublicHealthMessage phm, CdaRrModel rrModel, HealthcareSetting hs, String rrXml) {

    logger.info("Cda Rr Model in construct Doc Ref method:{}", rrModel);
    if (phm.getResponseProcessingInstruction() != null
        && (phm.getResponseProcessingInstruction()
                .equals(EicrTypes.ReportabilityType.RRVS1.toString())
            || phm.getResponseProcessingInstruction()
                .equals(EicrTypes.ReportabilityType.RRVS2.toString()))) {

      return ehrService.constructR4DocumentReference(
          rrXml,
          phm.getPatientId(),
          phm.getEncounterId(),
          hs.getDefaultProviderId(),
          hs.getDocRefMimeType(),
          CdaParserConstants.RR_DOC_DISPLAY_NAME,
          CdaParserConstants.RR_DOC_CODE,
          CdaParserConstants.RR_DOC_DISPLAY_NAME,
          CdaParserConstants.RR_DOC_CODE_SYSTEM);

    } else {
      logger.info(
          "Not posting RR to EHR as Response Processing Instruction is : {} ",
          phm.getResponseProcessingInstruction());
      return null;
    }
  }
}
