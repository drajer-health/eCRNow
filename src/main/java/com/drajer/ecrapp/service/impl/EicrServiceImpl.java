package com.drajer.ecrapp.service.impl;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
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
import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.R4ResourcesData;
import com.drajer.sof.utils.RefreshTokenScheduler;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.transaction.Transactional;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.DocumentReference;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class EicrServiceImpl implements EicrRRService {

  private final Logger logger = LoggerFactory.getLogger(EicrServiceImpl.class);

  private static final String ACCESS_TOKEN = "access_token";
  private static final String FHIR_VERSION = "fhirVersion";

  @Autowired EicrDao eicrDao;

  @Autowired ClientDetailsService clientDetailservice;

  @Autowired RefreshTokenScheduler tokenScheduler;

  @Autowired Authorization authorization;

  @Autowired FhirContextInitializer fhirContextInitializer;

  @Autowired R4ResourcesData r4ResourcesData;

  RrParser rrParser;

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

    Eicr ecr = eicrDao.getEicrByCoorrelationId(xCorrelationId);

    if (ecr != null) {

      logger.info(" Found the Eicr for correlation Id: {}", xCorrelationId);
      ecr.setResponseType(EicrTypes.RrType.FAILURE_MDN.toString());
      ecr.setResponseXRequestId(xRequestId);
      ecr.setResponseData(data.getRrXml());

      saveOrUpdate(ecr);

    } else {
      String errorMsg = "Unable to find Eicr for Correlation Id: " + xCorrelationId;
      logger.error(errorMsg);
      throw new IllegalArgumentException(errorMsg);
      // Create an Error Table and add it to error table for future administration.
    }
  }

  public void handleReportabilityResponse(ReportabilityResponse data, String xRequestId) {

    logger.debug(" Start processing RR");

    if (data.getRrXml() != null && !data.getRrXml().isEmpty()) {

      logger.debug("Reportability Response: {}", data.getRrXml());
      final CdaRrModel rrModel = rrParser.parse(data.getRrXml());
      final CdaIi docId = rrModel.getRrDocId();

      if (docId == null || StringUtils.isBlank(docId.getRootValue())) {
        throw new IllegalArgumentException("Reportability response is missing Doc Id");
      }

      final Eicr ecr = eicrDao.getEicrByDocId(rrModel.getEicrDocId().getRootValue());

      if (ecr != null) {

        logger.info(" Found the ecr for doc Id = {}", docId.getRootValue());
        ecr.setResponseType(EicrTypes.RrType.REPORTABLE.toString());
        ecr.setResponseDocId(docId.getRootValue());
        ecr.setResponseXRequestId(xRequestId);
        ecr.setResponseData(data.getRrXml());

        if (rrModel.getReportableType() != null) ecr.setResponseType(rrModel.getReportableType());
        else ecr.setResponseType(CdaRrModel.UNKONWN_RESPONSE_TYPE);

        if (rrModel.getReportableType() != null && rrModel.getReportableStatus() != null)
          ecr.setResponseTypeDisplay(
              rrModel.getReportableType() + "-" + rrModel.getReportableStatus().getDisplayName());
        else if (rrModel.getReportableType() != null)
          ecr.setResponseTypeDisplay(rrModel.getReportableType());
        else if (rrModel.getReportableStatus() != null)
          ecr.setResponseTypeDisplay(rrModel.getReportableStatus().getDisplayName());
        else ecr.setResponseTypeDisplay(CdaRrModel.UNKONWN_RESPONSE_TYPE);

        try {
          logger.info(" RR Xml and eCR is present hence create a document reference ");
          DocumentReference docRef = constructDocumentReference(data, ecr);

          if (docRef != null) {

            logger.info(" Document Reference created successfully, submitting to Ehr ");
            submitDocRefToEhr(docRef, ecr);
          }

        } catch (Exception e) {
          logger.error(
              " Error in the the submission of the Doc Reference to the EHR due to {}",
              e.getMessage());

          // Save the fact that we could not submit the message to the EHR.
          ecr.setRrProcStatus(EventTypes.RrProcStatusEnum.FAILED_EHR_SUBMISSION.toString());
        }

        saveOrUpdate(ecr);

      } else {
        String errorMsg = "Unable to find Eicr for Doc Id: {} " + docId.getRootValue();
        logger.error(errorMsg);
        throw new IllegalArgumentException(errorMsg);
      }
    } else {
      String errorMsg = "Received empty RR in request: " + xRequestId;
      logger.error(errorMsg);
      throw new IllegalArgumentException(errorMsg);
    }
  }

  public void submitDocRefToEhr(DocumentReference docRef, Eicr ecr) {

    final String fhirServerURL = ecr.getFhirServerUrl();

    // Get ClientDetails using the FHIR Server URL
    ClientDetails clientDetails = clientDetailservice.getClientDetailsByUrl(fhirServerURL);

    // Get the AccessToken using the Client Details and read the Metadata Information to know
    // about the FHIR Server Version.
    if (clientDetails != null) {

      logger.info(" Found the Ehr Server Url ");

      JSONObject tokenResponse = tokenScheduler.getSystemAccessToken(clientDetails);
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
          fhirContextInitializer.createClient(context, fhirServerURL, accessToken);

      MethodOutcome outcome = fhirContextInitializer.submitResource(client, docRef);

      if (outcome.getCreated()) {
        logger.info("Successfully sent RR to fhir");

        // Update the EHR Doc Ref Id in the eICR table if it was submitted successfully.
        ecr.setEhrDocRefId(docRef.getId());
        saveOrUpdate(ecr);

      } else {
        String errorMsg = "Unable to post RR response to FHIR server: " + ecr.getFhirServerUrl();
        logger.error(errorMsg);
        throw new RuntimeException(errorMsg);
      }

    } else {
      String errorMsg = "Unrecognized Fhir Server Url: " + ecr.getFhirServerUrl();
      logger.error(errorMsg);
      throw new IllegalArgumentException(errorMsg);
    }
  }

  public DocumentReference constructDocumentReference(ReportabilityResponse data, Eicr ecr) {

    if (ecr.getResponseType() != null
        && ecr.getResponseType().equals(EicrTypes.ReportabilityType.RRVS1.toString())) {
      return r4ResourcesData.constructR4DocumentReference(
          data.getRrXml(), ecr.getLaunchPatientId(), ecr.getEncounterId());
    } else return null;
  }

  public List<JSONObject> getEicrData(Map<String, String> searchParams) {
    List<Eicr> eicrData = eicrDao.getEicrData(searchParams);
    List<JSONObject> eicrDataList = new ArrayList<JSONObject>();
    for (Eicr eicr : eicrData) {
      JSONObject eicrObject = new JSONObject();
      eicrObject.put("eicrData", eicr.getEicrData());
      eicrDataList.add(eicrObject);
    }
    return eicrDataList;
  }

  public List<JSONObject> getRRData(Map<String, String> searchParams) {
    List<Eicr> rrData = eicrDao.getRRData(searchParams);
    List<JSONObject> rrDataList = new ArrayList<JSONObject>();
    for (Eicr eicr : rrData) {
      JSONObject eicrObject = new JSONObject();
      eicrObject.put("responseData", eicr.getResponseData());
      rrDataList.add(eicrObject);
    }
    return rrDataList;
  }

  @Override
  public void handleReportabilityResponse(
      ReportabilityResponse data, String xCorrelationId, String xRequestId) {
    // TODO Auto-generated method stub

  }
}
