package com.drajer.ecrapp.service.impl;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.cda.parser.CdaParserConstants;
import com.drajer.cda.parser.CdaRrModel;
import com.drajer.cda.parser.RrParser;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.EicrTypes;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.ecrapp.service.EicrRRService;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.RefreshTokenScheduler;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.transaction.Transactional;
import org.hl7.fhir.r4.model.Attachment;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DocumentReference;
import org.hl7.fhir.r4.model.DocumentReference.DocumentReferenceContentComponent;
import org.hl7.fhir.r4.model.DocumentReference.DocumentReferenceContextComponent;
import org.hl7.fhir.r4.model.DocumentReference.ReferredDocumentStatus;
import org.hl7.fhir.r4.model.Enumerations.DocumentReferenceStatus;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Reference;
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

      logger.info(" Found the ecr for coorrelation Id = {}", xCorrelationId);
      ecr.setResponseType(EicrTypes.RrType.FAILURE_MDN.toString());
      ecr.setResponseXRequestId(xRequestId);
      ecr.setResponseData(data.getRrXml());

      saveOrUpdate(ecr);

    } else {
      logger.error("Unable to find Eicr for Coorrelation Id {} ", xCorrelationId);

      // Create an Error Table and add it to error table for future administration.
    }
  }

  public void handleReportabilityResponse(
      ReportabilityResponse data, String xCorrelationId, String xRequestId) {

    logger.debug(" Start processing RR");

    Eicr ecr = eicrDao.getEicrByCoorrelationId(xCorrelationId);

    if (ecr != null) {

      logger.info(" Found the ecr for coorrelation Id = {}", xCorrelationId);

      CdaRrModel rrModel = rrParser.parse(data.getRrXml());
      ecr.setResponseType(EicrTypes.RrType.REPORTABLE.toString());
      ecr.setResponseDocId(rrModel.getRrDocId().getRootValue());

      ecr.setResponseXRequestId(xRequestId);
      if (data.getRrXml() != null && !data.getRrXml().isEmpty()) {

        logger.info(" RR Xml is present hence create a document reference ");
        ecr.setResponseData(data.getRrXml());
        saveOrUpdate(ecr);

        DocumentReference docRef = constructDocumentReference(data, ecr);

        if (docRef != null) {
          logger.info(" Document Reference created successfully, submitting to Ehr ");
          submitDocRefToEhr(docRef, ecr);
        }
      }
    } else {
      logger.error("Unable to find Eicr for Coorrelation Id {} ", xCorrelationId);
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

    } else {
      throw new RuntimeException("Unrecognized Fhir Server Url : " + ecr.getFhirServerUrl());
    }
  }

  public DocumentReference constructDocumentReference(ReportabilityResponse data, Eicr ecr) {

    DocumentReference documentReference = new DocumentReference();

    // Set Doc Ref Status
    documentReference.setStatus(DocumentReferenceStatus.CURRENT);
    documentReference.setDocStatus(ReferredDocumentStatus.FINAL);

    // Set Doc Ref Type
    CodeableConcept typeCode = new CodeableConcept();
    List<Coding> codingList = new ArrayList<>();
    Coding typeCoding = new Coding();
    typeCoding.setSystem(CdaParserConstants.RR_DOC_CODE_SYSTEM);
    typeCoding.setCode(CdaParserConstants.RR_DOC_CODE);
    typeCoding.setDisplay(CdaParserConstants.RR_DOC_DISPLAY_NAME);
    codingList.add(typeCoding);
    typeCode.setCoding(codingList);
    typeCode.setText(CdaParserConstants.RR_DOC_DISPLAY_NAME);
    documentReference.setType(typeCode);

    // Set Subject
    Reference patientReference = new Reference();
    patientReference.setReference("Patient/" + ecr.getLaunchPatientId());
    documentReference.setSubject(patientReference);

    // Set Doc Ref Content
    List<DocumentReferenceContentComponent> contentList = new ArrayList<>();
    DocumentReferenceContentComponent contentComp = new DocumentReferenceContentComponent();
    Attachment attachment = new Attachment();
    attachment.setContentType(CdaParserConstants.RR_DOC_CONTENT_TYPE);

    if (data.getRrXml() != null) {
      attachment.setData(data.getRrXml().getBytes());
    }
    contentComp.setAttachment(attachment);
    contentList.add(contentComp);
    documentReference.setContent(contentList);

    // Set Doc Ref Context
    DocumentReferenceContextComponent contextComp = new DocumentReferenceContextComponent();
    List<Reference> encounterRefList = new ArrayList<>();
    Reference encounterReference = new Reference();
    encounterReference.setReference("Encounter/" + ecr.getEncounterId());
    encounterRefList.add(encounterReference);
    contextComp.setEncounter(encounterRefList);

    Period period = new Period();
    period.setStart(new Date());
    period.setEnd(new Date());
    contextComp.setPeriod(period);
    documentReference.setContext(contextComp);

    String docReference =
        FhirContext.forR4().newJsonParser().encodeResourceToString(documentReference);
    logger.debug("DocumentReference Object===========> {}", docReference);

    return documentReference;
  }
}
