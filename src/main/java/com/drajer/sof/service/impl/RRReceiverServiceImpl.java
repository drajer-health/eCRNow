package com.drajer.sof.service.impl;

import static org.apache.commons.text.StringEscapeUtils.unescapeJson;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.sof.model.RRReceiver;
import com.drajer.sof.service.RRReceiverService;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class RRReceiverServiceImpl implements RRReceiverService {

  private final Logger logger = LoggerFactory.getLogger(RRReceiverServiceImpl.class);

  @Override
  public DocumentReference constructDocumentReference(RRReceiver rrReceiver) {
    DocumentReference documentReference = new DocumentReference();

    // Set Doc Ref Status
    documentReference.setStatus(DocumentReferenceStatus.CURRENT);
    documentReference.setDocStatus(ReferredDocumentStatus.FINAL);

    // Set Doc Ref Type
    CodeableConcept typeCode = new CodeableConcept();
    List<Coding> codingList = new ArrayList<>();
    Coding typeCoding = new Coding();
    typeCoding.setSystem("http://loinc.org");
    typeCoding.setCode("88085-6");
    typeCoding.setDisplay("Reportability response report Document Public health");
    codingList.add(typeCoding);
    typeCode.setCoding(codingList);
    typeCode.setText("Reportability response report Document Public health");
    documentReference.setType(typeCode);

    // Set Doc Ref Category
    /*
     * List<CodeableConcept> codeableConceptList = new ArrayList<CodeableConcept>();
     * CodeableConcept categoryCodeableConcept = new CodeableConcept(); List<Coding>
     * categoryCodingList = new ArrayList<Coding>(); Coding categoryCoding = new
     * Coding(); categoryCoding.setSystem("http://loinc.org");
     * categoryCoding.setCode("88085-6"); categoryCoding.
     * setDisplay("Reportability response report Document Public health");
     * categoryCodingList.add(categoryCoding);
     * categoryCodeableConcept.setCoding(categoryCodingList);
     * categoryCodeableConcept.
     * setText("Reportability response report Document Public health");
     * codeableConceptList.add(categoryCodeableConcept);
     * documentReference.setCategory(codeableConceptList);
     */

    // Set Subject
    Reference patientReference = new Reference();
    patientReference.setReference("Patient/" + unescapeJson(rrReceiver.getPatientId()));
    documentReference.setSubject(patientReference);

    // Set Doc Ref Content
    List<DocumentReferenceContentComponent> contentList = new ArrayList<>();
    DocumentReferenceContentComponent contentComp = new DocumentReferenceContentComponent();
    Attachment attachment = new Attachment();
    attachment.setContentType("application/xml;charset=utf-8");
    attachment.setData(unescapeJson(rrReceiver.getRrXml()).getBytes());
    contentComp.setAttachment(attachment);
    contentList.add(contentComp);
    documentReference.setContent(contentList);

    // Set Doc Ref Context
    DocumentReferenceContextComponent contextComp = new DocumentReferenceContextComponent();
    List<Reference> encounterRefList = new ArrayList<>();
    Reference encounterReference = new Reference();
    encounterReference.setReference("Encounter/" + unescapeJson(rrReceiver.getEncounterId()));
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
