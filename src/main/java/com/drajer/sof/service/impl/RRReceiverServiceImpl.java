package com.drajer.sof.service.impl;

import ca.uhn.fhir.context.FhirContext;
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
  public DocumentReference constructDocumentReference(
      String obj, String type, String patientId, String encounterId) {
    DocumentReference documentReference = new DocumentReference();

    // Set Doc Ref Status
    documentReference.setStatus(DocumentReferenceStatus.CURRENT);

    // Set Doc Ref Type
    CodeableConcept typeCode = new CodeableConcept();
    List<Coding> codingList = new ArrayList<Coding>();
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
    patientReference.setReference("Patient/" + patientId);
    documentReference.setSubject(patientReference);

    // Set Doc Ref Content
    List<DocumentReferenceContentComponent> contentList =
        new ArrayList<DocumentReference.DocumentReferenceContentComponent>();
    DocumentReferenceContentComponent contentComp = new DocumentReferenceContentComponent();
    Attachment attachment = new Attachment();
    attachment.setContentType("text/plain;charset=utf-8");
    // String encodedString = Base64.getEncoder().encodeToString(obj.getBytes());
    attachment.setData(obj.getBytes());
    contentComp.setAttachment(attachment);
    contentList.add(contentComp);
    documentReference.setContent(contentList);

    // Set Doc Ref Context
    DocumentReferenceContextComponent contextComp = new DocumentReferenceContextComponent();
    List<Reference> encounterRefList = new ArrayList<Reference>();
    Reference encounterReference = new Reference();
    encounterReference.setReference("Encounter/" + encounterId);
    encounterRefList.add(encounterReference);
    contextComp.setEncounter(encounterRefList);
    Period period = new Period();
    period.setStart(new Date());
    period.setEnd(new Date());
    contextComp.setPeriod(period);
    documentReference.setContext(contextComp);

    logger.info(
        "DocumentReference Object===========>"
            + FhirContext.forR4().newJsonParser().encodeResourceToString(documentReference));

    return documentReference;
  }
}
