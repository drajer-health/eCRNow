package com.drajer.fhirecr;

import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.List;
import org.hl7.fhir.r4.model.Composition;
import org.hl7.fhir.r4.model.Composition.SectionComponent;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.StringType;

public class EicrCompositionGenerator {

  private EicrCompositionGenerator() {}

  public static Composition convertR4FhirBundletoCdaEicr(R4FhirData data) {

    Composition comp = new Composition();

    // Add version number extension.
    comp.addExtension(
        FhirGeneratorConstants.COMP_CLIN_DOC_VERSION_NUM_URL,
        new StringType(String.valueOf(FhirGeneratorConstants.VERSION_NUM)));

    // Add Type
    comp.setType(
        FhirGeneratorUtils.getCodeableConcept(
            FhirGeneratorConstants.LOINC_CS_URL,
            FhirGeneratorConstants.COMP_TYPE_CODE,
            FhirGeneratorConstants.COMP_TYPE_CODE_DISPLAY));

    // Setup Patient.
    comp.getSubject().setResource(data.getPatient());

    // Setup Encounter
    if (data.getEncounter() != null) comp.getEncounter().setResource(data.getEncounter());

    // Add Authors.
    List<Reference> auths = new ArrayList<>();

    // Add organization
    if (data.getOrganization() != null)
      auths.add(FhirGeneratorUtils.getReference(data.getOrganization()));

    if (data.getPractitioner() != null)
      auths.add(FhirGeneratorUtils.getReference(data.getPractitioner()));

    comp.setAuthor(auths);

    // Add Reason for Visit Section;
    SectionComponent rvs =
        FhirGeneratorUtils.getSectionComponent(
            FhirGeneratorConstants.LOINC_CS_URL,
            FhirGeneratorConstants.REASON_FOR_VISIT_CODE,
            FhirGeneratorConstants.REASON_FOR_VISIT_CODE_DISPLAY);

    // Set the Section Code Display to be the narrative text.
    rvs.getCode()
        .setTextElement(
            new StringType(FhirGeneratorUtils.getReasonForVisitNarrativeText(data.getEncounter())));
    comp.getSection().add(rvs);

    return comp;
  }
}
