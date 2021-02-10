package com.drajer.fhirecr;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Composition.SectionComponent;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.junit.Test;
import org.mockito.Mockito;

public class FhirGeneratorUtilsTest {

  private Resource resource;

  @Test
  public void getCodingTest() {
    Coding cd = FhirGeneratorUtils.getCoding("TestValue", "123456", "Covid-19");
    assertNotNull(cd);
    assertEquals("Covid-19", cd.getDisplay());
  }

  @Test
  public void getCodeableConceptTest() {
    CodeableConcept cd = FhirGeneratorUtils.getCodeableConcept("TestValue", "123456", "Covid-19");
    assertNotNull(cd);
    assertEquals("Covid-19", cd.getText());
    assertEquals("Covid-19", cd.getCoding().get(0).getDisplay());
  }

  @Test
  public void getReferenceTest() {
    resource = Mockito.mock(Resource.class);
    Reference ref = FhirGeneratorUtils.getReference(resource);
    assertNotNull(ref);
  }

  @Test
  public void getSectionComponentTest() {
    SectionComponent sc = FhirGeneratorUtils.getSectionComponent("TestValue", "123456", "Covid-19");
    assertNotNull(sc);
    assertNotNull(sc.getCode());
    assertEquals("Covid-19", sc.getCode().getText());
  }

  @Test
  public void getTextTest() {
    CodeableConcept codeableConcept = new CodeableConcept();
    codeableConcept.setText("Covid-19 disease");
    String text = FhirGeneratorUtils.getText(codeableConcept);
    assertEquals("Covid-19 disease", text);
  }

  @Test
  public void getTextWithEmptyTextTest() {
    CodeableConcept codeableConcept = new CodeableConcept();
    List<Coding> codeList = new ArrayList<>();
    Coding coding = new Coding();
    coding.setDisplay("Diagnosed with Covid-19");
    codeList.add(coding);
    codeableConcept.setCoding(codeList);

    String text = FhirGeneratorUtils.getText(codeableConcept);
    assertEquals("Diagnosed with Covid-19", text);
  }

  @Test
  public void getTextWithMultipleDisplayTest() {
    CodeableConcept codeableConcept = new CodeableConcept();
    List<Coding> codeList = new ArrayList<>();
    Coding coding = new Coding();
    coding.setDisplay("Has Covid-19");

    Coding coding2 = new Coding();
    coding2.setDisplay("Has Cough");
    codeList.add(coding);
    codeList.add(coding2);
    codeableConcept.setCoding(codeList);

    String text = FhirGeneratorUtils.getText(codeableConcept);
    assertEquals("Has Covid-19,Has Cough", text);
  }

  @Test
  public void getReasonForVisitNarrativeTextTest() {
    Encounter enc = new Encounter();
    List<CodeableConcept> codeableConceptList = new ArrayList<>();
    CodeableConcept codeableConcept = new CodeableConcept();
    codeableConcept.setText("Diagnosed with Covid-19");
    codeableConceptList.add(codeableConcept);

    enc.setReasonCode(codeableConceptList);
    String text = FhirGeneratorUtils.getReasonForVisitNarrativeText(enc);
    assertEquals("Diagnosed with Covid-19", text);
  }
}
