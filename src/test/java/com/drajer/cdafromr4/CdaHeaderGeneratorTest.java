package com.drajer.cdafromr4;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.sof.model.R4FhirData;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import org.hl7.fhir.r4.model.*;
import org.junit.Test;

public class CdaHeaderGeneratorTest extends BaseGeneratorTest {

  @Test
  public void testGetParticipantXml() {
    R4FhirData r4FhirData1 = new R4FhirData();
    Bundle bundle = loadBundleFromFile(PATIENT_RES_FILENAME);
    r4FhirData1 = createR4Resource(r4FhirData1, bundle);

    Coding coding = new Coding();
    coding.setCode("N");
    coding.setSystem(CdaGeneratorConstants.FHIR_CONTACT_RELATIONSHIP_CODESYSTEM);

    List<Patient.ContactComponent> contactComponents = r4FhirData1.getPatient().getContact();

    String expectedXml =
        "<participant typeCode=\"IND\">\r\n"
            + "<associatedEntity classCode=\"NOK\">\r\n"
            + "<addr use=\"HP\">\r\n"
            + "<streetAddressLine>534 Erewhon St</streetAddressLine>\r\n"
            + "<city>PleasantVille</city>\r\n"
            + "<state>Vic</state>\r\n"
            + "<postalCode>3999</postalCode>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<telecom value=\"tel:(323)799-8327\"/>\r\n"
            + "<associatedPerson>\r\n"
            + "<name>\r\n"
            + "<given>B�n�dicte</given>\r\n"
            + "<family>du March�</family>\r\n"
            + "</name>\r\n"
            + "</associatedPerson>\r\n"
            + "</associatedEntity>\r\n"
            + "</participant>\r\n"
            + "";

    String actualXml = CdaHeaderGenerator.getParticipantXml(contactComponents.get(0), coding);

    assertThat(actualXml).isNotNull();

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetTranslatableCodeableConceptCoding() {
    List<CodeableConcept> codeableConcepts = new ArrayList<>();

    Coding coding = new Coding();
    coding.setCode("N");
    coding.setSystem(CdaGeneratorConstants.FHIR_CONTACT_RELATIONSHIP_CODESYSTEM);

    codeableConcepts.add(new CodeableConcept().setCoding(Collections.singletonList(coding)));

    Coding actual = CdaHeaderGenerator.getTranslatableCodeableConceptCoding(codeableConcepts);

    assertThat(actual).isNotNull();
  }

  @Test
  public void testGetTranslatableCoding() {

    Coding coding = new Coding();
    coding.setCode("N");
    coding.setSystem(CdaGeneratorConstants.FHIR_CONTACT_RELATIONSHIP_CODESYSTEM);

    Coding actual = CdaHeaderGenerator.getTranslatableCoding(Collections.singletonList(coding));

    assertThat(actual).isNotNull();
  }

  @Test
  public void testGetDeceasedXml() {

    Patient p = getPatientData();

    String decXml = CdaHeaderGenerator.getDeceasedXml(p);

    assertTrue(decXml.contains("false"));

    decXml = "";
    BooleanType btf = new BooleanType(false);
    p.setDeceased(btf);

    decXml = CdaHeaderGenerator.getDeceasedXml(p);
    assertTrue(decXml.contains("false"));

    BooleanType btt = new BooleanType(true);
    p.setDeceased(btt);
    decXml = CdaHeaderGenerator.getDeceasedXml(p);
    assertTrue(decXml.contains("true"));
    assertTrue(decXml.contains("sdtc:deceasedInd value="));
    assertTrue(decXml.contains("sdtc:deceasedTime nullFlavor="));
    assertTrue(decXml.contains("NI"));

    DateTimeType dtt = new DateTimeType();
    dtt.setValue(Date.from(Instant.now()));
    p.setDeceased(dtt);

    decXml = CdaHeaderGenerator.getDeceasedXml(p);
    assertTrue(decXml.contains("true"));
    assertTrue(decXml.contains("sdtc:deceasedInd value="));
    assertFalse(decXml.contains("sdtc:deceasedTime nullFlavor="));
    assertTrue(decXml.contains("sdtc:deceasedTime value="));

    BooleanType st = new BooleanType();
    p.setDeceased(st);
    decXml = CdaHeaderGenerator.getDeceasedXml(p);
    assertTrue(decXml.contains("false"));

    DateTimeType dtt1 = new DateTimeType();
    p.setDeceased(dtt1);

    decXml = CdaHeaderGenerator.getDeceasedXml(p);
    assertTrue(decXml.contains("true"));
    assertTrue(decXml.contains("sdtc:deceasedInd value="));
    assertTrue(decXml.contains("sdtc:deceasedTime nullFlavor="));
  }

  public Patient getPatientData() {

    Patient p = new Patient();

    p.setId("5474974");

    Extension ext1 = new Extension();
    ext1.setUrl("http://hl7.org/fhir/us/core/StructureDefinition/us-core-race");

    Extension subext1 = new Extension();
    subext1.setUrl("ombCategory");

    Coding st1 = new Coding();
    st1.setSystem("http://hl7.org/fhir/v3/NullFlavor");
    st1.setCode("UNK");
    st1.setDisplay("Unknown");

    Type tp1 = (Type) st1;
    subext1.setValue(tp1);
    ext1.addExtension(subext1);

    Extension ext = new Extension();
    ext.setUrl("http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity");
    Extension subext = new Extension();
    subext.setUrl("text");

    StringType st = new StringType("Unavailable");
    Type tp = (Type) st;
    subext.setValue(tp);
    ext.addExtension(subext);

    p.addExtension(ext1);
    p.addExtension(ext);

    return p;
  }
}
