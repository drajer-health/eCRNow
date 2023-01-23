package com.drajer.cdafromr4;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.Instant;
import java.util.Date;
import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Type;
import org.junit.Test;

public class CdaHeaderGeneratorTest {

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
