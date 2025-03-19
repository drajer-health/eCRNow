// package com.drajer.cdafromdstu2;
//
// import ca.uhn.fhir.model.api.IDatatype;
// import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
// import ca.uhn.fhir.model.dstu2.composite.CodingDt;
// import ca.uhn.fhir.model.dstu2.composite.ResourceReferenceDt;
// import ca.uhn.fhir.model.dstu2.resource.Observation;
// import ca.uhn.fhir.model.primitive.StringDt;
// import com.drajer.eca.model.MatchedTriggerCodes;
// import java.util.ArrayList;
// import java.util.HashSet;
// import java.util.List;
// import java.util.Set;
// import org.junit.jupiter.api.Test;
// import org.powermock.core.classloader.annotations.PowerMockIgnore;
// import org.junit.Assert;
// import org.junit.Ignore;
//
//
// public class CdaResultGeneratorTest extends BaseGenerator {
//
//  @Test
//  public void generateResultsSectionTest() {
//
//    StringBuilder codeXml = new StringBuilder(200);
//    StringBuilder valueXml = new StringBuilder(200);
//
// @PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*",
// "javax.management.*"})
// public class CdaResultGeneratorTest {
//    CodeableConceptDt codeableConceptDt = new CodeableConceptDt();
//    CodingDt codingDt = new CodingDt();
//    codingDt
//    List<CodingDt> codingDtList = new ArrayList<>();
//    codeableConceptDt.setCoding(codingDtList);
//    Observation observation = new Observation();
//    observation.setCode(codeableConceptDt);
//    List<Observation> results = new ArrayList<>();
//    results.add(observation);
//    dstu2FhirDataForPatient.setLabResults(results);
//    String actualXml =
//        Dstu2CdaResultGenerator.generateResultsSection(dstu2FhirDataForPatient, launchDetails);
//    System.out.println(actualXml);
//
//    /*  assert (codeXml.length() == 88);
//    assert (valueXml.toString().length() == 0);
//
//    StringBuilder codeXml1 = new StringBuilder(200);
//    StringBuilder valueXml1 = new StringBuilder(200);
//
//    Dstu2CdaResultGenerator.doesTriggerCodesMatchObservation(
//            getObservation2(), getMtc(), codeXml1, valueXml1);
//
//    assert (codeXml1.length() == 88);
//    assert (valueXml1.toString().length() == 127);*/
//  }
//
//  @Ignore
//  @Test
//  public void doesTriggerCodesMatchObservationTest() {
//
//    StringBuilder codeXml = new StringBuilder(200);
//    StringBuilder valueXml = new StringBuilder(200);
//
//    Dstu2CdaResultGenerator.doesTriggerCodesMatchObservation(
//        getObservation1(), getMtc(), codeXml, valueXml);
//
//    assert (codeXml.length() == 88);
//    assert (valueXml.toString().length() == 0);
//
//    StringBuilder codeXml1 = new StringBuilder(200);
//    StringBuilder valueXml1 = new StringBuilder(200);
//
//    Dstu2CdaResultGenerator.doesTriggerCodesMatchObservation(
//        getObservation2(), getMtc(), codeXml1, valueXml1);
//
//    assert (codeXml1.length() == 88);
//    assert (valueXml1.toString().length() == 127);
//  }
//
//  public MatchedTriggerCodes getMtc() {
//
//    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
//
//    Set<String> codes = new HashSet<String>();
//    codes.add("http://loinc.org|99999-1");
//    codes.add("http://loinc.org|88888-1");
//    codes.add("http://loinc.org|11111-1");
//
//    // Add the correct value
//    codes.add("http://loinc.org|91234-5");
//
//    // Add additional codes
//    codes.add("http://loinc.org|66666-1");
//    codes.add("http://loinc.org|91111-5");
//
//    mtc.setMatchedCodes(codes);
//    ;
//    return mtc;
//  }
//
//  @Test
//  public void addEntry() {
//    Observation observation = getObservation1();
//    String actualXml = Dstu2CdaResultGenerator.addEntry(observation, launchDetails, "", "");
//    Assert.assertNotNull(actualXml);
//  }
//
//  public Observation getObservation1() {
//
//    Observation obs = new Observation();
//
//    obs.setId("5474974");
//    CodingDt cd1 = new CodingDt();
//    cd1.setSystem("http://loinc.org");
//    cd1.setCode("91111-5");
//    cd1.setDisplay("DummyCode");
//
//    CodeableConceptDt cdt = new CodeableConceptDt();
//    cdt.addCoding(cd1);
//    obs.setCode(cdt);
//
//    StringDt st = new StringDt("Positive");
//    IDatatype dt = (IDatatype) st;
//
//    obs.setValue(dt);
//
//    ResourceReferenceDt pat = new ResourceReferenceDt();
//    pat.setReference("Patient/1");
//    obs.setSubject(pat);
//
//    return obs;
//  }
//
//  public Observation getObservation2() {
//
//    Observation obs = new Observation();
//
//    obs.setId("5474974");
//    CodingDt cd1 = new CodingDt();
//    cd1.setSystem("http://loinc.org");
//    cd1.setCode("91111-5");
//    cd1.setDisplay("DummyCode");
//
//    CodingDt cd3 = new CodingDt();
//    cd3.setSystem("http://loinc.org");
//    cd3.setCode("324324-5");
//    cd3.setDisplay("DummyCode");
//
//    CodeableConceptDt cdt = new CodeableConceptDt();
//    cdt.addCoding(cd1);
//    cdt.addCoding(cd3);
//    obs.setCode(cdt);
//
//    CodingDt cd2 = new CodingDt();
//    cd2.setSystem("http://loinc.org");
//    cd2.setCode("12345-5");
//    cd2.setDisplay("DummyCode");
//
//    CodeableConceptDt cdt1 = new CodeableConceptDt();
//    cdt1.addCoding(cd2);
//    obs.setValue(cdt1);
//
//    ResourceReferenceDt pat = new ResourceReferenceDt();
//    pat.setReference("Patient/1");
//    obs.setSubject(pat);
//
//    return obs;
//  }
// }
