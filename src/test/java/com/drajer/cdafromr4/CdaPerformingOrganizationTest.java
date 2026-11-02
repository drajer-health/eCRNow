package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointSystem;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.StringType;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

/**
 * Performing Organization for lab results (GitHub #1079), mapped per the
 * AuthorizingOrdering_xPath_FHIR_Mapping sheet, rows 16-18.
 */
public class CdaPerformingOrganizationTest {

  private static final String ORG =
      "//observation[templateId[@root='2.16.840.1.113883.10.20.22.4.2']]"
          + "/performer/assignedEntity/representedOrganization";

  private LaunchDetails details;
  private R4FhirData data;

  @Before
  public void setUp() throws Exception {
    details = new LaunchDetails();
    details.setAssigningAuthorityId("2.16.840.1.113883.1.1.1.1");
    details.setStatus(
        new ObjectMapper().writeValueAsString(new PatientExecutionState("pat-1", "enc-1")));
    data = new R4FhirData();
    data.setData(new Bundle());
  }

  @Test
  public void observationOrganizationPerformer_populatesEverySheetXPath() throws Exception {
    data.setOrganizationList(new ArrayList<>(List.of(lab("lab-1", "State Public Health Lab"))));

    Document doc = observation(refs("Organization/lab-1"), null);

    // Row 16: name
    assertEquals("State Public Health Lab", xpath(doc, ORG + "/name"));
    // Row 17: address (street + city)
    assertEquals("1610 NE 150th St", xpath(doc, ORG + "/addr/streetAddressLine"));
    assertEquals("Shoreline", xpath(doc, ORG + "/addr/city"));
    // Row 18: phone
    assertEquals("tel:(206)555-0150", xpath(doc, ORG + "/telecom[contains(@value,'tel')]/@value"));
    // assignedEntity/id is required by the CDA schema
    assertEquals("NI", xpath(doc, "//observation/performer/assignedEntity/id/@nullFlavor"));
  }

  @Test
  public void fallsBackToTheDiagnosticReportPerformer() throws Exception {
    data.setOrganizationList(new ArrayList<>(List.of(lab("lab-dr", "Report Lab"))));

    // The observation only has a practitioner performer; the report names the lab.
    Document doc = observation(refs("Practitioner/unknown"), refs("Organization/lab-dr"));

    assertEquals("Report Lab", xpath(doc, ORG + "/name"));
  }

  @Test
  public void observationPerformerWinsOverReportPerformer() throws Exception {
    data.setOrganizationList(
        new ArrayList<>(List.of(lab("lab-obs", "Observation Lab"), lab("lab-dr", "Report Lab"))));

    Document doc = observation(refs("Organization/lab-obs"), refs("Organization/lab-dr"));

    assertEquals("Observation Lab", xpath(doc, ORG + "/name"));
  }

  @Test
  public void noOrganizationPerformer_noPerformerElement() throws Exception {
    String xml = observationXml(refs("Practitioner/unknown"), refs("Practitioner/other"));

    assertFalse(xml, xml.contains("<performer"));
  }

  @Test
  public void performerComesBeforeAuthorAndReferenceRangeInTheObservation() throws Exception {
    data.setOrganizationList(new ArrayList<>(List.of(lab("lab-1", "Lab"))));

    String xml = observationXml(refs("Organization/lab-1"), null);

    int performer = xml.indexOf("<performer");
    assertTrue(xml, performer > xml.indexOf("<value") && performer > 0);
    int author = xml.indexOf("<author");
    assertTrue("performer must precede author", author < 0 || performer < author);
  }

  @Test
  public void organizationWithoutNameAddressOrPhone_isLeftOut() {
    Organization empty = new Organization();
    empty.setId("lab-empty");
    data.setOrganizationList(new ArrayList<>(List.of(empty)));

    assertEquals(
        "",
        CdaResultGenerator.getPerformingOrganizationXml(
            refs("Organization/lab-empty"), null, data));
  }

  @Test
  public void findOrganization_skipsNonOrganizationReferences() {
    data.setOrganizationList(new ArrayList<>(List.of(lab("lab-1", "Lab"))));

    assertNull(CdaResultGenerator.findOrganization(refs("Practitioner/lab-1"), data));
    assertNotNull(CdaResultGenerator.findOrganization(refs("Organization/lab-1"), data));
    assertNull(CdaResultGenerator.findOrganization(null, data));
    assertEquals("", CdaResultGenerator.getPerformingOrganizationXml(null, null, null));
  }

  @Test
  public void existingOverloadWithoutReportPerformers_stillAddsTheObservationLab()
      throws Exception {
    data.setOrganizationList(new ArrayList<>(List.of(lab("lab-1", "Lab One"))));

    String xml =
        CdaResultGenerator.getXmlForObservationComponent(
            details,
            loinc(),
            new StringType("Detected"),
            "obs-1",
            new DateTimeType("2026-10-01"),
            null,
            1,
            refs("Organization/lab-1"),
            data,
            "CDA_R31",
            null);

    assertEquals("Lab One", xpath(parse(xml), ORG + "/name"));
  }

  // -------------------------------- helpers -------------------------------

  private Document observation(List<Reference> obsPerformers, List<Reference> reportPerformers)
      throws Exception {
    return parse(observationXml(obsPerformers, reportPerformers));
  }

  private String observationXml(List<Reference> obsPerformers, List<Reference> reportPerformers) {
    return CdaResultGenerator.getXmlForObservationComponent(
        details,
        loinc(),
        new StringType("Detected"),
        "obs-1",
        new DateTimeType("2026-10-01"),
        null,
        1,
        obsPerformers,
        data,
        "CDA_R31",
        null,
        reportPerformers);
  }

  private static CodeableConcept loinc() {
    return new CodeableConcept()
        .addCoding(new Coding("http://loinc.org", "94500-6", "SARS-CoV-2 RNA"));
  }

  private static List<Reference> refs(String... references) {
    List<Reference> list = new ArrayList<>();
    for (String r : references) {
      list.add(new Reference(r));
    }
    return list;
  }

  private static Organization lab(String id, String name) {
    Organization org = new Organization();
    org.setId(id);
    org.setName(name);
    org.addTelecom(new ContactPoint().setSystem(ContactPointSystem.PHONE).setValue("2065550150"));
    org.addAddress(
        new Address()
            .addLine("1610 NE 150th St")
            .setCity("Shoreline")
            .setState("WA")
            .setPostalCode("98155"));
    return org;
  }

  private static Document parse(String componentXml) throws Exception {
    return DocumentBuilderFactory.newInstance()
        .newDocumentBuilder()
        .parse(new InputSource(new StringReader("<root>" + componentXml + "</root>")));
  }

  private static String xpath(Document doc, String expression) throws Exception {
    return ((String)
            XPathFactory.newInstance().newXPath().evaluate(expression, doc, XPathConstants.STRING))
        .trim();
  }
}
