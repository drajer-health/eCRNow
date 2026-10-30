package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointSystem;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointUse;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.PractitionerRole;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

/**
 * Ordering Facility for lab results (GitHub #1079), mapped per the
 * AuthorizingOrdering_xPath_FHIR_Mapping sheet, rows 10-15. The facility goes into
 * encompassingEncounter/location/healthCareFacility, and only when the encounter has neither a
 * Location nor a service provider Organization.
 */
public class CdaOrderingFacilityTest {

  private static final String FACILITY = "//encompassingEncounter/location/healthCareFacility";

  private LaunchDetails details;
  private R4FhirData data;
  private Encounter encounter;

  @Before
  public void setUp() throws Exception {
    details = new LaunchDetails();
    details.setAssigningAuthorityId("2.16.840.1.113883.1.1.1.1");
    details.setStatus(
        new ObjectMapper().writeValueAsString(new PatientExecutionState("pat-1", "enc-1")));
    data = new R4FhirData();
    data.setData(new Bundle());
    encounter = new Encounter();
    encounter.setId("enc-1");
    data.setEncounter(encounter);
  }

  @Test
  public void practitionerRoleOrganization_populatesEverySheetXPath() throws Exception {
    orderedVia("PractitionerRole/role-1");
    data.addPractitionerRoles(List.of(role("role-1", "Organization/fac-1")));
    orgs(facility("fac-1", "Northside Clinic"));

    Document doc = header();

    // Row 10: name
    assertEquals("Northside Clinic", xpath(doc, FACILITY + "/location/name"));
    // Rows 11-14: street, city, state, ZIP
    assertEquals("12 Main St", xpath(doc, FACILITY + "/location/addr/streetAddressLine"));
    assertEquals("Olympia", xpath(doc, FACILITY + "/location/addr/city"));
    assertEquals("WA", xpath(doc, FACILITY + "/location/addr/state"));
    assertEquals("98501", xpath(doc, FACILITY + "/location/addr/postalCode"));
    // Row 15: phone
    String phone =
        xpath(
            doc, FACILITY + "/serviceProviderOrganization/telecom[contains(@value,'tel')]/@value");
    assertTrue(phone, phone.startsWith("tel:") && phone.contains("555"));
    assertEquals("Northside Clinic", xpath(doc, FACILITY + "/serviceProviderOrganization/name"));
  }

  @Test
  public void organizationRequester_isTheFacility() throws Exception {
    orderedVia("Organization/fac-2");
    orgs(facility("fac-2", "Requesting Org"));

    assertEquals("Requesting Org", xpath(header(), FACILITY + "/location/name"));
  }

  @Test
  public void plainPractitionerRequester_leavesTheFacilityAsNullFlavor() throws Exception {
    orderedVia("Practitioner/pr-1");

    Document doc = header();

    // Same nullFlavor output as an encounter with no location or organization today
    assertEquals("", xpath(doc, FACILITY + "/location/name"));
    assertEquals("NI", xpath(doc, FACILITY + "/location/addr/city/@nullFlavor"));
    assertEquals("NI", xpath(doc, FACILITY + "/serviceProviderOrganization/telecom/@nullFlavor"));
    assertEquals("Unknown", xpath(doc, FACILITY + "/serviceProviderOrganization/name"));
  }

  @Test
  public void encounterServiceProvider_winsOverTheOrderingFacility() throws Exception {
    orderedVia("Organization/fac-1");
    Organization serviceProvider = facility("sp-1", "Encounter Hospital");
    orgs(facility("fac-1", "Northside Clinic"), serviceProvider);
    encounter.setServiceProvider(new Reference("Organization/sp-1"));
    data.setOrganization(serviceProvider);

    Document doc = header();

    assertEquals("Encounter Hospital", xpath(doc, FACILITY + "/location/name"));
    assertEquals("Encounter Hospital", xpath(doc, FACILITY + "/serviceProviderOrganization/name"));
  }

  @Test
  public void encounterLocation_winsOverTheOrderingFacility() throws Exception {
    orderedVia("Organization/fac-1");
    orgs(facility("fac-1", "Northside Clinic"));
    Location loc = new Location();
    loc.setId("loc-1");
    loc.setName("ER Room 4");
    loc.setAddress(address());
    data.setLocationList(new ArrayList<>(List.of(loc)));

    Document doc = header();

    assertEquals("ER Room 4", xpath(doc, FACILITY + "/location/name"));
    // A location is present, so the ordering facility is not used for the service provider either
    assertEquals("Unknown", xpath(doc, FACILITY + "/serviceProviderOrganization/name"));
  }

  @Test
  public void serviceProviderThatWasNotRetrieved_usesTheOrderingFacility() throws Exception {
    orderedVia("Organization/fac-1");
    orgs(facility("fac-1", "Northside Clinic"));
    encounter.setServiceProvider(new Reference("Organization/missing"));

    assertEquals("Northside Clinic", xpath(header(), FACILITY + "/location/name"));
  }

  @Test
  public void labObservationBasedOn_isUsedWhenThereIsNoReport() throws Exception {
    data.addServiceRequests(List.of(serviceRequest("sr-obs", "Organization/fac-obs")));
    orgs(facility("fac-obs", "Observation Facility"));
    Observation obs = new Observation();
    obs.setId("obs-1");
    obs.addBasedOn(new Reference("ServiceRequest/sr-obs"));
    data.setLabResults(new ArrayList<>(List.of(obs)));

    assertEquals("Observation Facility", xpath(header(), FACILITY + "/location/name"));
  }

  @Test
  public void firstResultWithAFacilityWins() {
    // First report's order has only a Practitioner; the second report has the facility.
    data.addServiceRequests(
        List.of(
            serviceRequest("sr-a", "Practitioner/pr-1"),
            serviceRequest("sr-b", "Organization/fac-b"),
            serviceRequest("sr-c", "Organization/fac-c")));
    Organization b = facility("fac-b", "B");
    orgs(b, facility("fac-c", "C"));
    data.setDiagReports(
        new ArrayList<>(
            List.of(report("dr-a", "sr-a"), report("dr-b", "sr-b"), report("dr-c", "sr-c"))));

    assertSame(b, CdaResultGenerator.findOrderingFacility(data));
  }

  @Test
  public void basedOnServiceRequestInTheBundle_isFound() {
    data.getData().addEntry().setResource(serviceRequest("sr-bundle", "Organization/fac-1"));
    Organization fac = facility("fac-1", "Bundle Facility");
    orgs(fac);
    data.setDiagReports(new ArrayList<>(List.of(report("dr-1", "sr-bundle"))));

    assertSame(fac, CdaResultGenerator.findOrderingFacility(data));
  }

  @Test
  public void noFacility_cases() {
    assertNull(CdaResultGenerator.findOrderingFacility(null));
    assertNull(CdaResultGenerator.findOrderingFacility(data));

    // role without an organization, and an organization that was not retrieved
    data.addPractitionerRoles(List.of(role("role-x", null)));
    assertNull(
        CdaResultGenerator.resolveRequesterOrganization(
            new Reference("PractitionerRole/role-x"), data));
    assertNull(
        CdaResultGenerator.resolveRequesterOrganization(
            new Reference("Organization/unknown"), data));
    assertNull(CdaResultGenerator.resolveRequesterOrganization(new Reference(), data));
    assertNull(CdaResultGenerator.resolveRequesterOrganization(null, data));
  }

  @Test
  public void hasEncounterServiceProvider_requiresARetrievedOrganization() {
    assertFalse(CdaHeaderGenerator.hasEncounterServiceProvider(data));
    encounter.setServiceProvider(new Reference("Organization/sp-1"));
    assertFalse(CdaHeaderGenerator.hasEncounterServiceProvider(data));
    orgs(facility("sp-1", "SP"));
    assertTrue(CdaHeaderGenerator.hasEncounterServiceProvider(data));
    data.setEncounter(null);
    assertFalse(CdaHeaderGenerator.hasEncounterServiceProvider(data));
  }

  // ---------------------------------------------------------------------------------------

  /** One DiagnosticReport whose basedOn ServiceRequest has the given requester. */
  private void orderedVia(String requester) {
    data.addServiceRequests(List.of(serviceRequest("sr-1", requester)));
    data.setDiagReports(new ArrayList<>(List.of(report("dr-1", "sr-1"))));
  }

  private void orgs(Organization... orgs) {
    data.setOrganizationList(new ArrayList<>(List.of(orgs)));
  }

  private Document header() throws Exception {
    String xml =
        CdaHeaderGenerator.getEncompassingEncounter(encounter, new HashMap<>(), details, data);
    return DocumentBuilderFactory.newInstance()
        .newDocumentBuilder()
        .parse(new InputSource(new StringReader(xml)));
  }

  private static String xpath(Document doc, String expr) throws Exception {
    return (String)
        XPathFactory.newInstance().newXPath().evaluate(expr, doc, XPathConstants.STRING);
  }

  private static DiagnosticReport report(String id, String serviceRequestId) {
    DiagnosticReport dr = new DiagnosticReport();
    dr.setId(id);
    dr.addBasedOn(new Reference("ServiceRequest/" + serviceRequestId));
    return dr;
  }

  private static ServiceRequest serviceRequest(String id, String requester) {
    ServiceRequest sr = new ServiceRequest();
    sr.setId(id);
    sr.setRequester(new Reference(requester));
    return sr;
  }

  private static PractitionerRole role(String id, String organization) {
    PractitionerRole role = new PractitionerRole();
    role.setId(id);
    if (organization != null) {
      role.setOrganization(new Reference(organization));
    }
    return role;
  }

  private static Organization facility(String id, String name) {
    Organization org = new Organization();
    org.setId(id);
    org.setName(name);
    org.addAddress(address());
    org.addTelecom(
        new ContactPoint()
            .setSystem(ContactPointSystem.PHONE)
            .setUse(ContactPointUse.WORK)
            .setValue("(360)555-0110"));
    return org;
  }

  private static Address address() {
    return new Address()
        .addLine("12 Main St")
        .setCity("Olympia")
        .setState("WA")
        .setPostalCode("98501");
  }
}
