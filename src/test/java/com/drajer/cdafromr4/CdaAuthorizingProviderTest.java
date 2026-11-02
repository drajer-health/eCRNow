package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
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
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.PractitionerRole;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.junit.Test;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

/**
 * Authorizing Provider for lab results (GitHub #1079), mapped per the
 * AuthorizingOrdering_xPath_FHIR_Mapping sheet, rows 2-9.
 */
public class CdaAuthorizingProviderTest {

  private static final String ORGANIZER =
      "//organizer[templateId[@root='2.16.840.1.113883.10.20.22.4.1']]";
  private static final String ROLE =
      ORGANIZER + "/participant[@typeCode='RESP']/participantRole[@classCode='PROV']";

  // ------------------------------ CDA output ------------------------------

  @Test
  public void directPractitioner_populatesEverySheetXPath() throws Exception {
    R4FhirData data = new R4FhirData();
    addToBundle(data, serviceRequest("sr-1", "Practitioner/pr-1"));
    data.addPractitionersList(List.of(fullPractitioner("pr-1")));

    Document doc = organizerWith(xmlFor("ServiceRequest/sr-1", data));

    // Row 2: name (given + family)
    assertEquals("Jane", xpath(doc, ROLE + "/playingEntity/name/given"));
    assertEquals("Smith", xpath(doc, ROLE + "/playingEntity/name/family"));
    // Row 3: NPI
    assertEquals("1234567893", xpath(doc, ROLE + "/id[@root='2.16.840.1.113883.4.6']/@extension"));
    // Rows 4-7: address
    assertEquals("100 Main St", xpath(doc, ROLE + "/addr/streetAddressLine"));
    assertEquals("Olympia", xpath(doc, ROLE + "/addr/city"));
    assertEquals("WA", xpath(doc, ROLE + "/addr/state"));
    assertEquals("98501", xpath(doc, ROLE + "/addr/postalCode"));
    // Row 8: work phone
    assertEquals("tel:(360)555-0100", xpath(doc, ROLE + "/telecom[@use='WP'][1]/@value"));
    // Row 9: fax
    assertEquals("fax:(360)555-0199", xpath(doc, ROLE + "/telecom[contains(@value,'fax')]/@value"));
  }

  @Test
  public void practitionerRoleRequester_resolvesThePractitioner() throws Exception {
    R4FhirData data = new R4FhirData();
    addToBundle(data, serviceRequest("sr-1", "PractitionerRole/role-1"));
    data.addPractitionerRoles(List.of(role("role-1", "Practitioner/pr-1")));
    data.addPractitionersList(List.of(fullPractitioner("pr-1")));

    Document doc = organizerWith(xmlFor("ServiceRequest/sr-1", data));

    assertEquals("Smith", xpath(doc, ROLE + "/playingEntity/name/family"));
    assertEquals("1234567893", xpath(doc, ROLE + "/id[@root='2.16.840.1.113883.4.6']/@extension"));
  }

  @Test
  public void phoneWithoutUse_defaultsToWorkPhone() throws Exception {
    Practitioner pr = new Practitioner();
    pr.setId("pr-1");
    pr.addName(new HumanName().setFamily("Lee"));
    pr.addTelecom(new ContactPoint().setSystem(ContactPointSystem.PHONE).setValue("3605550111"));
    R4FhirData data = dataWith(pr);

    Document doc = organizerWith(xmlFor("ServiceRequest/sr-1", data));

    assertEquals("tel:(360)555-0111", xpath(doc, ROLE + "/telecom[@use='WP'][1]/@value"));
  }

  @Test
  public void workPhoneIsPreferredOverHomePhone() {
    List<ContactPoint> telecoms = new ArrayList<>();
    telecoms.add(phone("3605550122", ContactPointUse.HOME));
    telecoms.add(phone("3605550100", ContactPointUse.WORK));

    String xml = CdaResultGenerator.getAuthorizingProviderTelecomXml(telecoms);

    assertTrue(xml, xml.contains("tel:(360)555-0100"));
    assertFalse(xml, xml.contains("555-0122"));
  }

  @Test
  public void missingNpiAddressAndTelecom_areLeftOut() throws Exception {
    Practitioner pr = new Practitioner();
    pr.setId("pr-1");
    pr.addName(new HumanName().setFamily("Lee").addGiven("Ann"));

    String xml = xmlFor("ServiceRequest/sr-1", dataWith(pr));
    Document doc = organizerWith(xml);

    assertEquals("Lee", xpath(doc, ROLE + "/playingEntity/name/family"));
    assertFalse(xml, xml.contains("<id"));
    assertFalse(xml, xml.contains("<addr"));
    assertFalse(xml, xml.contains("<telecom"));
  }

  @Test
  public void practitionerWithNeitherNpiNorName_producesNoParticipant() {
    Practitioner pr = new Practitioner();
    pr.setId("pr-1");

    assertEquals("", xmlFor("ServiceRequest/sr-1", dataWith(pr)));
  }

  @Test
  public void unresolvableRequesterOrServiceRequest_producesNoParticipant() {
    R4FhirData data = new R4FhirData();
    addToBundle(data, serviceRequest("sr-1", "Practitioner/missing"));

    assertEquals("", xmlFor("ServiceRequest/sr-1", data));
    assertEquals("", xmlFor("ServiceRequest/unknown", data));
  }

  @Test
  public void firstServiceRequestWithAResolvableRequesterWins() {
    R4FhirData data = new R4FhirData();
    ServiceRequest noRequester = new ServiceRequest();
    noRequester.setId("sr-0");
    addToBundle(data, noRequester, serviceRequest("sr-1", "Practitioner/pr-1"));
    data.addPractitionersList(List.of(fullPractitioner("pr-1")));

    List<Reference> basedOn =
        List.of(new Reference("ServiceRequest/sr-0"), new Reference("ServiceRequest/sr-1"));

    Practitioner found = CdaResultGenerator.findAuthorizingProvider(basedOn, data);

    assertNotNull(found);
    assertEquals("pr-1", found.getIdElement().getIdPart());
  }

  @Test
  public void dataRequirementServiceRequestsAreAlsoUsed() {
    R4FhirData data = new R4FhirData();
    data.setServiceRequests(new ArrayList<>(List.of(serviceRequest("sr-1", "Practitioner/pr-1"))));
    data.addPractitionersList(List.of(fullPractitioner("pr-1")));

    assertTrue(xmlFor("ServiceRequest/sr-1", data).contains("typeCode=\"RESP\""));
  }

  // --------------------------- data conversion ----------------------------

  @Test
  public void converter_keepsBasedOnServiceRequestsOutOfPlanOfTreatment() {
    KarProcessingData kd = new KarProcessingData();
    ServiceRequest sr = serviceRequest("sr-1", "PractitionerRole/role-1");
    PractitionerRole role = role("role-1", "Practitioner/pr-1");
    kd.addResourceByType(ResourceType.ServiceRequest, sr);
    kd.addResourceByType(ResourceType.PractitionerRole, role);

    R4FhirData data = new R4FhirData();
    Bundle bundle = new Bundle();
    R3ToR2DataConverterUtils.addSecondaryResources(
        null, bundle, data, new LaunchDetails(), kd, null, new HashMap<>());
    data.setData(bundle);

    assertTrue("used by Plan of Treatment", data.getServiceRequests().isEmpty());
    assertSame(sr, CdaResultGenerator.findInBundle(data, ResourceType.ServiceRequest, "sr-1"));
    assertSame(role, data.getPractitionerRoleById("role-1"));
    assertTrue("PractitionerRole also in the bundle", data.getData().getEntry().size() == 2);
  }

  @Test
  public void bundleLookupsReturnNullForUnknownIdsOrNoBundle() {
    R4FhirData data = new R4FhirData();
    assertNull(CdaResultGenerator.findInBundle(data, ResourceType.ServiceRequest, "x"));
    addToBundle(data, serviceRequest("sr-1", "Practitioner/pr-1"));
    assertNull(CdaResultGenerator.findInBundle(data, ResourceType.ServiceRequest, "nope"));
    assertNull(CdaResultGenerator.findInBundle(data, ResourceType.ServiceRequest, null));
    assertNull(CdaResultGenerator.findInBundle(data, ResourceType.Practitioner, "sr-1"));

    data.addPractitionerRoles(List.of(role("role-1", "Practitioner/pr-1")));
    assertNotNull(data.getPractitionerRoleById("role-1"));
    assertNull(data.getPractitionerRoleById("role-10"));
    assertNull(data.getPractitionerRoleById(null));
  }

  @Test
  public void converter_doesNotAddDuplicatesToTheBundle() {
    KarProcessingData kd = new KarProcessingData();
    ServiceRequest sr = serviceRequest("sr-1", "Practitioner/pr-1");
    kd.addResourceByType(ResourceType.ServiceRequest, sr);
    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(sr); // already added as a data-requirement ServiceRequest

    R3ToR2DataConverterUtils.addSecondaryResources(
        null, bundle, new R4FhirData(), new LaunchDetails(), kd, null, new HashMap<>());

    assertEquals(1, bundle.getEntry().size());
  }

  // -------------------------------- helpers -------------------------------

  private static String xmlFor(String basedOnRef, R4FhirData data) {
    return CdaResultGenerator.getAuthorizingProviderParticipantXml(
        new ArrayList<>(Collections.singletonList(new Reference(basedOnRef))), data);
  }

  /** Adds resources to the R4FhirData bundle, as the converter does. */
  private static void addToBundle(R4FhirData data, Resource... resources) {
    if (data.getData() == null) {
      data.setData(new Bundle());
    }
    for (Resource r : resources) {
      data.getData().addEntry().setResource(r);
    }
  }

  private static R4FhirData dataWith(Practitioner pr) {
    R4FhirData data = new R4FhirData();
    addToBundle(data, serviceRequest("sr-1", "Practitioner/pr-1"));
    data.addPractitionersList(List.of(pr));
    return data;
  }

  private static ServiceRequest serviceRequest(String id, String requester) {
    ServiceRequest sr = new ServiceRequest();
    sr.setId(id);
    sr.setRequester(new Reference(requester));
    return sr;
  }

  private static PractitionerRole role(String id, String practitioner) {
    PractitionerRole role = new PractitionerRole();
    role.setId(id);
    role.setPractitioner(new Reference(practitioner));
    return role;
  }

  private static ContactPoint phone(String value, ContactPointUse use) {
    return new ContactPoint().setSystem(ContactPointSystem.PHONE).setValue(value).setUse(use);
  }

  private static Practitioner fullPractitioner(String id) {
    Practitioner pr = new Practitioner();
    pr.setId(id);
    pr.addIdentifier().setSystem("http://hl7.org/fhir/sid/us-npi").setValue("1234567893");
    pr.addName(new HumanName().setFamily("Smith").addGiven("Jane"));
    pr.addAddress(
        new Address()
            .addLine("100 Main St")
            .setCity("Olympia")
            .setState("WA")
            .setPostalCode("98501")
            .setUse(Address.AddressUse.WORK));
    pr.addTelecom(phone("3605550100", ContactPointUse.WORK));
    pr.addTelecom(new ContactPoint().setSystem(ContactPointSystem.FAX).setValue("3605550199"));
    return pr;
  }

  /** Wraps the participant in a Result Organizer so the sheet's XPaths can be evaluated. */
  private static Document organizerWith(String participantXml) throws Exception {
    assertFalse("participant XML expected", participantXml.isEmpty());
    String xml =
        "<organizer><templateId root=\"2.16.840.1.113883.10.20.22.4.1\"/>"
            + participantXml
            + "</organizer>";
    return DocumentBuilderFactory.newInstance()
        .newDocumentBuilder()
        .parse(new InputSource(new StringReader(xml)));
  }

  private static String xpath(Document doc, String expression) throws Exception {
    return ((String)
            XPathFactory.newInstance().newXPath().evaluate(expression, doc, XPathConstants.STRING))
        .trim();
  }
}
