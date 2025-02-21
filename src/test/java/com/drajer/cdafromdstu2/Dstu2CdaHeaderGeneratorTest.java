package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import ca.uhn.fhir.model.dstu2.resource.*;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;

public class Dstu2CdaHeaderGeneratorTest extends BaseGenerator {
    private static String PRACTITIONER_RES_FILE = "CdaDstuTestData/Practitioner/Practitioner_4122622.json";
    private static String LOCATION_RES_FILE = "CdaDstuTestData/Location/Location.json";
    private static String ORGANIZATION_RES_FILE = "CdaDstuTestData/Organization/Organization.json";
    private static String ENCOUNTER_RES_FILE = "CdaDstuTestData/Encounter/Encounter_97953900.json";

    @Test
    public void createCdaHeaderTest() throws IOException {
        String actualXml = Dstu2CdaHeaderGenerator.createCdaHeader(dstu2FhirDataForPatient, launchDetails, eicr, "1");
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void getPractitionerXmlTest() throws IOException {
        String expectedXml = "<?xml version=\"1.0\"?>\n" +
                "<ClinicalDocument xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n" +
                " xmlns=\"urn:hl7-org:v3\"\n" +
                " xmlns:cda=\"urn:hl7-org:v3\"\n" +
                " xmlns:sdtc=\"urn:hl7-org:sdtc\">\n" +
                "<realmCode code=\"US\"/>\n" +
                "<typeId root=\"2.16.840.1.113883.1.3\" extension=\"POCD_HD000040\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.1.1\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.1.1\" extension=\"2015-08-01\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.15.2\" extension=\"2022-05-01\"/>\n" +
                "<id root=\"0467838c-f38c-4778-b36e-093a81a9356f\"/>\n" +
                "<code code=\"55751-2\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Initial Public Health Case Report\"/>\n" +
                "<title>Initial Public Health Case Report</title>\n" +
                "<effectiveTime value=\"20250220002305+0000\"/>\n" +
                "<confidentialityCode code=\"N\" codeSystem=\"2.16.840.1.113883.5.25\"/>\n" +
                "<languageCode code=\"en-US\"/>\n" +
                "<setId root=\"2.16.840.1.113883.1.1.1.1\" extension=\"1\"/>\n" +
                "<versionNumber value=\"1\"/>";
        dstu2FhirDataForPatient.setPractitioner(null);
        String actualXml = Dstu2CdaHeaderGenerator.createCdaHeader(dstu2FhirDataForPatient, launchDetails, eicr, "1");
        System.out.println(actualXml);
        //assertXmlEquals(expectedXml, actualXml);
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void getDeceasedXmlTest() throws IOException {
        String expectedXml = "<sdtc:deceasedInd value=\"false\"/>";
        String actualXml = Dstu2CdaHeaderGenerator.getDeceasedXml(patient);
        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void getParticipantXmlTest() throws IOException {
        Practitioner practitioner = loadResourceDataFromFile(Practitioner.class, PRACTITIONER_RES_FILE);
        String expectedXml = "<id root=\"2.16.840.1.113883.4.6\"/>\n" +
                "<addr>\n" +
                "<streetAddressLine nullFlavor=\"NI\"/>\n" +
                "<city nullFlavor=\"NI\"/>\n" +
                "<state nullFlavor=\"NI\"/>\n" +
                "<postalCode nullFlavor=\"NI\"/>\n" +
                "<country nullFlavor=\"NI\"/>\n" +
                "</addr>\n" +
                "<telecom nullFlavor=\"NI\"/>\n" +
                "<assignedPerson>\n" +
                "<name>\n" +
                "<given>Physician - Hospitalist</given>\n" +
                "<given>Physician - Hospitalist</given>\n" +
                "<family>Cerner Test</family>\n" +
                "</name>\n" +
                "</assignedPerson>";
        String actualXml = Dstu2CdaHeaderGenerator.getPractitionerXml(practitioner);
        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void getLocationXmlTest() throws IOException {
        Location location = loadResourceDataFromFile(Location.class, LOCATION_RES_FILE);
        Organization organization = loadResourceDataFromFile(Organization.class, ORGANIZATION_RES_FILE);
        String expectedXml = "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"Location/1\"/>\n" +
                "<code nullFlavor=\"NI\"/>\n" +
                "<location>\n" +
                "<addr>\n" +
                "<streetAddressLine>Galapagosweg 91, Building A</streetAddressLine>\n" +
                "<city>Den Burg</city>\n" +
                "<state nullFlavor=\"NI\"/>\n" +
                "<postalCode>9105 PZ</postalCode>\n" +
                "<country>NLD</country>\n" +
                "</addr>\n" +
                "</location>";
        String actualXml = Dstu2CdaHeaderGenerator.getLocationXml(location, organization, launchDetails);
        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void getAuthorXmlTest() throws IOException {
        Encounter encounter = loadResourceDataFromFile(Encounter.class, ENCOUNTER_RES_FILE);
        Organization organization = loadResourceDataFromFile(Organization.class, ORGANIZATION_RES_FILE);
        Practitioner practitioner = loadResourceDataFromFile(Practitioner.class, PRACTITIONER_RES_FILE);

        String expectedXml = "<author>\n" +
                "<time value=\"20200512134056+0000\"/>\n" +
                "<assignedAuthor>\n" +
                "<id root=\"2.16.840.1.113883.4.6\"/>\n" +
                "<addr>\n" +
                "<streetAddressLine nullFlavor=\"NI\"/>\n" +
                "<city nullFlavor=\"NI\"/>\n" +
                "<state nullFlavor=\"NI\"/>\n" +
                "<postalCode nullFlavor=\"NI\"/>\n" +
                "<country nullFlavor=\"NI\"/>\n" +
                "</addr>\n" +
                "<telecom nullFlavor=\"NI\"/>\n" +
                "<assignedPerson>\n" +
                "<name>\n" +
                "<given>Physician - Hospitalist</given>\n" +
                "<given>Physician - Hospitalist</given>\n" +
                "<family>Cerner Test</family>\n" +
                "</name>\n" +
                "</assignedPerson>\n" +
                "</assignedAuthor>\n" +
                "</author>";
        String actualXml = Dstu2CdaHeaderGenerator.getAuthorXml(practitioner, encounter);
        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void getCustodianXmlTest() throws IOException {
        Encounter encounter = loadResourceDataFromFile(Encounter.class, ENCOUNTER_RES_FILE);
        Organization organization = loadResourceDataFromFile(Organization.class, ORGANIZATION_RES_FILE);
        Practitioner practitioner = loadResourceDataFromFile(Practitioner.class, PRACTITIONER_RES_FILE);
        Location location = loadResourceDataFromFile(Location.class, LOCATION_RES_FILE);

        String expectedXml = "<custodian>\n" +
                "<assignedCustodian>\n" +
                "<representedCustodianOrganization>\n" +
                "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"Organization/hl7\"/>\n" +
                "<name>Health Level Seven International</name>\n" +
                "<telecom value=\"tel:+1(734)677-7777\" use=\"HP\"/>\n" +
                "</representedCustodianOrganization>\n" +
                "</assignedCustodian>\n" +
                "</custodian>";
        String actualXml = Dstu2CdaHeaderGenerator.getCustodianXml(organization, location, launchDetails);
        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void getEncompassingEncounterXMLTest() throws IOException {
        Encounter encounter = loadResourceDataFromFile(Encounter.class, ENCOUNTER_RES_FILE);
        Organization organization = loadResourceDataFromFile(Organization.class, ORGANIZATION_RES_FILE);
        Practitioner practitioner = loadResourceDataFromFile(Practitioner.class, PRACTITIONER_RES_FILE);
        Location location = loadResourceDataFromFile(Location.class, LOCATION_RES_FILE);

        String expectedXml = "<componentOf>\n" +
                "<encompassingEncounter>\n" +
                "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"97953900\"/>\n" +
                "<id root=\"5.5.5.5.5.5.\" extension=\"98314717\"/>\n" +
                "<code code=\"IMP\" codeSystem=\"2.16.840.1.113883.5.4\"/>\n" +
                "<effectiveTime>\n" +
                "<low value=\"20200512134056+0000\"/>\n" +
                "<high nullFlavor=\"NI\"/>\n" +
                "</effectiveTime>\n" +
                "<responsibleParty>\n" +
                "<assignedEntity>\n" +
                "<id root=\"2.16.840.1.113883.4.6\"/>\n" +
                "<addr>\n" +
                "<streetAddressLine nullFlavor=\"NI\"/>\n" +
                "<city nullFlavor=\"NI\"/>\n" +
                "<state nullFlavor=\"NI\"/>\n" +
                "<postalCode nullFlavor=\"NI\"/>\n" +
                "<country nullFlavor=\"NI\"/>\n" +
                "</addr>\n" +
                "<telecom nullFlavor=\"NI\"/>\n" +
                "<assignedPerson>\n" +
                "<name>\n" +
                "<given>Physician - Hospitalist</given>\n" +
                "<given>Physician - Hospitalist</given>\n" +
                "<family>Cerner Test</family>\n" +
                "</name>\n" +
                "</assignedPerson>\n" +
                "<representedOrganization>\n" +
                "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"Organization/hl7\"/>\n" +
                "<name>Health Level Seven International</name>\n" +
                "<telecom value=\"tel:+1(734)677-7777\" use=\"HP\"/>\n" +
                "</representedOrganization>\n" +
                "</assignedEntity>\n" +
                "</responsibleParty>\n" +
                "<location>\n" +
                "<healthCareFacility>\n" +
                "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"Location/1\"/>\n" +
                "<code nullFlavor=\"NI\"/>\n" +
                "<location>\n" +
                "<addr>\n" +
                "<streetAddressLine>Galapagosweg 91, Building A</streetAddressLine>\n" +
                "<city>Den Burg</city>\n" +
                "<state nullFlavor=\"NI\"/>\n" +
                "<postalCode>9105 PZ</postalCode>\n" +
                "<country>NLD</country>\n" +
                "</addr>\n" +
                "</location>\n" +
                "<serviceProviderOrganization>\n" +
                "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"Organization/hl7\"/>\n" +
                "<name>Health Level Seven International</name>\n" +
                "<telecom value=\"tel:+1(734)677-7777\" use=\"HP\"/>\n" +
                "</serviceProviderOrganization>\n" +
                "</healthCareFacility>\n" +
                "</location>\n" +
                "</encompassingEncounter>\n" +
                "</componentOf>";
        String actualXml = Dstu2CdaHeaderGenerator.getEncompassingEncounter(encounter, practitioner, location, organization, launchDetails);
        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void getPatientDetailsXMLTest() throws IOException {
        Encounter encounter = loadResourceDataFromFile(Encounter.class, ENCOUNTER_RES_FILE);
        Organization organization = loadResourceDataFromFile(Organization.class, ORGANIZATION_RES_FILE);
        Practitioner practitioner = loadResourceDataFromFile(Practitioner.class, PRACTITIONER_RES_FILE);
        Location location = loadResourceDataFromFile(Location.class, LOCATION_RES_FILE);

        String expectedXml = "<recordTarget>\n" +
                "<patientRole>\n" +
                "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"Patient/a-11287.E-4237\"/>\n" +
                "<addr>\n" +
                "<streetAddressLine>2221 HOME STREET</streetAddressLine>\n" +
                "<city>SALT LAKE CITY</city>\n" +
                "<state>UT</state>\n" +
                "<postalCode>84101</postalCode>\n" +
                "<country>USA</country>\n" +
                "</addr>\n" +
                "<telecom value=\"tel:(555)555-5006\" use=\"HP\"/>\n" +
                "<patient>\n" +
                "<name>\n" +
                "<given>Jill</given>\n" +
                "<family>Test</family>\n" +
                "</name>\n" +
                "<administrativeGenderCode code=\"F\" codeSystem=\"2.16.840.1.113883.5.1\"/>\n" +
                "<birthTime value=\"20201027\"/>\n" +
                "<sdtc:deceasedInd value=\"false\"/>\n" +
                "<raceCode nullFlavor=\"NI\"/>\n" +
                "<ethnicGroupCode nullFlavor=\"NI\"/>\n" +
                "<guardian>\n" +
                "<addr>\n" +
                "<streetAddressLine>534 Erewhon St</streetAddressLine>\n" +
                "<city>PleasantVille</city>\n" +
                "<state>Vic</state>\n" +
                "<postalCode>3999</postalCode>\n" +
                "<country nullFlavor=\"NI\"/>\n" +
                "</addr>\n" +
                "<telecom value=\"tel:(323)799-8327\" use=\"HP\"/>\n" +
                "<guardianPerson>\n" +
                "<name>\n" +
                "<given>B�n�dicte</given>\n" +
                "<family>du March�</family>\n" +
                "</name>\n" +
                "</guardianPerson>\n" +
                "</guardian>\n" +
                "<languageCommunication>\n" +
                "<languageCode code=\"en\"/>\n" +
                "</languageCommunication>\n" +
                "</patient>\n" +
                "</patientRole>\n" +
                "</recordTarget>";
        String actualXml = Dstu2CdaHeaderGenerator.getPatientDetails(dstu2FhirDataForPatient, patient, launchDetails);
        assertXmlEquals(expectedXml, actualXml);
        CodingDt codingDt = new CodingDt();
        codingDt.setCode("ABC");
    }
}
