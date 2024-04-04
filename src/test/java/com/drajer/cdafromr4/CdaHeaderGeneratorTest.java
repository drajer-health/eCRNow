package com.drajer.cdafromr4;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.EicrRRService;
import com.drajer.sof.model.R4FhirData;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Patient.ContactComponent;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Type;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class, ActionRepo.class})
public class CdaHeaderGeneratorTest extends BaseGeneratorTest {

  static final String PATIENT_RES_FILENAME = "CdaTestData/patient/Patient_resource.json";

  static final String LOCATION_FILENAME = "CdaTestData/patient/Location.json";

  static final String ORGANIZATION_FILENAME = "CdaTestData/patient/Organization.json";
  static final String PRACTITIONER_FILENAME = "CdaTestData/Practitioner/Practitioner.json";

  @Test
  public void testCreateCdaHeader() {
    // Set up test data
    R4FhirData r4Data = new R4FhirData();
    Bundle bundle = loadBundleFromFile(PATIENT_RES_FILENAME);
    r4Data = createR4Resource(r4Data, bundle);
    r4Data.setData(bundle);

    ActionRepo actionRepoMock = mock(ActionRepo.class);
    PowerMockito.mockStatic(ActionRepo.class);
    when(ActionRepo.getInstance()).thenReturn(actionRepoMock);

    EicrRRService eicrRRServiceMock = mock(EicrRRService.class);
    when(actionRepoMock.getEicrRRService()).thenReturn(eicrRRServiceMock);
    when(eicrRRServiceMock.getMaxVersionId(any(Eicr.class))).thenReturn(1);

    String actualXml = CdaHeaderGenerator.createCdaHeader(r4Data, launchDetails, eicr);

    assertNotNull(actualXml);
  }

  @Test
  public void testGenerateXmlForDetailedEthnicityCode() {
    List<Extension> extensions = new ArrayList<>();
    extensions.add(
        createExtension(
            "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity",
            "2178-2",
            "Latin American"));

    String expectedXml =
        "<sdtc:ethnicGroupCode code=\"2178-2\" codeSystem=\"2.16.840.1.113883.6.238\" codeSystemName=\"Race &amp; Ethnicity - CDC\" displayName=\"Latin American\"/>\r\n";
    String actualXml =
        CdaHeaderGenerator.generateXmlForDetailedRaceAndEthnicityCodes(
            extensions,
            CdaGeneratorConstants.FHIR_USCORE_ETHNICITY_EXT_URL,
            CdaGeneratorConstants.OMB_ETHNICITY_DETAILED_URL,
            CdaGeneratorConstants.SDTC_DETAILED_ETHNIC_GROUP_CODE);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateXmlForDetailedRaceCode() {
    List<Extension> extensions = new ArrayList<>();
    extensions.add(
        createExtension(
            "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race",
            "1072-8",
            "Mexican American Indian"));

    String expectedXml =
        "<sdtc:raceCode code=\"1072-8\" codeSystem=\"2.16.840.1.113883.6.238\" codeSystemName=\"Race &amp; Ethnicity - CDC\" displayName=\"Mexican American Indian\"/>\r\n";
    String actualXml =
        CdaHeaderGenerator.generateXmlForDetailedRaceAndEthnicityCodes(
            extensions,
            CdaGeneratorConstants.FHIR_USCORE_RACE_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_DETAILED_URL,
            CdaGeneratorConstants.SDTC_DETAILED_RACE_CODE);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateXmlForDetailedUnknownRaceCode() {
    List<Extension> extensions = new ArrayList<>();
    extensions.add(
        createExtension(
            "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race",
            "ASKU",
            "asked but unknown"));

    String expectedXml = "<sdtc:raceCode nullFlavor=\"ASKU\"/>";
    String actualXml =
        CdaHeaderGenerator.generateXmlForDetailedRaceAndEthnicityCodes(
            extensions,
            CdaGeneratorConstants.FHIR_USCORE_RACE_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_DETAILED_URL,
            CdaGeneratorConstants.SDTC_DETAILED_RACE_CODE);

    assertXmlEquals(expectedXml, actualXml);
  }

  private Extension createExtension(String extUrl, String code, String display) {
    Extension extension = new Extension();
    Coding codingValue = new Coding();
    codingValue.setSystem("urn:oid:2.16.840.1.113883.6.238");
    codingValue.setCode(code);
    codingValue.setDisplay(display);
    extension.setUrl(extUrl);
    extension.setValue(codingValue);
    return extension;
  }

  @Test
  public void testGetParticipantXml() {
    R4FhirData r4FhirData1 = new R4FhirData();
    Bundle bundle = loadBundleFromFile(PATIENT_RES_FILENAME);
    r4FhirData1 = createR4Resource(r4FhirData1, bundle);

    Coding coding = new Coding();
    coding.setCode("N");
    coding.setSystem(CdaGeneratorConstants.FHIR_CONTACT_RELATIONSHIP_CODESYSTEM);

    List<ContactComponent> contactComponents = r4FhirData1.getPatient().getContact();

    String expectedXml =
        "<participant typeCode=\"IND\">\r\n"
            + "<associatedEntity classCode=\"NOK\">\r\n"
            + "<addr use=\"HP\">\r\n"
            + "<streetAddressLine>534 Erewhon St</streetAddressLine>\r\n"
            + "<city>PleasantVille</city>\r\n"
            + "<county>Rainbow</county>\r\n"
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
            + "</participant>";

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
  public void testGetPractitionerXml() {

    Practitioner practitioner =
        (Practitioner) loadResourceDataFromFile(Practitioner.class, PRACTITIONER_FILENAME);

    String expectedXml =
        "<id root=\"2.16.840.1.113883.4.6\"/>\r\n"
            + "<addr use=\"HP\">\r\n"
            + "<streetAddressLine>534 Erewhon St</streetAddressLine>\r\n"
            + "<city>PleasantVille</city>\r\n"
            + "<state>Vic</state>\r\n"
            + "<postalCode>3999</postalCode>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<assignedPerson>\r\n"
            + "<name>\r\n"
            + "<given>Adam</given>\r\n"
            + "<family>Careful</family>\r\n"
            + "</name>\r\n"
            + "</assignedPerson>\r\n"
            + "";
    String actualXml = CdaFhirUtilities.getPractitionerXml(practitioner);

    assertThat(actualXml).isNotNull();

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateLocationXml_withLocation() {
    Location location = (Location) loadResourceDataFromFile(Location.class, LOCATION_FILENAME);
    Organization organization =
        (Organization) loadResourceDataFromFile(Organization.class, ORGANIZATION_FILENAME);

    String expectedXml =
        "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"1\"/>\r\n"
            + "<code nullFlavor=\"NI\"/>\r\n"
            + "<location>\r\n"
            + "<addr use=\"WP\">\r\n"
            + "<streetAddressLine>Galapagosweg 91, Building A</streetAddressLine>\r\n"
            + "<city>Den Burg</city>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode>9105 PZ</postalCode>\r\n"
            + "<country>NLD</country>\r\n"
            + "</addr>\r\n"
            + "</location>\r\n";

    String actualXml = CdaHeaderGenerator.getLocationXml(location, organization, launchDetails);

    assertThat(actualXml).isNotNull();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateLocationXml_withOrganization() {
    Organization organization =
        (Organization) loadResourceDataFromFile(Organization.class, ORGANIZATION_FILENAME);

    String expectedXml =
        "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"hl7\"/>\r\n"
            + "<code nullFlavor=\"NI\"/>\r\n"
            + "<location>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</location>";

    String actualXml = CdaHeaderGenerator.getLocationXml(null, organization, launchDetails);

    assertThat(actualXml).isNotNull();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateLocationXml_noLocationOrOrganization() {
    String expectedXml =
        "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"Unknown\"/>\r\n"
            + "<code nullFlavor=\"NI\"/>\r\n"
            + "<location>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</location>\r\n";

    String actualXml = CdaHeaderGenerator.getLocationXml(null, null, launchDetails);

    assertThat(actualXml).isNotNull();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetAdditionalAuthorXml() {

    String expectedXml =
        "<author>\r\n"
            + "<time value=\"20230503103307+0000\"/><assignedAuthor>\r\n"
            + "<id root=\"b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9\"/><addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<assignedAuthoringDevice>\r\n"
            + "<manufacturerModelName displayName=\"manfacture\"/>\r\n"
            + "<softwareName displayName=\"1.0v\"/>\r\n"
            + "</assignedAuthoringDevice>\r\n"
            + "</assignedAuthor>\r\n"
            + "</author>\r\n"
            + "";

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForEffectiveTime(any(), any()))
        .thenReturn("<time value=\"20230503103307+0000\"/>");
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid())
        .thenReturn("<id root=\"b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9\"/>");
    String actualXml = CdaHeaderGenerator.getAdditionalAuthorXml("manfacture", "1.0v");

    assertThat(actualXml).isNotNull();

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateAuthorXml_noPractitionerData() {
    String expectedXml =
        "<author>\r\n"
            + "<time value=\"20230503103307+0000\"/><assignedAuthor>\r\n"
            + "<id root=\"2.16.840.1.113883.4.6\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<assignedPerson>\r\n"
            + "<name>\r\n"
            + "<given nullFlavor=\"NI\"/>\r\n"
            + "<family nullFlavor=\"NI\"/>\r\n"
            + "</name>\r\n"
            + "</assignedPerson>\r\n"
            + "</assignedAuthor>\r\n"
            + "</author>\r\n";

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForEffectiveTime(any(), any()))
        .thenReturn("<time value=\"20230503103307+0000\"/>");

    String actualXml = CdaHeaderGenerator.getAuthorXml(r4FhirData, null, null);

    assertThat(actualXml).isNotNull();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateAuthorXml_withPractitionerData() {
    HashMap<V3ParticipationType, List<Practitioner>> practitionerMap = new HashMap<>();
    R4FhirData r4FhirData = new R4FhirData();
    Practitioner practitioner = new Practitioner();
    List<Practitioner> practitionerList = new ArrayList<>();
    practitionerList.add(practitioner);
    practitionerMap.put(V3ParticipationType.PPRF, practitionerList);
    Encounter encounter =
        (Encounter)
            loadResourceDataFromFile(
                Encounter.class, "CdaTestData/Encounter/Encounter_withOneParticipant.json");

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForEffectiveTime(any(), any()))
        .thenReturn("<time value=\"20230503103307+0000\"/>");

    String expectedXml =
        "<author>\r\n"
            + "<time value=\"20200512134056+0000\"/>\r\n"
            + "<assignedAuthor>\r\n"
            + "<id root=\"2.16.840.1.113883.4.6\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<assignedPerson>\r\n"
            + "<name>\r\n"
            + "<given nullFlavor=\"NI\"/>\r\n"
            + "<family nullFlavor=\"NI\"/>\r\n"
            + "</name>\r\n"
            + "</assignedPerson>\r\n"
            + "</assignedAuthor>\r\n"
            + "</author>\r\n";

    String actualXml = CdaHeaderGenerator.getAuthorXml(r4FhirData, encounter, practitionerMap);

    assertThat(actualXml).isNotNull();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetAuthorXmlWith_noEncounterData() {

    HashMap<V3ParticipationType, List<Practitioner>> practMap =
        new HashMap<V3ParticipationType, List<Practitioner>>();

    Practitioner pr = new Practitioner();
    List<Practitioner> practList = new ArrayList<Practitioner>();
    practList.add(pr);
    practMap.put(V3ParticipationType.ATND, practList);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForEffectiveTime(any(), any()))
        .thenReturn("<time value=\"20230503103307+0000\"/>");

    String expectedXml =
        "<author>\r\n"
            + "<time value=\"20230503103307+0000\"/><assignedAuthor>\r\n"
            + "<id root=\"2.16.840.1.113883.4.6\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<assignedPerson>\r\n"
            + "<name>\r\n"
            + "<given nullFlavor=\"NI\"/>\r\n"
            + "<family nullFlavor=\"NI\"/>\r\n"
            + "</name>\r\n"
            + "</assignedPerson>\r\n"
            + "</assignedAuthor>\r\n"
            + "</author>\r\n"
            + "";

    String actualXml = CdaHeaderGenerator.getAuthorXml(r4FhirData, null, practMap);

    assertThat(actualXml).isNotNull();

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetAuthorXmlWith_noEncounter() {

    HashMap<V3ParticipationType, List<Practitioner>> practMap =
        new HashMap<V3ParticipationType, List<Practitioner>>();
    Practitioner pr = new Practitioner();
    List<Practitioner> practList = new ArrayList<Practitioner>();
    practList.add(pr);
    practMap.put(V3ParticipationType.SPRF, practList);

    String actual = CdaHeaderGenerator.getAuthorXml(r4FhirData, null, practMap);

    assertThat(actual).isNotNull();
  }

  @Test
  public void testGenerateOrganizationXml_withOrganizationData() {
    Organization organization =
        (Organization) loadResourceDataFromFile(Organization.class, ORGANIZATION_FILENAME);

    String expectedXml =
        "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"hl7\"/>\r\n"
            + "<name>Health Level Seven International</name>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n";

    String actualXml = CdaHeaderGenerator.getOrganizationXml(organization, launchDetails, false);

    assertThat(actualXml).isNotNull();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateOrganizationXml_withNoOrganizationData() {
    String expectedXml =
        "<id nullFlavor=\"NI\"/>\r\n"
            + "<name>Unknown</name>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>";
    String actualXml = CdaHeaderGenerator.getOrganizationXml(null, launchDetails, false);

    assertThat(actualXml).isNotNull();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetCustodianXml() {
    R4FhirData r4FhirData1 = new R4FhirData();

    Organization organization =
        (Organization) loadResourceDataFromFile(Organization.class, ORGANIZATION_FILENAME);
    r4FhirData1.setOrganization(organization);

    String expectedXml =
        "<custodian>\r\n"
            + "<assignedCustodian>\r\n"
            + "<representedCustodianOrganization>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"hl7\"/>\r\n"
            + "<name>Health Level Seven International</name>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</representedCustodianOrganization>\r\n"
            + "</assignedCustodian>\r\n"
            + "</custodian>\r\n"
            + "";
    String actualXml = CdaHeaderGenerator.getCustodianXml(launchDetails, r4FhirData1);

    assertThat(actualXml).isNotNull();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetEncompassingEncounter() {
    HashMap<V3ParticipationType, List<Practitioner>> practMap =
        new HashMap<V3ParticipationType, List<Practitioner>>();
    R4FhirData r4FhirData1 = new R4FhirData();
    Practitioner pr = new Practitioner();
    List<Practitioner> practList = new ArrayList<Practitioner>();
    practList.add(pr);
    practMap.put(V3ParticipationType.PPRF, practList);

    Location location = (Location) loadResourceDataFromFile(Location.class, LOCATION_FILENAME);

    Organization organization =
        (Organization) loadResourceDataFromFile(Organization.class, ORGANIZATION_FILENAME);

    Encounter encounter =
        (Encounter)
            loadResourceDataFromFile(
                Encounter.class, "CdaTestData/Encounter/Encounter_withOneParticipant.json");

    r4FhirData1.setOrganization(organization);
    r4FhirData1.setLocation(location);

    String expectedXml =
        "<componentOf>\r\n"
            + "<encompassingEncounter>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"97953900\"/>\r\n"
            + "<id root=\"2.16.840.1.113883.4.4\" extension=\"98314717\"/>\r\n"
            + "<code code=\"IMP\" codeSystem=\"2.16.840.1.113883.5.4\" codeSystemName=\"v3-ActCode\" displayName=\"inpatient encounter\"></code>\r\n"
            + "<effectiveTime>\r\n"
            + "<low value=\"20200512134056+0000\"/>\r\n"
            + "<high value=\"20200512134057+0000\"/>\r\n"
            + "</effectiveTime>\r\n"
            + "<responsibleParty>\r\n"
            + "<assignedEntity>\r\n"
            + "<id root=\"2.16.840.1.113883.4.6\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<assignedPerson>\r\n"
            + "<name>\r\n"
            + "<given nullFlavor=\"NI\"/>\r\n"
            + "<family nullFlavor=\"NI\"/>\r\n"
            + "</name>\r\n"
            + "</assignedPerson>\r\n"
            + "<representedOrganization>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"hl7\"/>\r\n"
            + "<name>Health Level Seven International</name>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</representedOrganization>\r\n"
            + "</assignedEntity>\r\n"
            + "</responsibleParty>\r\n"
            + "<location>\r\n"
            + "<healthCareFacility>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"1\"/>\r\n"
            + "<code nullFlavor=\"NI\"/>\r\n"
            + "<location>\r\n"
            + "<addr use=\"WP\">\r\n"
            + "<streetAddressLine>Galapagosweg 91, Building A</streetAddressLine>\r\n"
            + "<city>Den Burg</city>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode>9105 PZ</postalCode>\r\n"
            + "<country>NLD</country>\r\n"
            + "</addr>\r\n"
            + "</location>\r\n"
            + "<serviceProviderOrganization>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"hl7\"/>\r\n"
            + "<name>Health Level Seven International</name>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</serviceProviderOrganization>\r\n"
            + "</healthCareFacility>\r\n"
            + "</location>\r\n"
            + "</encompassingEncounter>\r\n"
            + "</componentOf>\r\n"
            + "";

    String actualXml =
        CdaHeaderGenerator.getEncompassingEncounter(
            encounter, practMap, launchDetails, r4FhirData1);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetEncompassingEncounter_withNoEncounterData() {
    HashMap<V3ParticipationType, List<Practitioner>> practMap =
        new HashMap<V3ParticipationType, List<Practitioner>>();
    R4FhirData r4FhirData1 = new R4FhirData();
    Practitioner pr = new Practitioner();
    List<Practitioner> practList = new ArrayList<Practitioner>();
    practList.add(pr);
    practMap.put(V3ParticipationType.PPRF, practList);

    Location location = (Location) loadResourceDataFromFile(Location.class, LOCATION_FILENAME);

    Organization organization =
        (Organization) loadResourceDataFromFile(Organization.class, ORGANIZATION_FILENAME);

    r4FhirData1.setOrganization(organization);
    r4FhirData1.setLocation(location);

    String expectedXml =
        "<componentOf>\r\n"
            + "<encompassingEncounter>\r\n"
            + "<id root=\"b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9\"/><code nullFlavor=\"NI\"/>\r\n"
            + "<effectiveTime nullFlavor=\"NI\"/>\r\n"
            + "<responsibleParty>\r\n"
            + "<assignedEntity>\r\n"
            + "<id root=\"2.16.840.1.113883.4.6\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<assignedPerson>\r\n"
            + "<name>\r\n"
            + "<given nullFlavor=\"NI\"/>\r\n"
            + "<family nullFlavor=\"NI\"/>\r\n"
            + "</name>\r\n"
            + "</assignedPerson>\r\n"
            + "<representedOrganization>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"hl7\"/>\r\n"
            + "<name>Health Level Seven International</name>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</representedOrganization>\r\n"
            + "</assignedEntity>\r\n"
            + "</responsibleParty>\r\n"
            + "<location>\r\n"
            + "<healthCareFacility>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"1\"/>\r\n"
            + "<code nullFlavor=\"NI\"/>\r\n"
            + "<location>\r\n"
            + "<addr use=\"WP\">\r\n"
            + "<streetAddressLine>Galapagosweg 91, Building A</streetAddressLine>\r\n"
            + "<city>Den Burg</city>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode>9105 PZ</postalCode>\r\n"
            + "<country>NLD</country>\r\n"
            + "</addr>\r\n"
            + "</location>\r\n"
            + "<serviceProviderOrganization>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"hl7\"/>\r\n"
            + "<name>Health Level Seven International</name>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</serviceProviderOrganization>\r\n"
            + "</healthCareFacility>\r\n"
            + "</location>\r\n"
            + "</encompassingEncounter>\r\n"
            + "</componentOf>\r\n"
            + "";

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid())
        .thenReturn("<id root=\"b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9\"/>");

    String actualXml =
        CdaHeaderGenerator.getEncompassingEncounter(null, practMap, launchDetails, r4FhirData1);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetSortedPractitionerList() {
    R4FhirData r4FhirData1 = new R4FhirData();
    Encounter encounter =
        (Encounter)
            loadResourceDataFromFile(
                Encounter.class, "CdaTestData/Encounter/Encounter_withOneParticipant.json");
    r4FhirData1.setEncounter(encounter);
    Practitioner practitioner = new Practitioner();
    practitioner.setId("11817978");
    r4FhirData1.setPractitionersList(Collections.singletonList(practitioner));

    HashMap<V3ParticipationType, List<Practitioner>> actual =
        CdaHeaderGenerator.getSortedPractitionerList(r4FhirData1);

    assertNotNull(actual);
  }

  @Test
  public void testGetXmlForRelevantPractitioner() {
    HashMap<V3ParticipationType, List<Practitioner>> practMap =
        new HashMap<V3ParticipationType, List<Practitioner>>();
    Practitioner pr = new Practitioner();
    List<Practitioner> practList = new ArrayList<Practitioner>();
    practList.add(pr);
    practMap.put(V3ParticipationType.PPRF, practList);
    String result = CdaHeaderGenerator.getXmlForRelevantPractitioner(practMap);
    assertNotNull(result);
    assertTrue(result.length() > 0);

    practMap.clear();

    practMap.put(V3ParticipationType.ATND, practList);

    String result1 = CdaHeaderGenerator.getXmlForRelevantPractitioner(practMap);
    assertNotNull(result1);
    assertTrue(result1.length() > 0);

    practMap.clear();

    practMap.put(V3ParticipationType.SPRF, practList);

    String result2 = CdaHeaderGenerator.getXmlForRelevantPractitioner(practMap);
    assertNotNull(result2);
    assertTrue(result2.length() > 0);
  }

  @Test
  public void testGetXmlForRelevantPractitioner_withNoPractitioner() {

    String result1 = CdaHeaderGenerator.getXmlForRelevantPractitioner(null);
    assertNotNull(result1);
    assertTrue(result1.length() > 0);
  }

  @Test
  public void testGetXmlForAllRelevantPractitioners() {
    String result = CdaHeaderGenerator.getXmlForAllRelevantPractitioners(new HashMap<>());
    assertEquals("", result);

    // Test case 2: input with one practitioner of type ATND
    Practitioner practitioner1 = new Practitioner();
    List<Practitioner> practitioners1 = new ArrayList<>();
    practitioners1.add(practitioner1);
    HashMap<V3ParticipationType, List<Practitioner>> input2 = new HashMap<>();
    input2.put(V3ParticipationType.ATND, practitioners1);
    result = CdaHeaderGenerator.getXmlForAllRelevantPractitioners(input2);
    assertTrue(result.contains("<encounterParticipant typeCode=\"ATND\">"));
    assertTrue(result.contains("<assignedEntity>"));
    assertTrue(result.contains("</assignedEntity>"));
    assertTrue(result.contains("</encounterParticipant>"));

    // Test case 3: input with multiple practitioners of different types
    Practitioner practitioner2 = new Practitioner();
    Practitioner practitioner3 = new Practitioner();
    List<Practitioner> practitioners2 = new ArrayList<>();
    List<Practitioner> practitioners3 = new ArrayList<>();
    practitioners2.add(practitioner2);
    practitioners3.add(practitioner3);
    HashMap<V3ParticipationType, List<Practitioner>> input3 = new HashMap<>();
    input3.put(V3ParticipationType.CON, practitioners2);
    input3.put(V3ParticipationType.REF, practitioners3);
    result = CdaHeaderGenerator.getXmlForAllRelevantPractitioners(input3);
    assertTrue(result.contains("<encounterParticipant typeCode=\"CON\">"));
    assertTrue(result.contains("<encounterParticipant typeCode=\"REF\">"));
    assertTrue(result.contains("<assignedEntity>"));
    assertTrue(result.contains("</assignedEntity>"));
    assertTrue(result.contains("</encounterParticipant>"));
  }

  @Test
  public void testGetPatientDetails() {
    R4FhirData r4FhirData1 = new R4FhirData();
    Bundle bundle = loadBundleFromFile(PATIENT_RES_FILENAME);
    r4FhirData1 = createR4Resource(r4FhirData1, bundle);

    String expectedXml =
        "<recordTarget>\r\n"
            + "<patientRole>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"123456\"/>\r\n"
            + "<addr use=\"HP\">\r\n"
            + "<streetAddressLine>2221 HOME STREET</streetAddressLine>\r\n"
            + "<city>SALT LAKE CITY</city>\r\n"
            + "<state>UT</state>\r\n"
            + "<postalCode>84101</postalCode>\r\n"
            + "<country>USA</country>\r\n"
            + "</addr>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine>2221 HOME STREET</streetAddressLine>\r\n"
            + "<city>SALT LAKE CITY</city>\r\n"
            + "<state>UT</state>\r\n"
            + "<postalCode>84101</postalCode>\r\n"
            + "<country>USA</country>\r\n"
            + "</addr>\r\n"
            + "<telecom value=\"tel:(555)555-5006\" use=\"HP\"/>\r\n"
            + "<telecom value=\"tel:(555)555-5006\" use=\"MC\"/>\r\n"
            + "<telecom value=\"mailto:jill@email.com\"/>\r\n"
            + "<patient>\r\n"
            + "<name use=\"L\">\r\n"
            + "<given qualifier=\"PR\">Jill</given>\r\n"
            + "<family>Test</family>\r\n"
            + "</name>\r\n"
            + "<administrativeGenderCode code=\"F\" codeSystem=\"2.16.840.1.113883.5.1\" codeSystemName=\"HL7AdministrativeGenderCode\" displayName=\"female\"/>\r\n"
            + "<birthTime value=\"20201027\"/>\r\n"
            + "<sdtc:deceasedInd value=\"false\"/>\r\n"
            + "<maritalStatusCode code=\"S\" codeSystem=\"2.16.840.1.113883.5.2\" codeSystemName=\"v3-MaritalStatus\" displayName=\"Never Married\"/>\r\n"
            + "<raceCode code=\"2028-9\" codeSystem=\"2.16.840.1.113883.6.238\" codeSystemName=\"Race &amp; Ethnicity - CDC\" displayName=\"Asian\"/>\r\n"
            + "<ethnicGroupCode code=\"2186-5\" codeSystem=\"2.16.840.1.113883.6.238\" codeSystemName=\"Race &amp; Ethnicity - CDC\" displayName=\"Not Hispanic or Latino\"/>\r\n"
            + "<guardian>\r\n"
            + "<code nullFlavor=\"OTH\"><translation code=\"N\" codeSystem=\"2.16.840.1.113883.18.58\" codeSystemName=\"v2-0131\"/>\r\n"
            + "</code>\r\n"
            + "<addr use=\"HP\">\r\n"
            + "<streetAddressLine>534 Erewhon St</streetAddressLine>\r\n"
            + "<city>PleasantVille</city>\r\n"
            + "<county>Rainbow</county>\r\n"
            + "<state>Vic</state>\r\n"
            + "<postalCode>3999</postalCode>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<telecom value=\"tel:(323)799-8327\"/>\r\n"
            + "<guardianPerson>\r\n"
            + "<name>\r\n"
            + "<given>B�n�dicte</given>\r\n"
            + "<family>du March�</family>\r\n"
            + "</name>\r\n"
            + "</guardianPerson>\r\n"
            + "</guardian>\r\n"
            + "<languageCommunication>\r\n"
            + "<languageCode code=\"en\"/>\r\n"
            + "</languageCommunication>\r\n"
            + "</patient>\r\n"
            + "</patientRole>\r\n"
            + "</recordTarget>\r\n"
            + "";

    String actualXml =
        CdaHeaderGenerator.getPatientDetails(r4FhirData1.getPatient(), launchDetails);

    assertThat(actualXml).isNotNull();

    assertXmlEquals(expectedXml, actualXml);
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

  public String getCdaHeaderData() {
    String cdaHeaderXml =
        "<?xml version=\"1.0\"?>\r\n"
            + "<ClinicalDocument xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\r\n"
            + " xmlns=\"urn:hl7-org:v3\"\r\n"
            + " xmlns:cda=\"urn:hl7-org:v3\"\r\n"
            + " xmlns:sdtc=\"urn:hl7-org:sdtc\">\r\n"
            + "<realmCode code=\"US\"/>\r\n"
            + "<typeId root=\"2.16.840.1.113883.1.3\" extension=\"POCD_HD000040\"/>\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.1.1\"/>\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.1.1\" extension=\"2015-08-01\"/>\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.15.2\" extension=\"2016-12-01\"/>\r\n"
            + "<id root=\"d5d04894-e345-4571-afc5-56db664e2678\"/>\r\n"
            + "<code code=\"55751-2\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Initial Public Health Case Report\"/>\r\n"
            + "<title>Initial Public Health Case Report</title>\r\n"
            + "<effectiveTime value=\"20230508134408+0530\"/>\r\n"
            + "<confidentialityCode code=\"N\" codeSystem=\"2.16.840.1.113883.5.25\"/>\r\n"
            + "<languageCode code=\"en-US\"/>\r\n"
            + "<setId root=\"2.16.840.1.113883.1.1.1.1\" extension=\"1\"/>\r\n"
            + "<versionNumber value=\"43\"/>\r\n"
            + "<recordTarget>\r\n"
            + "<patientRole>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"a-11287.E-4237\"/>\r\n"
            + "<addr use=\"HP\">\r\n"
            + "<streetAddressLine>2221 HOME STREET</streetAddressLine>\r\n"
            + "<city>SALT LAKE CITY</city>\r\n"
            + "<state>UT</state>\r\n"
            + "<postalCode>84101</postalCode>\r\n"
            + "<country>USA</country>\r\n"
            + "</addr>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine>2221 HOME STREET</streetAddressLine>\r\n"
            + "<city>SALT LAKE CITY</city>\r\n"
            + "<state>UT</state>\r\n"
            + "<postalCode>84101</postalCode>\r\n"
            + "<country>USA</country>\r\n"
            + "</addr>\r\n"
            + "<telecom value=\"tel:(555)555-5006\" use=\"HP\"/>\r\n"
            + "<telecom value=\"tel:(555)555-5006\" use=\"MC\"/>\r\n"
            + "<telecom value=\"mailto:jill@email.com\"/>\r\n"
            + "<patient>\r\n"
            + "<name use=\"L\">\r\n"
            + "<given qualifier=\"PR\">Jill</given>\r\n"
            + "<family>Test</family>\r\n"
            + "</name>\r\n"
            + "<administrativeGenderCode code=\"F\" codeSystem=\"2.16.840.1.113883.5.1\"/>\r\n"
            + "<birthTime value=\"20201027\"/>\r\n"
            + "<sdtc:deceasedInd value=\"false\"/>\r\n"
            + "<maritalStatusCode code=\"S\" codeSystem=\"2.16.840.1.113883.5.2\" codeSystemName=\"v3-MaritalStatus\" displayName=\"Never Married\"/>\r\n"
            + "<raceCode code=\"2028-9\" codeSystem=\"2.16.840.1.113883.6.238\" codeSystemName=\"Race &amp; Ethnicity - CDC\" displayName=\"Asian\"/>\r\n"
            + "<ethnicGroupCode code=\"2186-5\" codeSystem=\"2.16.840.1.113883.6.238\" codeSystemName=\"Race &amp; Ethnicity - CDC\" displayName=\"Not Hispanic or Latino\"/>\r\n"
            + "<languageCommunication>\r\n"
            + "<languageCode code=\"en\"/>\r\n"
            + "</languageCommunication>\r\n"
            + "</patient>\r\n"
            + "</patientRole>\r\n"
            + "</recordTarget>\r\n"
            + "<author>\r\n"
            + "<time value=\"20220920101200+0000\"/>\r\n"
            + "<assignedAuthor>\r\n"
            + "<id root=\"2.16.840.1.113883.4.6\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<assignedPerson>\r\n"
            + "<name>\r\n"
            + "<given nullFlavor=\"NI\"/>\r\n"
            + "<family nullFlavor=\"NI\"/>\r\n"
            + "</name>\r\n"
            + "</assignedPerson>\r\n"
            + "</assignedAuthor>\r\n"
            + "</author>\r\n"
            + "<author>\r\n"
            + "<time value=\"20230508134408+0530\"/>\r\n"
            + "<assignedAuthor>\r\n"
            + "<id root=\"12219cbd-8006-43a1-b933-dfdcb989c0e4\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<assignedAuthoringDevice>\r\n"
            + "<manufacturerModelName displayName=\"ecrNowApp\"/>\r\n"
            + "<softwareName displayName=\"Version 3.1.X\"/>\r\n"
            + "</assignedAuthoringDevice>\r\n"
            + "</assignedAuthor>\r\n"
            + "</author>\r\n"
            + "<author>\r\n"
            + "<time value=\"20230508134408+0530\"/>\r\n"
            + "<assignedAuthor>\r\n"
            + "<id root=\"99055c5f-819d-402d-b12e-db78ddf23080\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<assignedAuthoringDevice>\r\n"
            + "<manufacturerModelName displayName=\"Example-Ehr\"/>\r\n"
            + "<softwareName displayName=\"1.0.0\"/>\r\n"
            + "</assignedAuthoringDevice>\r\n"
            + "</assignedAuthor>\r\n"
            + "</author>\r\n"
            + "<author>\r\n"
            + "<time value=\"20230508134408+0530\"/>\r\n"
            + "<assignedAuthor>\r\n"
            + "<id root=\"ba1de91b-a307-4e39-84fd-4009f15501a8\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<assignedAuthoringDevice>\r\n"
            + "<manufacturerModelName displayName=\"System-Integrator\"/>\r\n"
            + "<softwareName displayName=\"1.0.0\"/>\r\n"
            + "</assignedAuthoringDevice>\r\n"
            + "</assignedAuthor>\r\n"
            + "</author>\r\n"
            + "<custodian>\r\n"
            + "<assignedCustodian>\r\n"
            + "<representedCustodianOrganization>\r\n"
            + "<id nullFlavor=\"NI\"/>\r\n"
            + "<name>Unknown</name>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</representedCustodianOrganization>\r\n"
            + "</assignedCustodian>\r\n"
            + "</custodian>\r\n"
            + "<componentOf>\r\n"
            + "<encompassingEncounter>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"a-11287.stay-9787\"/>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"a-11287.stay-9787\"/>\r\n"
            + "<code code=\"IMP\" codeSystem=\"2.16.840.1.113883.5.4\" codeSystemName=\"v3-ActCode\" displayName=\"inpatient encounter\"></code>\r\n"
            + "<effectiveTime>\r\n"
            + "<low value=\"20220920101200+0000\"/>\r\n"
            + "<high nullFlavor=\"NI\"/>\r\n"
            + "</effectiveTime>\r\n"
            + "<responsibleParty>\r\n"
            + "<assignedEntity>\r\n"
            + "<id root=\"2.16.840.1.113883.4.6\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<assignedPerson>\r\n"
            + "<name>\r\n"
            + "<given nullFlavor=\"NI\"/>\r\n"
            + "<family nullFlavor=\"NI\"/>\r\n"
            + "</name>\r\n"
            + "</assignedPerson>\r\n"
            + "<representedOrganization>\r\n"
            + "<id nullFlavor=\"NI\"/>\r\n"
            + "<name>Unknown</name>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</representedOrganization>\r\n"
            + "</assignedEntity>\r\n"
            + "</responsibleParty>\r\n"
            + "<location>\r\n"
            + "<healthCareFacility>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"Location/a-11287.Department-1\"/>\r\n"
            + "<code code=\"CHR\" codeSystem=\"2.16.840.1.113883.5.111\" codeSystemName=\"v3-RoleCode\" displayName=\"Chronic Care Facility\"></code>\r\n"
            + "<location>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine>0987 Facility Drive</streetAddressLine>\r\n"
            + "<city>SALT LAKE CITY</city>\r\n"
            + "<state>UT</state>\r\n"
            + "<postalCode>84101-0001</postalCode>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</location>\r\n"
            + "<serviceProviderOrganization>\r\n"
            + "<id nullFlavor=\"NI\"/>\r\n"
            + "<name>Unknown</name>\r\n"
            + "<telecom nullFlavor=\"NI\"/>\r\n"
            + "<addr>\r\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\r\n"
            + "<city nullFlavor=\"NI\"/>\r\n"
            + "<state nullFlavor=\"NI\"/>\r\n"
            + "<postalCode nullFlavor=\"NI\"/>\r\n"
            + "<country nullFlavor=\"NI\"/>\r\n"
            + "</addr>\r\n"
            + "</serviceProviderOrganization>\r\n"
            + "</healthCareFacility>\r\n"
            + "</location>\r\n"
            + "</encompassingEncounter>";
    return cdaHeaderXml;
  }
}
