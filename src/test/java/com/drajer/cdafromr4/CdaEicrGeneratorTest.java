package com.drajer.cdafromr4;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.impl.EicrServiceImpl;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Type;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({
  CdaHeaderGenerator.class,
  CdaGeneratorUtils.class,
  ActionRepo.class,
  CdaResultGenerator.class
})
public class CdaEicrGeneratorTest extends BaseGeneratorTest {

  // Constants
  private static final String R4_BUNDLE_FILE =
      "SampleTestData/r4-loading-query-bundle-sample1.json";
  private static final String PATIENT_SAMPLE_CDA_FILE = "CdaTestData/Cda/sample/PatientSample.xml";
  private static final String EICR_CDA_FILE = "CdaTestData/Eicr/eicr.xml";

  private static final String LAB_SECTION_FILE = "CdaTestData/cda/Result/result-section.xml";

  @Test
  public void testConvertR4FhirBundleToEicr() {
    R4FhirData data = new R4FhirData();
    Bundle b =
        loadBundleFromFile(
            "CdaTestData/LoadingQuery/LoadingQueryBundle_e9bd7100-48af-4c69-a557-c13235f72f74.json");

    List<BundleEntryComponent> entries = b.getEntry();
    Bundle bundle = new Bundle();
    Set<Resource> resourceSet = new HashSet<>(); // Initialize HashSet outside the loop

    Map<String, List<String>> uniqueResourceIdsByType = new HashMap<>();
    for (BundleEntryComponent ent : entries) {

      resourceSet.add(ent.getResource());
      ResourceType resourceType = ent.getResource().getResourceType();
      R3ToR2DataConverterUtils.addResourcesToR4FhirData(
          "1",
          bundle,
          data,
          launchDetails,
          resourceSet,
          resourceType.toString(),
          uniqueResourceIdsByType);
      resourceSet.clear();
    }
    //    data.getLabResults().sort(Comparator.comparing(Observation::getId));
    //    data.getDiagReports().sort(Comparator.comparing(DiagnosticReport::getId));
    data.setData(bundle);

    String expectedXml = TestUtils.getFileContentAsString(EICR_CDA_FILE);
    String labSection = TestUtils.getFileContentAsString(LAB_SECTION_FILE);

    PowerMockito.mockStatic(ActionRepo.class);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);

    ActionRepo actionRepoMock = PowerMockito.mock(ActionRepo.class);

    PowerMockito.when(ActionRepo.getInstance()).thenReturn(actionRepoMock);

    EicrServiceImpl eicrRRServiceMock = PowerMockito.mock(EicrServiceImpl.class);

    PowerMockito.when(actionRepoMock.getEicrRRService()).thenReturn(eicrRRServiceMock);
    PowerMockito.when(eicrRRServiceMock.getMaxVersionId(Mockito.any(Eicr.class))).thenReturn(0);

    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    PowerMockito.when(CdaGeneratorUtils.getGuid())
        .thenReturn("b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9");

    PowerMockito.when(CdaGeneratorUtils.getCurrentDateTime()).thenReturn("20240819101316");

    PowerMockito.when(
            CdaGeneratorUtils.getXmlForEffectiveTime(
                CdaGeneratorConstants.EFF_TIME_EL_NAME, CdaGeneratorUtils.getCurrentDateTime()))
        .thenReturn("<effectiveTime value=\"20240819101316\"/>");

    PowerMockito.mockStatic(CdaResultGenerator.class);
    PowerMockito.when(CdaResultGenerator.generateResultsSection(data, launchDetails))
        .thenReturn(labSection);

    String actualXml =
        CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data, launchDetails, eicr);

    // saveDataToFile(actualXml,
    // "C://codebase/eCRNow/src/test/resources/CdaTestData/Eicr/eicr.xml");
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testConvertR4FhirBundleToCdaEicrWithNullData() {

    String result = CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(null, null, null);

    assertThat(result).isEmpty();
  }

  @Test
  public void testConvertR4FhirBundletoCdaEicrWithEmptyData() {

    assertThrows(
        RuntimeException.class,
        () -> CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(new R4FhirData(), null, null));
  }

  private R4FhirData createR4FhirData() {
    Bundle data = loadBundleFromFile(R4_BUNDLE_FILE);
    R4FhirData r4Data = new R4FhirData();
    r4Data.setData(data);
    r4Data = createR4Resource(r4Data, data);
    return r4Data;
  }

  public R4FhirData getFhirData() {

    String resourceName = "LoadingQueryR4Bundle.json";

    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(resourceName).getFile());
    String absolutePath = file.getAbsolutePath();

    R4FhirData data = new R4FhirData();

    ApplicationUtils ap = new ApplicationUtils();

    Bundle bund = ap.readBundleFromFile(absolutePath);
    data.setData(bund);

    return data;
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

  public LaunchDetails getLaunchDetails() {

    LaunchDetails ld = new LaunchDetails();

    ld.setId(1);
    ld.setClientId("DummyClient");
    ld.setClientSecret("DummySecret");
    ld.setEhrServerURL("DummyServerUrl");
    ld.setAuthUrl("DummyAuthUrl");
    ld.setTokenUrl("DummyTokenUrl");
    ld.setAccessToken("DummyAccessToken");
    ld.setUserId("DummyUser");
    ld.setExpiry(60);
    ld.setScope("DummyScope");
    ld.setLastUpdated(Date.from(Instant.now()));
    ld.setStartDate(Date.from(Instant.now()));
    ld.setEndDate(Date.from(Instant.now()));
    ld.setRefreshToken("DummyRefreshToken");
    ld.setLaunchPatientId("1234");
    ld.setFhirVersion("4.0.1");
    ld.setEncounterId("5678");
    ld.setStatus("active");
    ld.setAssigningAuthorityId("2.16.840.1.113883.1.1.1.1.1");
    ld.setSetId("1234" + "|" + "5678");
    ld.setVersionNumber(1);
    ld.setDirectHost("ett.healthit.gov");
    ld.setDirectUser("test@ett.healthit.gov");
    ld.setDirectPwd("password");
    ld.setSmtpPort("25");
    ld.setImapPort("443");
    ld.setDirectRecipient("connectathon@aimsplatform.org");
    ld.setRestAPIURL("DummyRestApiUrl");
    ld.setIsCovid(true);
    ld.setLaunchId("DummyLaunchId");
    ld.setLaunchState(1);
    ld.setRedirectURI("DummyRedirectUri");
    ld.setIsSystem(true);
    ld.setDebugFhirQueryAndEicr(true);

    return ld;
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
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"a-11287.Department-1\"/>\r\n"
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
            + "</encompassingEncounter>\r\n"
            + "</componentOf>";
    return cdaHeaderXml;
  }

  public static void saveDataToFile(String data, String filename) {

    if (true) {
      try (DataOutputStream outStream =
          new DataOutputStream(new BufferedOutputStream(new FileOutputStream(filename)))) {

        logger.info(" Writing data to file: {}", filename);
        outStream.writeBytes(data);
      } catch (IOException e) {
        logger.debug(" Unable to write data to file: {}", filename, e);
      }
    }
  }
}
