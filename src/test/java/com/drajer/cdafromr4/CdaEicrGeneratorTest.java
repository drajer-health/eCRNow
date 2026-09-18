package com.drajer.cdafromr4;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import ca.uhn.fhir.parser.IParser;
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
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({
  CdaHeaderGenerator.class,
  CdaGeneratorUtils.class,
  ActionRepo.class,
  CdaResultGenerator.class
})
@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
public class CdaEicrGeneratorTest extends BaseGeneratorTest {

  // Constants
  private static final String R4_DUPLICATE_SOC_HISTORY_ENTRIES_FILE =
      "SampleTestData/LoadingQueryBundle_DuplicateSocialHistory.json";

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

    data.setData(bundle);

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
    PowerMockito.when(CdaResultGenerator.generateResultsSection(data, launchDetails, "CDA_R11"))
        .thenReturn(labSection);

    String actualXml =
        CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            data, launchDetails, eicr, 0, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertNotNull("Generated XML should not be null", actualXml);
    assertThat(actualXml).isNotEmpty();
    assertThat(actualXml).contains("ClinicalDocument");
    assertThat(actualXml).contains("recordTarget");
  }

  @Test
  public void testConvertR4FhirBundleToCdaEicrDynamic() {
    // Initialize test data
    R4FhirData r4Data = createR4FhirData(R4_DUPLICATE_SOC_HISTORY_ENTRIES_FILE);
    launchDetails.setStatus(
        TestUtils.toJsonString(
            createPatientExecutionState("Condition", "http://loinc.org|68518-0")));

    // Mock static methods
    PowerMockito.mockStatic(ActionRepo.class);
    ActionRepo actionRepoMock = PowerMockito.mock(ActionRepo.class);
    PowerMockito.when(ActionRepo.getInstance()).thenReturn(actionRepoMock);
    EicrServiceImpl eicrRRServiceMock = PowerMockito.mock(EicrServiceImpl.class);
    PowerMockito.when(actionRepoMock.getEicrRRService()).thenReturn(eicrRRServiceMock);
    PowerMockito.when(eicrRRServiceMock.getMaxVersionId(Mockito.any(Eicr.class))).thenReturn(0);

    PowerMockito.mockStatic(CdaHeaderGenerator.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);

    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    PowerMockito.when(CdaGeneratorUtils.getGuid())
        .thenReturn("b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9");

    String actualXml =
        CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            r4Data, launchDetails, eicr, 0, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    ApplicationUtils.saveDataToFile(actualXml, "./Eicr.xml");
    assertNotNull("Generated EICR XML should not be null", actualXml);
    assertThat(actualXml).isNotEmpty();
    assertThat(actualXml).contains("ClinicalDocument");
    assertThat(actualXml).contains("<realmCode code=\"US\"/>");
    assertThat(actualXml).contains("recordTarget");
    assertThat(actualXml).contains("author");
  }

  @Test
  public void testConvertR4FhirBundleToCdaEicrWithNullData() {

    String result =
        CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            null, null, null, 0, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertThat(result).isEmpty();
  }

  @Test
  public void testConvertR4FhirBundletoCdaEicrWithEmptyData() {

    assertThrows(
        RuntimeException.class,
        () ->
            CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
                new R4FhirData(), null, null, 0, CdaGeneratorConstants.CDA_EICR_VERSION_R11));
  }

  private R4FhirData createR4FhirData(String file) {
    Bundle data = loadBundleFromFile(file);
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

    IParser jsonParserMock = Mockito.mock(IParser.class);
    ApplicationUtils ap = new ApplicationUtils(jsonParserMock);

    Bundle bund = ap.readBundleFromFile(absolutePath);
    data.setData(bund);

    return data;
  }

  @Override
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
        """
        <?xml version="1.0"?>\r
        <ClinicalDocument xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\r
         xmlns="urn:hl7-org:v3"\r
         xmlns:cda="urn:hl7-org:v3"\r
         xmlns:sdtc="urn:hl7-org:sdtc">\r
        <realmCode code="US"/>\r
        <typeId root="2.16.840.1.113883.1.3" extension="POCD_HD000040"/>\r
        <templateId root="2.16.840.1.113883.10.20.22.1.1"/>\r
        <templateId root="2.16.840.1.113883.10.20.22.1.1" extension="2015-08-01"/>\r
        <templateId root="2.16.840.1.113883.10.20.15.2" extension="2016-12-01"/>\r
        <id root="d5d04894-e345-4571-afc5-56db664e2678"/>\r
        <code code="55751-2" codeSystem="2.16.840.1.113883.6.1" codeSystemName="LOINC" displayName="Initial Public Health Case Report"/>\r
        <title>Initial Public Health Case Report</title>\r
        <effectiveTime value="20230508134408+0530"/>\r
        <confidentialityCode code="N" codeSystem="2.16.840.1.113883.5.25"/>\r
        <languageCode code="en-US"/>\r
        <setId root="2.16.840.1.113883.1.1.1.1" extension="1"/>\r
        <versionNumber value="43"/>\r
        <recordTarget>\r
        <patientRole>\r
        <id root="2.16.840.1.113883.1.1.1.1" extension="a-11287.E-4237"/>\r
        <addr use="HP">\r
        <streetAddressLine>2221 HOME STREET</streetAddressLine>\r
        <city>SALT LAKE CITY</city>\r
        <state>UT</state>\r
        <postalCode>84101</postalCode>\r
        <country>USA</country>\r
        </addr>\r
        <addr>\r
        <streetAddressLine>2221 HOME STREET</streetAddressLine>\r
        <city>SALT LAKE CITY</city>\r
        <state>UT</state>\r
        <postalCode>84101</postalCode>\r
        <country>USA</country>\r
        </addr>\r
        <telecom value="tel:(555)555-5006" use="HP"/>\r
        <telecom value="tel:(555)555-5006" use="MC"/>\r
        <telecom value="mailto:jill@email.com"/>\r
        <patient>\r
        <name use="L">\r
        <given qualifier="PR">Jill</given>\r
        <family>Test</family>\r
        </name>\r
        <administrativeGenderCode code="F" codeSystem="2.16.840.1.113883.5.1"/>\r
        <birthTime value="20201027"/>\r
        <sdtc:deceasedInd value="false"/>\r
        <maritalStatusCode code="S" codeSystem="2.16.840.1.113883.5.2" codeSystemName="v3-MaritalStatus" displayName="Never Married"/>\r
        <raceCode code="2028-9" codeSystem="2.16.840.1.113883.6.238" codeSystemName="Race &amp; Ethnicity - CDC" displayName="Asian"/>\r
        <ethnicGroupCode code="2186-5" codeSystem="2.16.840.1.113883.6.238" codeSystemName="Race &amp; Ethnicity - CDC" displayName="Not Hispanic or Latino"/>\r
        <languageCommunication>\r
        <languageCode code="en"/>\r
        </languageCommunication>\r
        </patient>\r
        </patientRole>\r
        </recordTarget>\r
        <author>\r
        <time value="20220920101200+0000"/>\r
        <assignedAuthor>\r
        <id root="2.16.840.1.113883.4.6"/>\r
        <addr>\r
        <streetAddressLine nullFlavor="NI"/>\r
        <city nullFlavor="NI"/>\r
        <state nullFlavor="NI"/>\r
        <postalCode nullFlavor="NI"/>\r
        <country nullFlavor="NI"/>\r
        </addr>\r
        <telecom nullFlavor="NI"/>\r
        <assignedPerson>\r
        <name>\r
        <given nullFlavor="NI"/>\r
        <family nullFlavor="NI"/>\r
        </name>\r
        </assignedPerson>\r
        </assignedAuthor>\r
        </author>\r
        <author>\r
        <time value="20230508134408+0530"/>\r
        <assignedAuthor>\r
        <id root="12219cbd-8006-43a1-b933-dfdcb989c0e4"/>\r
        <addr>\r
        <streetAddressLine nullFlavor="NI"/>\r
        <city nullFlavor="NI"/>\r
        <state nullFlavor="NI"/>\r
        <postalCode nullFlavor="NI"/>\r
        <country nullFlavor="NI"/>\r
        </addr>\r
        <assignedAuthoringDevice>\r
        <manufacturerModelName displayName="ecrNowApp"/>\r
        <softwareName displayName="Version 3.1.X"/>\r
        </assignedAuthoringDevice>\r
        </assignedAuthor>\r
        </author>\r
        <author>\r
        <time value="20230508134408+0530"/>\r
        <assignedAuthor>\r
        <id root="99055c5f-819d-402d-b12e-db78ddf23080"/>\r
        <addr>\r
        <streetAddressLine nullFlavor="NI"/>\r
        <city nullFlavor="NI"/>\r
        <state nullFlavor="NI"/>\r
        <postalCode nullFlavor="NI"/>\r
        <country nullFlavor="NI"/>\r
        </addr>\r
        <assignedAuthoringDevice>\r
        <manufacturerModelName displayName="Example-Ehr"/>\r
        <softwareName displayName="1.0.0"/>\r
        </assignedAuthoringDevice>\r
        </assignedAuthor>\r
        </author>\r
        <author>\r
        <time value="20230508134408+0530"/>\r
        <assignedAuthor>\r
        <id root="ba1de91b-a307-4e39-84fd-4009f15501a8"/>\r
        <addr>\r
        <streetAddressLine nullFlavor="NI"/>\r
        <city nullFlavor="NI"/>\r
        <state nullFlavor="NI"/>\r
        <postalCode nullFlavor="NI"/>\r
        <country nullFlavor="NI"/>\r
        </addr>\r
        <assignedAuthoringDevice>\r
        <manufacturerModelName displayName="System-Integrator"/>\r
        <softwareName displayName="1.0.0"/>\r
        </assignedAuthoringDevice>\r
        </assignedAuthor>\r
        </author>\r
        <custodian>\r
        <assignedCustodian>\r
        <representedCustodianOrganization>\r
        <id nullFlavor="NI"/>\r
        <name>Unknown</name>\r
        <telecom nullFlavor="NI"/>\r
        <addr>\r
        <streetAddressLine nullFlavor="NI"/>\r
        <city nullFlavor="NI"/>\r
        <state nullFlavor="NI"/>\r
        <postalCode nullFlavor="NI"/>\r
        <country nullFlavor="NI"/>\r
        </addr>\r
        </representedCustodianOrganization>\r
        </assignedCustodian>\r
        </custodian>\r
        <componentOf>\r
        <encompassingEncounter>\r
        <id root="2.16.840.1.113883.1.1.1.1" extension="a-11287.stay-9787"/>\r
        <id root="2.16.840.1.113883.1.1.1.1" extension="a-11287.stay-9787"/>\r
        <code code="IMP" codeSystem="2.16.840.1.113883.5.4" codeSystemName="v3-ActCode" displayName="inpatient encounter"></code>\r
        <effectiveTime>\r
        <low value="20220920101200+0000"/>\r
        <high nullFlavor="NI"/>\r
        </effectiveTime>\r
        <responsibleParty>\r
        <assignedEntity>\r
        <id root="2.16.840.1.113883.4.6"/>\r
        <addr>\r
        <streetAddressLine nullFlavor="NI"/>\r
        <city nullFlavor="NI"/>\r
        <state nullFlavor="NI"/>\r
        <postalCode nullFlavor="NI"/>\r
        <country nullFlavor="NI"/>\r
        </addr>\r
        <telecom nullFlavor="NI"/>\r
        <assignedPerson>\r
        <name>\r
        <given nullFlavor="NI"/>\r
        <family nullFlavor="NI"/>\r
        </name>\r
        </assignedPerson>\r
        <representedOrganization>\r
        <id nullFlavor="NI"/>\r
        <name>Unknown</name>\r
        <telecom nullFlavor="NI"/>\r
        <addr>\r
        <streetAddressLine nullFlavor="NI"/>\r
        <city nullFlavor="NI"/>\r
        <state nullFlavor="NI"/>\r
        <postalCode nullFlavor="NI"/>\r
        <country nullFlavor="NI"/>\r
        </addr>\r
        </representedOrganization>\r
        </assignedEntity>\r
        </responsibleParty>\r
        <location>\r
        <healthCareFacility>\r
        <id root="2.16.840.1.113883.1.1.1.1" extension="a-11287.Department-1"/>\r
        <code code="CHR" codeSystem="2.16.840.1.113883.5.111" codeSystemName="v3-RoleCode" displayName="Chronic Care Facility"></code>\r
        <location>\r
        <addr>\r
        <streetAddressLine>0987 Facility Drive</streetAddressLine>\r
        <city>SALT LAKE CITY</city>\r
        <state>UT</state>\r
        <postalCode>84101-0001</postalCode>\r
        <country nullFlavor="NI"/>\r
        </addr>\r
        </location>\r
        <serviceProviderOrganization>\r
        <id nullFlavor="NI"/>\r
        <name>Unknown</name>\r
        <telecom nullFlavor="NI"/>\r
        <addr>\r
        <streetAddressLine nullFlavor="NI"/>\r
        <city nullFlavor="NI"/>\r
        <state nullFlavor="NI"/>\r
        <postalCode nullFlavor="NI"/>\r
        <country nullFlavor="NI"/>\r
        </addr>\r
        </serviceProviderOrganization>\r
        </healthCareFacility>\r
        </location>\r
        </encompassingEncounter>\r
        </componentOf>
        """;
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
