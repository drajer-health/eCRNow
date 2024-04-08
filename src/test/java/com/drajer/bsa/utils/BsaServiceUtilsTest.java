package com.drajer.bsa.utils;

import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatusList;
import com.drajer.bsa.kar.action.SubmitReport;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.MessageType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.service.KarParser;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.test.util.Utility;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.io.FileUtils;
import org.hl7.fhir.instance.model.api.IBase;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DocumentReference;
import org.hl7.fhir.r4.model.MessageHeader;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Parameters.ParametersParameterComponent;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ValueSet;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class BsaServiceUtilsTest {

  @Autowired KarParser parser;

  BsaServiceUtils bsaServiceUtils;

  @Mock KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;

  private static Set<Resource> resources = new HashSet<>();

  private Bundle bundle;

  private FhirContext fhirContext;

  @Before
  public void initializeTestData() throws Exception {
    bsaServiceUtils = new BsaServiceUtils();

    ReflectionTestUtils.setField(bsaServiceUtils, "SAVE_DEBUG_TO_FILES", true);
    ReflectionTestUtils.setField(bsaServiceUtils, "debugDirectory", "src/test/resources/bsa/");
    ReflectionTestUtils.setField(bsaServiceUtils, "DEBUG_DIRECTORY", "src/test/resources/bsa/");

    IParser iParser = mock(IParser.class);
    iParser.setOmitResourceId(true);
    iParser.setStripVersionsFromReferences(true);

    ReflectionTestUtils.setField(bsaServiceUtils, "jsonParser", iParser);
    ReflectionTestUtils.setField(bsaServiceUtils, "FHIR_JSON_PARSER", iParser);

    String notificationBundle =
        FileUtils.readFileToString(
            new File("src/test/resources/Bsa/NotificationBundleEncounterClose.json"),
            Charset.defaultCharset());
    Mockito.lenient()
        .when(iParser.encodeResourceToString(Mockito.any()))
        .thenReturn(notificationBundle);

    fhirContext = FhirContext.forR4();
    bundle =
        fhirContext
            .newJsonParser()
            .parseResource(
                Bundle.class,
                BsaServiceUtilsTest.class.getResourceAsStream(
                    "/R4/Condition/ConditionResource.json"));

    bundle
        .getEntry()
        .forEach(
            (e) -> {
              resources.add(e.getResource());
            });
    InputStream inputStreamMock = Mockito.mock(InputStream.class);
    Mockito.lenient()
        .when(iParser.parseResource(Mockito.eq(Bundle.class), Mockito.any(InputStream.class)))
        .thenReturn(bundle);
  }

  @Test
  public void getFhirPathVariableString() {
    String expectedPathVariable = "%plandefinition-cancer-example";
    String actualPathVariable =
        bsaServiceUtils.getFhirPathVariableString("plandefinition-cancer-example");
    assertEquals(expectedPathVariable, actualPathVariable);
  }

  @Test
  public void readKarFromFile() {
    fhirContext = FhirContext.forR4();
    Bundle expectedBundle =
        fhirContext
            .newJsonParser()
            .parseResource(
                Bundle.class,
                BsaServiceUtilsTest.class.getResourceAsStream(
                    "/R4/Condition/ConditionWithAndWithoutTriggerCodes.json"));
    Bundle actualBundle =
        bsaServiceUtils.readKarFromFile(
            "src/test/resources//R4/Condition/ConditionWithAndWithoutTriggerCodes.json");
    assertEquals(expectedBundle.getId(), actualBundle.getId());
  }

  @Test
  public void readKarFromFileByException() {
    assertThatExceptionOfType(RuntimeException.class)
        .isThrownBy(
            () -> {
              doThrow(RuntimeException.class).when(bsaServiceUtils.readKarFromFile(""));
            });
  }

  @Test
  public void filterResources() {
    KarProcessingData karProcessingData = Utility.karProcessingData();
    DataRequirement dataRequirement = new DataRequirement();
    DataRequirement.DataRequirementCodeFilterComponent codeFilter = dataRequirement.addCodeFilter();
    codeFilter.setPath("code");
    List<Coding> codingList = new ArrayList<>();
    codingList.add(
        new Coding().setCode("RSK").setSystem("http://terminology.hl7.org/CodeSystem/v3-ActMood"));
    codeFilter.setCode(codingList);
    codeFilter.setValueSet(ResourceType.ValueSet.toString());
    DataRequirement.DataRequirementDateFilterComponent dateFilter = dataRequirement.addDateFilter();
    dateFilter.setValue(new Period());

    DataRequirement profileFilter = dataRequirement.addProfile("837567");
    CanonicalType canonicalType = new CanonicalType();
    canonicalType.setId("67438");
    canonicalType.setValue("837567");
    canonicalType.setValueAsString(
        "http://hl7.org/fhir/us/core/StructureDefinition/us-core-condition");
    List<CanonicalType> canonicalTypeList = new ArrayList<>();
    canonicalTypeList.add(canonicalType);
    profileFilter.setProfile(canonicalTypeList);
    profileFilter.setDisallowExtensions(true);
    profileFilter.setId("873647");

    Set<Resource> filterResource =
        BsaServiceUtils.filterResources(resources, dataRequirement, karProcessingData);

    assertEquals(104, resources.size());
    assertEquals(13, filterResource.size());
  }

  @Test
  public void getTriggerMatchStatus() throws IOException {
    assertThrows(
        Exception.class,
        () -> {
          bsaServiceUtils.getTriggerMatchStatus("");
        });
  }

  @Test
  public void getEncodedTriggerMatchStatus() throws Exception {
    KarProcessingData kd = Utility.karProcessingData();
    CheckTriggerCodeStatusList expectedcheckTriggerList = Utility.getCheckTriggerCodeStatusList();
    String actualcheckTriggerList =
        bsaServiceUtils.getEncodedTriggerMatchStatus(
            expectedcheckTriggerList,
            kd,
            "763845684756",
            "create-report-actionId",
            "create-report-actionType");
    assertNotNull(actualcheckTriggerList);
  }

  @Test
  public void convertDataToParameters() {

    Parameters params = new Parameters();
    ParametersParameterComponent parametersParameterComponent = new ParametersParameterComponent();
    parametersParameterComponent.setName("%Jon%4656");
    List<ParametersParameterComponent> parameterList = params.getParameter();
    bsaServiceUtils.convertDataToParameters("4656", "R4", "10", resources, params);
  }

  @Test
  public void convertDataToParametersForException() {

    Set<Resource> resource = new HashSet<>();
    Parameters params = new Parameters();
    ParametersParameterComponent parametersParameterComponent = new ParametersParameterComponent();
    parametersParameterComponent.setName("%Jon%4656");
    List<ParametersParameterComponent> parameterList = params.getParameter();
    bsaServiceUtils.convertDataToParameters("4656", "R4", "10", resource, params);
  }

  @Test
  public void hasCdaData() throws Exception {

    MessageHeader messageHeader = new MessageHeader();
    Coding coding = new Coding();
    coding.setCode(BsaTypes.getMessageTypeString(MessageType.CDA_EICR_MESSAGE));
    messageHeader.setEvent(coding);
    BundleEntryComponent bundleEntry = new BundleEntryComponent().setResource(messageHeader);
    BundleEntryComponent bundleEntryComponent =
        new BundleEntryComponent().setResource(messageHeader);

    Bundle bundle = new Bundle().setType(BundleType.MESSAGE).addEntry(bundleEntry);
    bundle.addEntry(bundleEntryComponent);
    Boolean cdaData = BsaServiceUtils.hasCdaData(bundle);
    assertTrue(cdaData);
  }

  @Test
  public void getDefaultQueriesForAction() {
    BsaAction bsaAction = new SubmitReport();

    FhirQueryFilter fhirQueryFilter = new FhirQueryFilter();
    fhirQueryFilter.setCustomized(true);
    fhirQueryFilter.setDataReqId("ecrunittest_id");
    fhirQueryFilter.setResourceType(ResourceType.Patient);
    HashMap<String, FhirQueryFilter> requirementQueriesMap = new HashMap<>();
    requirementQueriesMap.put("relatedDataId", fhirQueryFilter);
    bsaAction.setInputDataRequirementQueries(requirementQueriesMap);

    HashMap<String, String> relatedDataIdMap = new HashMap<>();
    relatedDataIdMap.put("inputDataId", "relatedDataId");
    bsaAction.setInputDataIdToRelatedDataIdMap(relatedDataIdMap);

    KnowledgeArtifact knowledgeArtifact = new KnowledgeArtifact();
    knowledgeArtifact.setDefaultQueries(requirementQueriesMap);

    Map<String, FhirQueryFilter> defaultQueriesForAction =
        bsaServiceUtils.getDefaultQueriesForAction(bsaAction, knowledgeArtifact);
    assertNotNull(defaultQueriesForAction);
  }

  @Test
  public void saveFhirResourceToFile() throws Exception {
    FhirContext fhirContext = FhirContext.forR4();
    Patient patient = new Patient();
    patient.setId("1");
    patient.addName().setFamily("Doe").addGiven("John");
    bsaServiceUtils.saveFhirResourceToFile(patient, "NotificationBundleEncounterClose");
  }

  @Test
  public void saveResourceToFile() {
    Patient patient = new Patient();
    patient.setId("1");
    patient.addName().setFamily("Doe").addGiven("John");
    bsaServiceUtils.saveResourceToFile(patient);
  }

  @Test
  public void saveCdaDocumentFromDocumentBundleToFile() {
    FhirContext fhirContext = FhirContext.forR4();
    Bundle messageHeader =
        fhirContext
            .newJsonParser()
            .parseResource(
                Bundle.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream(
                    "/Bsa/DocumentReferenceResource.json"));
    List<Pair<String, String>> cdaDocument =
        bsaServiceUtils.saveCdaDocumentFromDocumentBundleToFile(
            "/bsa/", "DocumentReference.xml", messageHeader);
    assertNotNull(cdaDocument);
  }

  @Test
  public void findMessageHeaderAndDocumentReferences() {
    FhirContext fhirContext = FhirContext.forR4();
    Bundle bundle =
        fhirContext
            .newJsonParser()
            .parseResource(
                Bundle.class,
                R3ToR2DataConverterUtilsTest.class.getResourceAsStream("/Bsa/MessageHeader.json"));
    List<DocumentReference> documentReferenceList = new ArrayList<>();
    MessageHeader messageHeader =
        bsaServiceUtils.findMessageHeaderAndDocumentReferences(bundle, documentReferenceList);
    assertNotNull(messageHeader);
  }

  @Test
  public void getMatchableCodes() {
    CodeableConcept codeableConcept = new CodeableConcept();
    codeableConcept.setDisallowExtensions(true);
    codeableConcept.setId("76324");

    Coding coding = new Coding();
    coding.setCode("444971000124105");
    coding.setDisplay("Annual wellness visit (procedure)");
    coding.setSystem("http://snomed.info/sct");
    List<Coding> codingList = new ArrayList<>();
    codingList.add(coding);
    codeableConcept.setCoding(codingList);
    Set<String> actualMatcheableCodes = BsaServiceUtils.getMatchableCodes(codeableConcept);

    String matcheableCodes = "http://snomed.info/sct|444971000124105";
    Set<String> expectedMatcheableCodes = new HashSet<>();
    expectedMatcheableCodes.add(matcheableCodes);
    assertEquals(expectedMatcheableCodes, actualMatcheableCodes);
  }

  @Test
  public void matchesValueSet() {
    KarProcessingData karProcessingData = Utility.karProcessingData();
    Coding coding = new Coding();
    IBase iBase = (IBase) coding;
    coding.setCode("700217006");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("Suspected coronavirus infection (situation)");
    iBase.setUserData("ValueSet", coding);
    Boolean valueSet = bsaServiceUtils.matchesValueSet(iBase, "ValueSet", karProcessingData);
    assertTrue(valueSet);
  }

  @Test
  public void matchesCodes() {
    KarProcessingData karProcessingData = Utility.karProcessingData();
    Coding coding = new Coding();

    IBase iBase = (IBase) coding;
    coding.setCode("700217006");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("Suspected coronavirus infection (situation)");
    iBase.setUserData("ValueSet", coding);
    List<Coding> codingList = new ArrayList<>();
    codingList.add(new Coding().setCode("700217006").setSystem("http://snomed.info/sct"));

    Boolean matchedCodes = bsaServiceUtils.matchesCodes(iBase, codingList, karProcessingData);
    assertTrue(matchedCodes);
  }

  @Test
  public void isCodeableConceptPresentInValueSet() {

    ValueSet valueSet =
        fhirContext
            .newJsonParser()
            .parseResource(
                ValueSet.class, BsaServiceUtils.class.getResourceAsStream("/Bsa/ValueSet.json"));
    CodeableConcept codeableConcept = new CodeableConcept();
    Coding coding = new Coding();
    coding.setCode("700217006");
    coding.setSystem("http://snomed.info/sct");
    coding.setDisplay("Suspected coronavirus infection (situation)");
    List<Coding> codingList = new ArrayList<>();
    codingList.add(coding);
    codeableConcept.setCoding(codingList);
    Pair<Boolean, MatchedTriggerCodes> actualCodeableConcept =
        BsaServiceUtils.isCodeableConceptPresentInValueSet(valueSet, codeableConcept, "code", true);
    assertNotNull(actualCodeableConcept);
  }

  @Test
  public void testHasCdaData() {
    Patient patient = new Patient();
    Boolean cdaData = BsaServiceUtils.hasCdaData(patient);
    assertFalse(cdaData);
  }
}
