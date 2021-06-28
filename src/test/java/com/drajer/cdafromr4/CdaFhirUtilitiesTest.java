package com.drajer.cdafromr4;

import static org.junit.Assert.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.eca.model.ActionRepo;
import com.drajer.fhirecr.FhirGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Collectors;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Address.AddressType;
import org.hl7.fhir.r4.model.Address.AddressTypeEnumFactory;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DecimalType;
import org.hl7.fhir.r4.model.Enumeration;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Patient.ContactComponent;
import org.hl7.fhir.r4.model.Patient.PatientCommunicationComponent;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.UriType;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.javatuples.Pair;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.powermock.api.mockito.PowerMockito;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

class CdaFhirUtilitiesTest {
  Map<String, List<String>> testData = new HashMap<String, List<String>>();
  private static final Logger logger = LoggerFactory.getLogger(CdaFhirUtilitiesTest.class);
  private static LaunchDetails expectedLaunchDetails;
  private static final String INTERPRETATION_CODE = "interpretationCode";
  private static final String OBSERVATION = "Observation";
  List<Coding> codes = new ArrayList<Coding>();
  @Autowired ObjectMapper mapper;

  @BeforeEach
  void setUp() throws Exception {
	  createTestDataForStatusCodeTest();
	  createTestDataForCodingXML();
    expectedLaunchDetails =
        (LaunchDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/LaunchDetails/LaunchDetails.json", LaunchDetails.class);
    expectedLaunchDetails.setLastUpdated(new Date());
  }

  @AfterEach
  void tearDown() throws Exception {}

  @Test
  void testGetIdentifierForType() {
    List<Identifier> retIds =
        CdaFhirUtilities.getIdentifierForType(
            getPatientObj().getIdentifier(), CdaFhirEnumConstants.FHIR_ID_TYPE_MR);
    assertEquals(
        retIds.get(0).getType().getCoding().get(0).getCode(), CdaFhirEnumConstants.FHIR_ID_TYPE_MR);
  }

  @Test
  public void testGetStatusCodeForFhirMedStatusCodes() {
    Set<String> testSet = testData.keySet();
    for (String dataKey : testSet) {
      for (String input : testData.get(dataKey)) {
        String output = CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(input);
        assertEquals(dataKey, output);
      }
    }
  }

  @Test
  void testGetGuardianContact() {
    Patient.ContactComponent pcontactComp =
        CdaFhirUtilities.getGuardianContact(getContactCompList());
    assertEquals(
        pcontactComp.getRelationship().get(0).getText(), CdaGeneratorConstants.GUARDIAN_EL_NAME);
  }

  private CodeableConcept getCodeableConceptObj() {
    CodeableConcept codeableConcept = new CodeableConcept();
    codeableConcept.setText(CdaGeneratorConstants.GUARDIAN_EL_NAME);
    codeableConcept.setCoding(getCodingListObj());
    return codeableConcept;
  }

  private List<CodeableConcept> getCodeableConceptList() {
    List<CodeableConcept> codeableConceptList = new ArrayList<CodeableConcept>();
    codeableConceptList.add(getCodeableConceptObj());
    return codeableConceptList;
  }

  private List<ContactComponent> getContactCompList() {
    List<ContactComponent> contactCompList = new ArrayList<ContactComponent>();
    ContactComponent contactComp = new ContactComponent();
    contactComp.setRelationship(getCodeableConceptList());
    contactCompList.add(contactComp);
    return contactCompList;
  }

  @Test
  void testGetIdentifierForSystem() {
    List<Identifier> identList = new ArrayList<Identifier>();
    Identifier identifie = getIdentiferObj();
    identifie.setSystem(CdaGeneratorConstants.FHIR_NPI_URL);
    identList.add(identifie);

    Identifier ident =
        CdaFhirUtilities.getIdentifierForSystem(identList, CdaGeneratorConstants.FHIR_NPI_URL);
    assertEquals(ident.getSystem(), CdaGeneratorConstants.FHIR_NPI_URL);
  }

  @Test
  void testGetCodingExtension() {
    String extUrl = CdaGeneratorConstants.FHIR_USCORE_RACE_EXT_URL;
    String subextUrl = CdaGeneratorConstants.OMB_RACE_CATEGORY_URL;
    Coding coding =
        CdaFhirUtilities.getCodingExtension(getExtsListObj(new Coding()), extUrl, subextUrl);
    assertEquals("Covid-19", coding.getDisplay());
  }

  private List<Extension> getExtsListObj(Object objtype) {
    List<Extension> extsList = new ArrayList<Extension>();
    Extension extension = new Extension();
    extension.setUrl(CdaGeneratorConstants.FHIR_USCORE_RACE_EXT_URL);
    if (objtype instanceof Coding) {
      extension.setValue(FhirGeneratorUtils.getCoding("TestValue", "123456", "Covid-19"));
    } else if (objtype instanceof CodeType) {
      extension.setValue(new CodeType());
    }
    extension.getExtensionsByUrl(CdaGeneratorConstants.OMB_RACE_CATEGORY_URL);
    extsList.add(extension);
    return extsList;
  }

  @Test
  void testGetCodeExtension() {
    CodeType codeType =
        CdaFhirUtilities.getCodeExtension(
            getExtsListObj(new CodeType()), CdaGeneratorConstants.FHIR_USCORE_RACE_EXT_URL);
    assertEquals(null, codeType.getValue());
  }

  @Test
  void testGetCodingForCodeSystem() {
    Coding coding =
        CdaFhirUtilities.getCodingForCodeSystem(
            getPatCommComponent().get(0).getLanguage(),
            CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL);
    assertEquals(CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL, coding.getSystem());
  }

  private List<PatientCommunicationComponent> getPatCommComponent() {
    List<PatientCommunicationComponent> comms = new ArrayList<PatientCommunicationComponent>();
    PatientCommunicationComponent patCommComponent = new PatientCommunicationComponent();
    CodeableConcept codeableConcept = new CodeableConcept();
    codeableConcept.setText(CdaGeneratorConstants.GUARDIAN_EL_NAME);

    codeableConcept.setCoding(getCodingListObj());

    patCommComponent.setLanguage(codeableConcept);
    comms.add(patCommComponent);
    return comms;
  }

  private List<Coding> getCodingListObj() {
    List<Coding> codingList = new ArrayList<Coding>();
    codingList.add(getCodingObj());
    return codingList;
  }

  private Coding getCodingObj() {
    Coding coding = FhirGeneratorUtils.getCoding("TestValue", "123456", "Covid-19");
    coding.setSystem(CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL);
    return coding;
  }

  @Test
  void testGetLanguageForCodeSystem() {
    Coding coding =
        CdaFhirUtilities.getLanguageForCodeSystem(
            getPatCommComponent(), CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL);
    assertEquals(CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL, coding.getSystem());
  }

  @Test
  void testGetAddressXml() {
    String adrsString = CdaFhirUtilities.getAddressXml(getAdrsListObj());
    System.out.println("adrsString"+adrsString);
    assert(adrsString.contains("300 Guide Drive"));
    assert(adrsString.contains("Rockville"));
    assert(adrsString.contains("nullFlavor=\"NI\"")); //postal code
  }

  private List<Address> getAdrsListObj() {
    List<Address> adrsList = new ArrayList<Address>();
    StringType line1 = new StringType();
    line1.setValue("300 Guide Drive");
    List<StringType> lines = new ArrayList<StringType>();
    lines.add(line1);    
    
    Address address = new Address();
    address.setCity("Rockville");
    address.setCountry("USA");
    address.setState("MD");
    address.setType(new Enumeration<AddressType>(new AddressTypeEnumFactory()).getValue());
    address.setLine(lines);
    adrsList.add(address);
    return adrsList;
  }

  @Test
  void testGetTelecomXml() {
    String teleComXmlString = CdaFhirUtilities.getTelecomXml(getContactPointListObj("PHONE","HOME"), true);
    assertTrue(teleComXmlString.contains("(299)444-4322"));
    teleComXmlString = CdaFhirUtilities.getTelecomXml(getContactPointListObj("PHONE","MOBILE"), true);
    assertTrue(teleComXmlString.contains("(299)444-4322"));
    teleComXmlString = CdaFhirUtilities.getTelecomXml(getContactPointListObj("PHONE","WORK"), true);
    assertTrue(teleComXmlString.contains("(299)444-4322"));
  }

  private List<ContactPoint> getContactPointListObj(String system,String systemUse) {
		List<ContactPoint> contactPointList = new ArrayList<ContactPoint>();
		ContactPoint contactPoint = new ContactPoint();
		if (system.equalsIgnoreCase("PHONE")) {
			contactPoint.setSystem(ContactPoint.ContactPointSystem.PHONE);
			contactPoint.setValue("2994444322");
		} else if (system.equalsIgnoreCase("EMAIL")) {
			contactPoint.setSystem(ContactPoint.ContactPointSystem.EMAIL);
			contactPoint.setValue("aa@aa.com");
		}
		
		if (system.equalsIgnoreCase("HOME")) {
			contactPoint.setUse(ContactPoint.ContactPointUse.HOME);
		} else if (system.equalsIgnoreCase("MOBILE")) {
			contactPoint.setUse(ContactPoint.ContactPointUse.MOBILE);
		} else if (system.equalsIgnoreCase("WORK")) {
			contactPoint.setUse(ContactPoint.ContactPointUse.MOBILE);
		}
		

		contactPointList.add(contactPoint);
		return contactPointList;
  }

  @Test
  void testGetEmailXml() {
    String emailXmlString = CdaFhirUtilities.getEmailXml(getContactPointListObj("EMAIL","WORK"));
    assertFalse(emailXmlString.contains("email"));
  }

  @Test
  void testGetCodingXmlForMappedConceptDomain() {
	    String expectedResult =
	            "<value xsi:type=\"CD\" code=\"A\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"Abnormal\"><translation code=\"N\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"Normal\"/>";
	        expectedResult +=
	            "\n"
	                + "<translation code=\"L\" codeSystem=\"2.16.840.1.113883.5.83\" codeSystemName=\"v3-ObservationInterpretation\" displayName=\"Low\"/>";
	        expectedResult += "\n" + "</value>" + "\n";
	        String actualResult =
	            CdaFhirUtilities.getCodingXmlForValueForMappedConceptDomain(
	                INTERPRETATION_CODE, codes, INTERPRETATION_CODE, false);
	        assertEquals(expectedResult, actualResult);
  }

  @Test
  void testGetCodingXmlForValueForMappedConceptDomain() {
	    String expectedResult = "<interpretationCode xsi:type=\"CD\" nullFlavor=\"NI\"/>" + "\n";
	    List<Coding> codes = new ArrayList<Coding>();
	    String actualResult =
	        CdaFhirUtilities.getCodingXmlForValueForMappedConceptDomain(
	            INTERPRETATION_CODE, codes, INTERPRETATION_CODE, true);
	    assertEquals(expectedResult, actualResult);
  }

  @Test
  void testGetPractitionersForType() {
    List<Practitioner> practList =
        CdaFhirUtilities.getPractitionersForType(getR4FhirDataObj(), V3ParticipationType.AUT);
    assert (practList.size() <= 0);
  }

  private R4FhirData getR4FhirDataObj() {
    R4FhirData mockR4Data = PowerMockito.mock(R4FhirData.class);
    return mockR4Data;
  }

  @Test
  void testIsCodingPresentForCodeSystem() {
    boolean codingPresent =
        CdaFhirUtilities.isCodingPresentForCodeSystem(
            getCodingListObj(), CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL);
    assert (codingPresent);
  }

  @Test
  void testGetCodingDisplayForCodeSystem() {
    Pair<String, Boolean> comparePair = new Pair<String, Boolean>("Covid-19", true);
    Pair<String, Boolean> pair =
        CdaFhirUtilities.getCodingDisplayForCodeSystem(
            getCodingListObj(), CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL, true);
    assertEquals(comparePair, pair);
  }

  @Test
  void testGetCodeableConceptDisplayForCodeSystem() {
    Pair<String, Boolean> comparePair = new Pair<String, Boolean>("Covid-19", true);
    Pair<String, Boolean> pair =
        CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(
            getCodeableConceptObj(), CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL, false);
    assertEquals(comparePair, pair);
  }

  @Test
  void testGetActualDate() {
    Pair<Date, TimeZone> comparePair =
        new Pair<Date, TimeZone>(new Date(), TimeZone.getTimeZone("America/New_York"));
    Pair<Date, TimeZone> actualDate =
        CdaFhirUtilities.getActualDate(new DateTimeType().setValue(new Date()));
    assertEquals(actualDate.getValue1(), comparePair.getValue1());
  }

  @Test
  void testGetCodeableConceptXmlForCodeSystem() {
    String retString =
        CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
            getCodeableConceptList(),
            CdaGeneratorConstants.GUARDIAN_EL_NAME,
            false,
            CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL,
            false);
    assert (retString.contains(CdaGeneratorConstants.GUARDIAN_EL_NAME));
  }

  @Test
  void testGetCodingForValidCodeSystems() {
    List<CodeableConcept> codeConceptList = getCodeableConceptList();
    String systemUrl = "http://terminology.hl7.org/CodeSystem/failure-action";
    codeConceptList.get(0).getCoding().get(0).setSystem(systemUrl);
    List<Coding> codingList = CdaFhirUtilities.getCodingForValidCodeSystems(codeConceptList);
    assertEquals(systemUrl, codingList.get(0).getSystem());
  }

  @Test
  void testGetCodeableConceptXml() {
    List<CodeableConcept> codeConceptList = getCodeableConceptList();
    String systemUrl = "http://terminology.hl7.org/CodeSystem/failure-action";
    codeConceptList.get(0).getCoding().get(0).setSystem(systemUrl);
    String conceptStr =
        CdaFhirUtilities.getCodeableConceptXml(
            codeConceptList, CdaGeneratorConstants.CODE_EL_NAME, false);
    assert (conceptStr.contains(CdaGeneratorConstants.CODE_EL_NAME));
  }

  @Test
  void testGetCodingXmlForCodeSystem() {
    List<Coding> codingList = getCodingListObj();
    codingList.get(0).setSystem(CdaGeneratorConstants.FHIR_LOINC_URL);
    String xmlCodeStr =
        CdaFhirUtilities.getCodingXmlForCodeSystem(
            getCodingListObj(),
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.FHIR_LOINC_URL,
            false,
            "");
    assert (xmlCodeStr.contains(CdaGeneratorConstants.CODE_EL_NAME));
  }

  @Test
  void testGetCodingXml() {
    List<Coding> codingList = getCodingListObj();
    String failureAction = "failure-action";
    String systemUrl = "http://terminology.hl7.org/CodeSystem/failure-action";
    codingList.get(0).setSystem(systemUrl);
    String xmlCodeStr =
        CdaFhirUtilities.getCodingXml(codingList, CdaGeneratorConstants.CODE_EL_NAME, "");
    assert (xmlCodeStr.contains(failureAction));
  }

  @Test
  void testGetCodingXmlForValueForCodeSystem() {
    List<Coding> codingList = getCodingListObj();
    String failureAction = "failure-action";
    String systemUrl = "http://terminology.hl7.org/CodeSystem/failure-action";
    codingList.get(0).setSystem(systemUrl);
    String xmlCodeStr =
        CdaFhirUtilities.getCodingXmlForValueForCodeSystem(
            codingList,
            CdaGeneratorConstants.VAL_EL_NAME,
            CdaGeneratorConstants.FHIR_SNOMED_URL,
            false);
    assert (xmlCodeStr.contains(failureAction));
  }

  @Test
  void testGetCodingXmlForValue() {
    List<Coding> codingList = getCodingListObj();
    String failureAction = "failure-action";
    String systemUrl = "http://terminology.hl7.org/CodeSystem/failure-action";
    codingList.get(0).setSystem(systemUrl);
    String xmlForValueStr =
        CdaFhirUtilities.getCodingXmlForValue(codingList, CdaGeneratorConstants.CODE_EL_NAME);
    assert (xmlForValueStr.contains(failureAction));
  }

  @Test
  void testGetPeriodXml() {
    Period period = new Period();
    period.setStart(new Date());
    Calendar c = Calendar.getInstance();
    c.setTime(new Date());
    c.add(Calendar.DATE, 15);

    period.setEnd(c.getTime());
    String periodXmlStr =
        CdaFhirUtilities.getPeriodXml(period, CdaGeneratorConstants.EFF_TIME_EL_NAME);
    assert (periodXmlStr.contains(CdaGeneratorConstants.EFF_TIME_EL_NAME));
  }

  @Test
  void testGetQuantityXml() {
    Quantity dose = new Quantity();
    String units = "nos";
    dose.setValue(100);
    dose.setUnit(units);
    String quantXmlStr =
        CdaFhirUtilities.getQuantityXml(dose, CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME, false);
    assert (quantXmlStr.contains(units));
  }

  @Test
  void testGetBirthSexXml() {
    String male = "Male";
    String birthSexXml = CdaFhirUtilities.getBirthSexXml("M");
    assert (birthSexXml.contains(male));
  }

  @Test
  void testGetGenderXml() {
    String adminGenderStr = CdaFhirUtilities.getGenderXml(Enumerations.AdministrativeGender.MALE);
    assert (adminGenderStr.contains(CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME));
  }

  @Test
  void testGetNameXml() {
    String strName = "First Name";
    List<HumanName> humanNames = new ArrayList<HumanName>();
    HumanName humanName = new HumanName();
    List<StringType> theGiven = new ArrayList<StringType>();
    theGiven.add((StringType) new StringType().setValue(strName));
    theGiven.add((StringType) new StringType().setValue("Last Name"));
    humanName.setGiven(theGiven);
    humanName.setFamily("family name");
    humanNames.add(humanName);
    String nameXmlStr = CdaFhirUtilities.getNameXml(humanNames);
    assert (nameXmlStr.contains(strName));
  }

  @Test
  void testGetStringForCoding() {
    String forCodingStr = CdaFhirUtilities.getStringForCoding(getCodingObj());
    assertEquals(forCodingStr, getCodingObj().getDisplay());
  }

  @Test
  void testGetCombinationStringForCodeSystem() {
    CodeableConcept codeableConcept = getCodeableConceptObj();
    codeableConcept.getCoding().get(0).setSystem(CdaGeneratorConstants.FHIR_SNOMED_URL);
    String codeSysetemStr =
        CdaFhirUtilities.getCombinationStringForCodeSystem(
            codeableConcept, getCodeableConceptObj(), CdaGeneratorConstants.FHIR_SNOMED_URL, true);
    assert (codeSysetemStr.contains(codeableConcept.getCoding().get(0).getDisplay()));
  }

  @Test
  void testGetStringForQuantity() {
    String units = "nos";
    Quantity qt = new Quantity();
    qt.setUnit(units);
    DecimalType deElement = new DecimalType();
    deElement.setValue(100);
    qt.setValueElement(deElement);
    UriType uriType = new UriType();
    uriType.setIdElement((StringType) new StringType().setValue("id element"));
    qt.setSystemElement(uriType);
    String forQuantityStr = CdaFhirUtilities.getStringForQuantity(qt);
    assert (forQuantityStr.contains(units));
  }

  @Test
  void testGetStringForMedicationType() {
    MedicationRequest medicationRequest = new MedicationRequest();
    Reference referece = new Reference();
    referece.setReference(
        CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE
            + CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE);
    medicationRequest.setMedication(referece);

    medicationRequest.setContained(getContaintedList());

    String forMedTypeStr = CdaFhirUtilities.getStringForMedicationType(medicationRequest);
    assert (forMedTypeStr.contains("Covid-19"));
  }

  private List<Resource> getContaintedList() {
    List<Resource> containedList = new ArrayList<Resource>();
    Medication med = new Medication();
    CodeableConcept codeableConcept = getCodeableConceptObj();
    codeableConcept.getCoding().get(0).setSystem(CdaGeneratorConstants.FHIR_RXNORM_URL);

    med.setCode(codeableConcept);

    containedList.add(med);
    med.setId(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE);
    return containedList;
  }

  @Test
  void testGetStringForMedicationFromContainedResources() {
    String forMedFromContRefStr =
        CdaFhirUtilities.getStringForMedicationFromContainedResources(
            getContaintedList(), CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE);
    assert (forMedFromContRefStr.contains("Covid-19"));
  }

  @Test
  void testGetStringForType() {
    String stringForTypeStr = CdaFhirUtilities.getStringForType(getCodeableConceptObj());
    assert (stringForTypeStr.contains(CdaGeneratorConstants.GUARDIAN_EL_NAME));
  }

  @Test
  void testGetXmlForType() {
    String xmlForTypeStr =
        CdaFhirUtilities.getXmlForType(
            getCodeableConceptObj(), CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL, false);
    assert (xmlForTypeStr.contains(CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL));
  }

  @Test
  void testGetXmlForMedicationTypeForCodeSystem() {
    MedicationStatement domainResource = new MedicationStatement();
    CodeableConcept codeableConcept = getCodeableConceptObj();
    codeableConcept.getCoding().get(0).setSystem(CdaGeneratorConstants.FHIR_RXNORM_URL);

    String forMedTypeForCodeSystemStr =
        CdaFhirUtilities.getXmlForMedicationTypeForCodeSystem(
            codeableConcept,
            CdaGeneratorConstants.CODE_EL_NAME,
            false,
            CdaGeneratorConstants.FHIR_RXNORM_URL,
            false,
            domainResource);
    assert (forMedTypeForCodeSystemStr.contains("Covid-19"));
  }

  @Test
  void testGetXmlForTypeForCodeSystem() {
    CodeableConcept codeableConcept = getCodeableConceptObj();
    codeableConcept.getCoding().get(0).setSystem(CdaGeneratorConstants.FHIR_RXNORM_URL);

    String xmlForTypeForCodeSystem =
        CdaFhirUtilities.getXmlForTypeForCodeSystem(
            codeableConcept,
            CdaGeneratorConstants.CODE_EL_NAME,
            false,
            CdaGeneratorConstants.FHIR_RXNORM_URL,
            false);
    assert (xmlForTypeForCodeSystem.contains("Covid-19"));
  }

  @Test
  void testGetMatchedCodesForResourceAndUrl() {
    CdaFhirUtilities.getMatchedCodesForResourceAndUrl(
        expectedLaunchDetails, OBSERVATION, CdaGeneratorConstants.FHIR_LOINC_URL);
  }

  @Test
  void testGetMatchedValuesForResourceAndUrl() {
    CdaFhirUtilities.getMatchedValuesForResourceAndUrl(
        expectedLaunchDetails, OBSERVATION, CdaGeneratorConstants.FHIR_LOINC_URL);
  }

  @Test
  void testIsCodePresent() {
    String code = "aa";
    List<String> codesList = new ArrayList<String>();
    codesList.add(code);
    boolean codePresent = CdaFhirUtilities.isCodePresent(codesList, code);
    assert (codePresent);
  }

  @Test
  void testGetMatchingCodeFromCodingForCodeSystem() {
    CdaFhirUtilities.getMatchingCodeFromCodingForCodeSystem(
        null, getCodingListObj(), CdaGeneratorConstants.FHIR_LOINC_URL);
  }

  @Test
  void testGetMatchingCodeFromTypeForCodeSystem() {
    String code = "123456";
    List<String> codesList = new ArrayList<String>();
    codesList.add(code);
    CodeableConcept codeableConcept = getCodeableConceptObj();
    codeableConcept.getCoding().get(0).setSystem(CdaGeneratorConstants.FHIR_RXNORM_URL);

    String matCodeFromTypeForCodeSystemStr =
        CdaFhirUtilities.getMatchingCodeFromTypeForCodeSystem(
            codesList, codeableConcept, CdaGeneratorConstants.FHIR_RXNORM_URL);
    assert (matCodeFromTypeForCodeSystemStr.contains(code));
  }

  @Test
  void testGetMatchingCodeFromCodeableConceptForCodeSystem() {
    String code = "123456";
    List<String> codesList = new ArrayList<String>();
    codesList.add(code);
    CodeableConcept codeableConcept = getCodeableConceptObj();
    codeableConcept.getCoding().get(0).setSystem(CdaGeneratorConstants.FHIR_RXNORM_URL);
    String matCodeFromCodeConcForCodeStystemStr =
        CdaFhirUtilities.getMatchingCodeFromTypeForCodeSystem(
            codesList, codeableConcept, CdaGeneratorConstants.FHIR_RXNORM_URL);
    assert (matCodeFromCodeConcForCodeStystemStr.contains(code));
  }

  @Test
  void testGetXmlForCodeableConceptWithCDAndValueSetAndVersion() {
    String code = "123456";
    List<String> codesList = new ArrayList<String>();
    codesList.add(code);
    CodeableConcept codeableConcept = getCodeableConceptObj();
    codeableConcept.getCoding().get(0).setSystem(CdaGeneratorConstants.FHIR_RXNORM_URL);
    String contentRef =
        CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT + Integer.toString(1);
    String retValue =
        CdaFhirUtilities.getXmlForCodeableConceptWithCDAndValueSetAndVersion(
            CdaGeneratorConstants.CODE_EL_NAME,
            code,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.RCTC_OID,
            ActionRepo.getInstance().getRctcVersion(),
            codeableConcept,
            CdaGeneratorConstants.FHIR_LOINC_URL,
            contentRef);
    assert (retValue.contains(code));
  }

  private void createTestDataForStatusCodeTest() {
    ObjectMapper mapper = TestUtils.getJsonMapper();
    String testDataJson = TestUtils.getFileContentAsString("R4/Misc/FhirStatusCodeTestData.json");
    Map<String, String> testMap = null;
    try {
      testMap = mapper.readValue(testDataJson, Map.class);
    } catch (JsonProcessingException e) {
      logger.error("Error in reading the test data:::", e);
    }
    if (testMap != null) {
      Set<String> dataKeySet = testMap.keySet();

      for (String key : dataKeySet) {

        String value = testMap.get(key);

        List<String> valueList = Arrays.stream(value.split("\\|")).collect(Collectors.toList());

        testData.put(key, valueList);
      }
    }
  }

  private Patient getPatientObj() {
    Patient p = new Patient();
    List<Identifier> identList = new ArrayList<Identifier>();
    identList.add(getIdentiferObj());
    p.setIdentifier(identList);
    return p;
  }

  private Identifier getIdentiferObj() {
    Identifier identifer = new Identifier();
    CodeableConcept codealbeConcept = new CodeableConcept();
    List<Coding> codingList = new ArrayList<Coding>();
    Coding coding = new Coding();
    coding.setCode(CdaFhirEnumConstants.FHIR_ID_TYPE_MR);
    coding.setSystem(CdaGeneratorConstants.FHIR_IDENTIFIER_TYPE_SYSTEM);
    codingList.add(coding);

    codealbeConcept.setCoding(codingList);
    identifer.setType(codealbeConcept);
    return identifer;
  }
  private void createTestDataForCodingXML() {
	    String testDataJson = TestUtils.getFileContentAsString("R4/Misc/FhirToCDAMappedCodes.json");
	    JSONArray array = new JSONArray(testDataJson);
	    for (Object obj : array) {
	      JSONObject jsonObj = (JSONObject) obj;
	      Coding coding = new Coding();
	      coding.setCode(jsonObj.getString("code"));
	      coding.setDisplay(jsonObj.getString("display"));
	      coding.setSystem(jsonObj.getString("system"));
	      codes.add(coding);
	    }
  }
}
