package com.drajer.cdafromdstu2;


import ca.uhn.fhir.model.api.ExtensionDt;
import ca.uhn.fhir.model.api.IDatatype;
import ca.uhn.fhir.model.dstu2.composite.*;
import ca.uhn.fhir.model.dstu2.resource.*;
import ca.uhn.fhir.model.dstu2.valueset.*;
import ca.uhn.fhir.model.primitive.*;

import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import org.hl7.fhir.dstu2.model.Reference;
import org.javatuples.Pair;
import org.junit.Test;
import ca.uhn.fhir.model.dstu2.composite.AddressDt;
import ca.uhn.fhir.model.dstu2.composite.BoundCodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import ca.uhn.fhir.model.dstu2.composite.ContactPointDt;
import ca.uhn.fhir.model.dstu2.composite.IdentifierDt;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.composite.QuantityDt;
import ca.uhn.fhir.model.dstu2.composite.TimingDt;

import ca.uhn.fhir.model.dstu2.valueset.IdentifierTypeCodesEnum;
import ca.uhn.fhir.model.primitive.CodeDt;


import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import static com.drajer.cda.utils.CdaGeneratorConstants.*;
import static com.helger.commons.mock.CommonsAssert.assertEquals;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class Dstu2CdaFhirUtilitiesTest extends  BaseGeneratorTest{
    private String elName = "testElement";
    private boolean valFlag = true;

    private static final String CODE_SYSTEM_URL = "http://loinc.org";
    private static final String CODE = "12345";
    private static final String DISPLAY = "Test Code Display";
    private static final String CD_NAME = "TestCode";
    private static final String FIXED_DATE_STRING = "2025-01-01T12:00:00";
    private static final TimeZone UTC_TIMEZONE = TimeZone.getTimeZone("UTC");

    @Test
    public void testGetIdentifierForType_notFound() {
        IdentifierDt expectedIdentifier = new IdentifierDt();
        BoundCodeableConceptDt<IdentifierTypeCodesEnum> enumBoundCodeableConceptDt = new BoundCodeableConceptDt<>();
        CodingDt codingDt= new CodingDt();
        codingDt.setSystem(FHIR_IDTYPE_SYSTEM);
        codingDt.setCode("1234");


        enumBoundCodeableConceptDt.setCoding(Collections.singletonList(codingDt));

        expectedIdentifier.setType(enumBoundCodeableConceptDt);

        List<IdentifierDt> identifiers = Collections.singletonList(expectedIdentifier);
        IdentifierTypeCodesEnum typeCode =IdentifierTypeCodesEnum.SOCIAL_BENEFICIARY_IDENTIFIER;

        IdentifierDt result = Dstu2CdaFhirUtilities.getIdentifierForType(identifiers, typeCode);

        assertNull(result);
    }

    @Test
    public void testGetIdentifierForType_Found() {
        IdentifierDt expectedIdentifier = new IdentifierDt();
        BoundCodeableConceptDt<IdentifierTypeCodesEnum> enumBoundCodeableConceptDt = new BoundCodeableConceptDt<>();
        CodingDt codingDt= new CodingDt();
        codingDt.setSystem(FHIR_IDTYPE_SYSTEM);
        codingDt.setCode(IdentifierTypeCodesEnum.SOCIAL_BENEFICIARY_IDENTIFIER.getCode());


        enumBoundCodeableConceptDt.setCoding(Collections.singletonList(codingDt));

        expectedIdentifier.setType(enumBoundCodeableConceptDt);

        List<IdentifierDt> identifiers = Collections.singletonList(expectedIdentifier);
        IdentifierTypeCodesEnum typeCode =IdentifierTypeCodesEnum.SOCIAL_BENEFICIARY_IDENTIFIER;

        IdentifierDt result = Dstu2CdaFhirUtilities.getIdentifierForType(identifiers, typeCode);

        assertNotNull(result);
    }

    @Test
    public void testGetIdentifierForType_NotFound() {
        List<IdentifierDt> identifiers = new ArrayList<>();
        IdentifierTypeCodesEnum typeCode =IdentifierTypeCodesEnum.SOCIAL_BENEFICIARY_IDENTIFIER;

        IdentifierDt result = Dstu2CdaFhirUtilities.getIdentifierForType(identifiers, typeCode);

        assertNull(result);

    }

    @Test
    public void testGetIdentifierForType_NullList() {
        IdentifierTypeCodesEnum typeCode =IdentifierTypeCodesEnum.SOCIAL_BENEFICIARY_IDENTIFIER;

        IdentifierDt result = Dstu2CdaFhirUtilities.getIdentifierForType(null, typeCode);

        assertNull(result);
    }

    @Test
    public void testIdentifierFound() {
        List<IdentifierDt> ids = Arrays.asList(
                new IdentifierDt("http://example.com/system1", "12345"),
                new IdentifierDt("http://example.com/system2", "67890")
        );

        IdentifierDt result = Dstu2CdaFhirUtilities.getIdentifierForSystem(ids, "http://example.com/system1");

        assertNotNull(result);
        assertEquals("http://example.com/system1", result.getSystem());
        assertEquals("12345", result.getValue());
    }

    @Test
    public void testIdentifierNotFound() {
        List<IdentifierDt> ids = Arrays.asList(
                new IdentifierDt("http://example.com/system1", "12345"),
                new IdentifierDt("http://example.com/system2", "67890")
        );

        IdentifierDt result = Dstu2CdaFhirUtilities.getIdentifierForSystem(ids, "http://example.com/system3");

        assertNull(result);
    }

    @Test
    public void testEmptyList() {
        List<IdentifierDt> ids = Arrays.asList();

        IdentifierDt result = Dstu2CdaFhirUtilities.getIdentifierForSystem(ids, "http://example.com/system1");

        assertNull(result);
    }

    @Test
    public void testNullList() {
        IdentifierDt result = Dstu2CdaFhirUtilities.getIdentifierForSystem(null, "http://example.com/system1");

        assertNull(result);
    }

    @Test
    public void testNullSystem() {
        List<IdentifierDt> ids = Arrays.asList(
                new IdentifierDt("http://example.com/system1", "12345")
        );

        IdentifierDt result = Dstu2CdaFhirUtilities.getIdentifierForSystem(ids, null);

        assertNull(result);
    }

    @Test
    public void testGetCodeableConceptExtension_WhenExtensionExists() {
        // Create a CodeableConcept with a Coding
        CodeableConceptDt expectedConcept = new CodeableConceptDt();
        expectedConcept.addCoding(new CodingDt("http://snomed.info/sct", "44054006")).setText("Diabetes Type 2");

        ExtensionDt ext1 = new ExtensionDt().setUrl("http://hosp.com/patient-blood-type");
        ExtensionDt ext2 = new ExtensionDt().setUrl("http://hosp.com/patient-condition").setValue(expectedConcept);
        ExtensionDt ext3 = new ExtensionDt().setUrl("http://hosp.com/patient-allergy");

        List<ExtensionDt> extensions = Arrays.asList(ext1, ext2, ext3);

        CodeableConceptDt actualConcept =
                Dstu2CdaFhirUtilities.getCodeableConceptExtension(extensions, "http://hosp.com/patient-condition");

        assertNotNull(actualConcept);
        assertFalse(actualConcept.getCoding().isEmpty());
        assertEquals("http://snomed.info/sct", actualConcept.getCodingFirstRep().getSystem());
        assertEquals("44054006", actualConcept.getCodingFirstRep().getCode());
        assertEquals("Diabetes Type 2", actualConcept.getText());
    }

    @Test
    public void testGetCodeableConceptExtension_WhenExtensionDoesNotExist() {
        CodeableConceptDt conditionConcept = new CodeableConceptDt();
        conditionConcept.addCoding(new CodingDt("http://loinc.org", "12345-6")).setText("Hypertension");

        ExtensionDt ext1 = new ExtensionDt().setUrl("http://hosp.com/patient-blood-type").setValue(conditionConcept);
        ExtensionDt ext2 = new ExtensionDt().setUrl("http://hosp.com/patient-allergy");

        List<ExtensionDt> extensions = Arrays.asList(ext1, ext2);

        CodeableConceptDt actualConcept =
                Dstu2CdaFhirUtilities.getCodeableConceptExtension(extensions, "http://hosp.com/patient-medication");

        assertNull(actualConcept);
    }

    @Test
    public void testGetCodeableConceptExtension_WithNullList() {
        List<ExtensionDt> extensions = null;

        CodeableConceptDt actualConcept =
                Dstu2CdaFhirUtilities.getCodeableConceptExtension(extensions, "http://hosp.com/patient-condition");

        assertNull(actualConcept);
    }

    @Test
    public void testGetCodeableConceptExtension_WithEmptyList() {
        List<ExtensionDt> extensions = Arrays.asList();

        CodeableConceptDt actualConcept =
                Dstu2CdaFhirUtilities.getCodeableConceptExtension(extensions, "http://hosp.com/patient-condition");

        assertNull(actualConcept);
    }

    @Test
    public void testGetCodeableConceptExtension_WithNullUrlInExtension() {
        ExtensionDt ext1 = new ExtensionDt(); // No URL
        ExtensionDt ext2 = new ExtensionDt().setUrl("http://hosp.com/patient-diagnosis")
                .setValue(new CodeableConceptDt().addCoding(new CodingDt("http://snomed.info/sct", "123456"))
                        .setText("Asthma"));

        List<ExtensionDt> extensions = Arrays.asList(ext1, ext2);

        CodeableConceptDt actualConcept =
                Dstu2CdaFhirUtilities.getCodeableConceptExtension(extensions, "http://hosp.com/patient-diagnosis");

        assertNotNull(actualConcept);
        assertEquals("Asthma", actualConcept.getText());
        assertFalse(actualConcept.getCoding().isEmpty());
        assertEquals("http://snomed.info/sct", actualConcept.getCodingFirstRep().getSystem());
        assertEquals("123456", actualConcept.getCodingFirstRep().getCode());
    }

    @Test
    public void testGetLanguage() {
        // Arrange
        List<Patient.Communication> comms = new ArrayList<>();
        Patient.Communication communication = new Patient.Communication();

        CodeableConceptDt languageCodeableConcept = new CodeableConceptDt();
        CodingDt language = new CodingDt();
        language.setCode("en-US");
        language.setSystem("http://hl7.org/fhir/ValueSet/languages");
        language.setDisplay("English (United States)");

        languageCodeableConcept.setText("patient language");
        languageCodeableConcept.setCoding(Collections.singletonList(language));
        communication.setLanguage(languageCodeableConcept);
        comms.add(communication);
        CodingDt result = Dstu2CdaFhirUtilities.getLanguage(comms);

        assertNotNull(result);
        assertEquals(language.getCode(), result.getCode());
        assertEquals(language.getSystem(), result.getSystem());
        assertEquals(language.getDisplay(), result.getDisplay());
    }

    @Test
    public void testGetLanguage_EmptyList() {

        List<Patient.Communication> comms = new ArrayList<>();

        CodingDt result = Dstu2CdaFhirUtilities.getLanguage(comms);

        assertNull(result);
    }

    @Test
    public void testGetLanguage_NullList() {
        List<Patient.Communication> comms = null;
        CodingDt result = Dstu2CdaFhirUtilities.getLanguage(comms);
        assertNull(result);
    }

    @Test
    public void testGetLanguage_NoValidCoding() {
        // Arrange
        List<Patient.Communication> comms = new ArrayList<>();
        Patient.Communication communication = new Patient.Communication();
        CodeableConceptDt languageCodeableConcept = new CodeableConceptDt();
        languageCodeableConcept.setText("patient language");
        communication.setLanguage(languageCodeableConcept);
        comms.add(communication);

        CodingDt result = Dstu2CdaFhirUtilities.getLanguage(comms);
        assertNull(result);
    }

    @Test
    public void testGetCodeExtension_Dstu2() {
        // Arrange
        List<ExtensionDt> exts = new ArrayList<>();
        ExtensionDt extension = new ExtensionDt();
        String extUrl = "http://hosp.com/ext";
        CodeDt extCodeDt = new CodeDt("123");

        extension.setUrl(extUrl);
        extension.setValue(extCodeDt);
        exts.add(extension);


        CodeDt result = Dstu2CdaFhirUtilities.getCodeExtension(exts, extUrl);

        assertNotNull(result);
        assertEquals(extCodeDt.getValue(), result.getValue());

        CodeDt resultNull = Dstu2CdaFhirUtilities.getCodeExtension(null, extUrl);
        assertNull(resultNull);

        List<ExtensionDt> emptyExts = new ArrayList<>();
        CodeDt resultEmpty = Dstu2CdaFhirUtilities.getCodeExtension(emptyExts, extUrl);
        assertNull(resultEmpty);

        String nonExistingExtUrl = "http://nonexistent.com/ext";
        CodeDt resultNotFound = Dstu2CdaFhirUtilities.getCodeExtension(exts, nonExistingExtUrl);
        assertNull(resultNotFound);
    }
    @Test
    public void testGetStringForCoding_Dstu2() {

        CodingDt coding = new CodingDt();
        coding.setSystem("http://loinc.org");
        coding.setCode("15074-8");
        coding.setDisplay("Glucose [Moles/volume] in Blood");

        String actual = Dstu2CdaFhirUtilities.getStringForCoding(coding);

        assertEquals("Glucose [Moles/volume] in Blood", actual);

        CodingDt coding1 = new CodingDt();
        coding1.setCode("15074-8");
        coding1.setSystem("http://loinc.org");

        actual = Dstu2CdaFhirUtilities.getStringForCoding(coding1);

        assertEquals("http://loinc.org|15074-8", actual);
    }

    @Test
    public void testGetStringForQuantity_Dstu2() {


        QuantityDt quantity = new QuantityDt();
        quantity.setValue(100);
        quantity.setSystem("http://snomed.info/sct");
        quantity.setUnit("mg");

        String expectedResult = "100|http://snomed.info/sct|mg";
        String actualResult = Dstu2CdaFhirUtilities.getStringForQuantity(quantity);
        assertEquals(expectedResult, actualResult);


        QuantityDt quantity1 = new QuantityDt();
        quantity1.setValue(100);

        expectedResult = "Unknown";
        actualResult = Dstu2CdaFhirUtilities.getStringForQuantity(quantity1);
        assertEquals(expectedResult, actualResult);


        expectedResult = "Unknown";
        actualResult = Dstu2CdaFhirUtilities.getStringForQuantity(null);
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void testIsCodePresent_MatchCodes() {

        List<String> matchCodes = Arrays.asList("code1", "code2", "code3");
        String code = "code2";
        Boolean result = Dstu2CdaFhirUtilities.isCodePresent(matchCodes, code);
        assertEquals(true, result);

        Boolean result1 = Dstu2CdaFhirUtilities.isCodePresent(matchCodes, "code4");
        assertEquals(false, result1);
    }



    @Test
    public void testGetMatchingCodeFromCodingForCodeSystem_Dstu2() {

        List<String> matchedCodes = Arrays.asList("26464-2", "718-7");
        List<CodingDt> cds =
                Arrays.asList(
                        new CodingDt().setSystem("http://loinc.org").setCode("26464-2"),
                        new CodingDt().setSystem("http://loinc.org").setCode("718-7"),
                        new CodingDt().setSystem("http://loinc.org").setCode("48642-3"));

        String csUrl = "http://loinc.org";

        String result = Dstu2CdaFhirUtilities.getMatchingCodeFromCodingForCodeSystem(matchedCodes, cds, csUrl);

        assertEquals("26464-2", result);
    }

    @Test
    public void testGetMatchingCodeFromCodingForCodeSystem_EmptyMatchedCodes_Dstu2() {
        List<String> matchedCodes = new ArrayList<>();
        List<CodingDt> cds =
                Arrays.asList(
                        new CodingDt().setSystem("http://loinc.org").setCode("26464-2"),
                        new CodingDt().setSystem("http://loinc.org").setCode("718-7"));

        String csUrl = "http://loinc.org";

        String result = Dstu2CdaFhirUtilities.getMatchingCodeFromCodingForCodeSystem(matchedCodes, cds, csUrl);

        assertEquals("", result);
    }



    @Test
    public void testGetLocation_ValidMatchingLocation_Dstu2() {

        Encounter encounter = new Encounter();
        Encounter.Location locationRef = encounter.getLocationFirstRep();
        ResourceReferenceDt locationReference = new ResourceReferenceDt("Location/123");
        locationRef.setLocation(locationReference);


        Entry locationEntry = new Entry();
        Location locationResource = new Location();
        locationResource.setId("123");
        locationEntry.setResource(locationResource);


        List<Entry> entries = new ArrayList<>();
        entries.add(locationEntry);


        Location result = Dstu2CdaFhirUtilities.getLocation(entries, encounter);
        assertNotNull(result);
        assertEquals("123", result.getId().getIdPart());
    }

    @Test
    public void testGetLocation_NoMatchingLocation_Dstu2() {

        Encounter encounter = new Encounter();
        Encounter.Location locationRef = encounter.getLocationFirstRep();
        ResourceReferenceDt locationReference = new ResourceReferenceDt("Location/999");
        locationRef.setLocation(locationReference);


        Entry locationEntry = new Entry();
        Location locationResource = new Location();
        locationResource.setId("123");
        locationEntry.setResource(locationResource);


        List<Entry> entries = new ArrayList<>();
        entries.add(locationEntry);


        Location result = Dstu2CdaFhirUtilities.getLocation(entries, encounter);
        assertNull(result);
    }

    @Test
    public void testGetLocation_NullEncounterLocation_Dstu2() {

        Encounter encounter = new Encounter();


        List<Entry> entries = new ArrayList<>();


        Location result = Dstu2CdaFhirUtilities.getLocation(entries, encounter);
        assertNull(result);
    }

    @Test
    public void testGetResourceEntryForId_ValidMatchingEntry() {

        Location location = new Location();
        location.setId(new IdDt("Location", "123"));


        Entry locationEntry = new Entry();
        locationEntry.setResource(location);


        List<Entry> entries = new ArrayList<>();
        entries.add(locationEntry);


        Entry result = Dstu2CdaFhirUtilities.getResourceEntryForId("123", "Location", entries);
        assertNotNull(result);
        assertEquals("123", result.getResource().getId().getIdPart());
    }

    @Test
    public void testGetResourceEntryForId_NoMatchingEntry() {

        Location location = new Location();
        location.setId(new IdDt("Location", "456"));

        Entry locationEntry = new Entry();
        locationEntry.setResource(location);


        List<Entry> entries = new ArrayList<>();
        entries.add(locationEntry);


        Entry result = Dstu2CdaFhirUtilities.getResourceEntryForId("123", "Location", entries);
        assertNull(result);
    }

    @Test
    public void testGetResourceEntryForId_EmptyEntriesList() {

        List<Entry> entries = new ArrayList<>();


        Entry result = Dstu2CdaFhirUtilities.getResourceEntryForId("123", "Location", entries);
        assertNull(result);
    }


        @Test
        public void testGetCodingExtension_TopLevelMatch() {
            CodingDt coding = new CodingDt("http://hosp.com", "CODE123");
            ExtensionDt ext = new ExtensionDt();
            ext.setUrl("http://hosp.com/ext");
            ext.setValue(coding);

            List<ExtensionDt> exts = new ArrayList<>();
            exts.add(ext);

            CodingDt result = Dstu2CdaFhirUtilities.getCodingExtension(exts, "http://hosp.com/ext", "http://hosp.com/subext");

            assertNotNull(result);
            assertEquals("CODE123", result.getCode());
            assertEquals("http://hosp.com", result.getSystem());
        }

        @Test
        public void testGetCodingExtension_SubExtensionMatch() {
            CodingDt coding = new CodingDt("http://hosp.com", "SUBCODE123");
            ExtensionDt subExt = new ExtensionDt();
            subExt.setUrl("http://hosp.com/subext");
            subExt.setValue(coding);

            ExtensionDt parentExt = new ExtensionDt();
            parentExt.setUrl("http://hosp.com/ext");
            parentExt.addUndeclaredExtension(subExt);

            List<ExtensionDt> exts = new ArrayList<>();
            exts.add(parentExt);

            CodingDt result = Dstu2CdaFhirUtilities.getCodingExtension(exts, "http://hosp.com/ext", "http://hosp.com/subext");

            assertNotNull(result);
            assertEquals("SUBCODE123", result.getCode());
            assertEquals("http://hosp.com", result.getSystem());
        }

        @Test
        public void testGetCodingExtension_NoMatch() {
            ExtensionDt ext = new ExtensionDt();
            ext.setUrl("http://wrong-url.com");

            List<ExtensionDt> exts = new ArrayList<>();
            exts.add(ext);

            CodingDt result = Dstu2CdaFhirUtilities.getCodingExtension(exts, "http://hosp.com/ext", "http://hosp.com/subext");

            assertNull(result);
        }

        @Test
        public void testGetCodingExtension_NullOrEmptyList() {
            assertNull(Dstu2CdaFhirUtilities.getCodingExtension(null, "http://hosp.com/ext", "http://hosp.com/subext"));
            assertNull(Dstu2CdaFhirUtilities.getCodingExtension(new ArrayList<>(), "http://hosp.com/ext", "http://hosp.com/subext"));
        }

        @Test
        public void testGetCodingExtension_UnexpectedValueType() {
            ExtensionDt ext = new ExtensionDt();
            ext.setUrl("http://hosp.com/ext");
            ext.setValue(new ca.uhn.fhir.model.primitive.StringDt("UnexpectedType"));

            List<ExtensionDt> exts = new ArrayList<>();
            exts.add(ext);

            CodingDt result = Dstu2CdaFhirUtilities.getCodingExtension(exts, "http://hosp.com/ext", "http://hosp.com/subext");

            assertNull(result);
        }
    @Test
    public void testGetAddressXml() {
        List<AddressDt> addrs;

        // Test case 1: Valid Home Address
        addrs = new ArrayList<>();
        AddressDt ad1 = new AddressDt();
        ad1.setUse(AddressUseEnum.HOME);
        ad1.addLine("142 Example Drive");
        ad1.setCity("Albany");
        ad1.setState("NY");
        ad1.setPostalCode("NY12");
        ad1.setCountry("USA");
        addrs.add(ad1);

        String result1 = Dstu2CdaFhirUtilities.getAddressXml(addrs);
        assertTrue(result1.contains("<city>Albany</city>"));
        assertTrue(result1.contains("<streetAddressLine>142 Example Drive</streetAddressLine>"));
        assertTrue(result1.contains("<state>NY</state>"));
        assertTrue(result1.contains("<postalCode>NY12</postalCode>"));
        assertTrue(result1.contains("<country>USA</country>"));

        // Test case 2: Address with missing fields (Should have nullFlavor="NI")
        addrs = new ArrayList<>();
        AddressDt ad2 = new AddressDt();
        ad2.setUse(AddressUseEnum.HOME);
        ad2.addLine("142 Example Drive");
        addrs.add(ad2);

        String result2 = Dstu2CdaFhirUtilities.getAddressXml(addrs);
        assertTrue(result2.contains("<streetAddressLine>142 Example Drive</streetAddressLine>"));
        assertTrue(result2.contains("<city nullFlavor=\"NI\"/>"));
        assertTrue(result2.contains("<state nullFlavor=\"NI\"/>"));
        assertTrue(result2.contains("<postalCode nullFlavor=\"NI\"/>"));
        assertTrue(result2.contains("<country nullFlavor=\"NI\"/>"));

        // Test case 3: Address with only city, state, and postal code
        addrs = new ArrayList<>();
        AddressDt ad3 = new AddressDt();
        ad3.setUse(AddressUseEnum.HOME);
        ad3.setCity("Vegas");
        ad3.setState("California");
        ad3.setPostalCode("12345");
        ad3.setCountry("US");
        addrs.add(ad3);

        String result3 = Dstu2CdaFhirUtilities.getAddressXml(addrs);
        assertTrue(result3.contains("<streetAddressLine nullFlavor=\"NI\"/>"));
        assertTrue(result3.contains("<city>Vegas</city>"));
        assertTrue(result3.contains("<state>California</state>"));
        assertTrue(result3.contains("<postalCode>12345</postalCode>"));
        assertTrue(result3.contains("<country>US</country>"));

        // Test case 4: Valid Work Address
        addrs = new ArrayList<>();
        AddressDt ad4 = new AddressDt();
        ad4.setUse(AddressUseEnum.WORK);
        ad4.addLine("185 Example Avenue");
        ad4.setCity("Ashton");
        ad4.setState("ND");
        ad4.setPostalCode("30874");
        ad4.setCountry("US");
        addrs.add(ad4);

        String result4 = Dstu2CdaFhirUtilities.getAddressXml(addrs);
        assertTrue(result4.contains("<streetAddressLine>185 Example Avenue</streetAddressLine>"));
        assertTrue(result4.contains("<city>Ashton</city>"));
        assertTrue(result4.contains("<state>ND</state>"));
        assertTrue(result4.contains("<postalCode>30874</postalCode>"));
        assertTrue(result4.contains("<country>US</country>"));

        // Test case 5: Temporary Address (Should not be included)
        addrs = new ArrayList<>();
        AddressDt ad5 = new AddressDt();
        ad5.setUse(AddressUseEnum.TEMPORARY);
        ad5.addLine("Invalid Address");
        ad5.setCity("Neterhart");
        ad5.setState("SD");
        ad5.setPostalCode("40874");
        ad5.setCountry("US");
        addrs.add(ad5);

        String result5 = Dstu2CdaFhirUtilities.getAddressXml(addrs);
        assertFalse(result5.contains("Invalid Address")); // Temporary address should not be included

        // Test case 6: Null Address List
        String result6 = Dstu2CdaFhirUtilities.getAddressXml(null);
        assertTrue(result6.contains("<streetAddressLine nullFlavor=\"NI\"/>"));
        assertTrue(result6.contains("<city nullFlavor=\"NI\"/>"));
        assertTrue(result6.contains("<state nullFlavor=\"NI\"/>"));
        assertTrue(result6.contains("<postalCode nullFlavor=\"NI\"/>"));
        assertTrue(result6.contains("<country nullFlavor=\"NI\"/>"));

        // Test case 7: Empty Address List
        addrs = new ArrayList<>();
        String result7 = Dstu2CdaFhirUtilities.getAddressXml(addrs);
        assertTrue(result7.contains("<streetAddressLine nullFlavor=\"NI\"/>"));
        assertTrue(result7.contains("<city nullFlavor=\"NI\"/>"));
        assertTrue(result7.contains("<state nullFlavor=\"NI\"/>"));
        assertTrue(result7.contains("<postalCode nullFlavor=\"NI\"/>"));
        assertTrue(result7.contains("<country nullFlavor=\"NI\"/>"));
    }


    @Test
    public void testGetEmailXmlWithNoEmails_Dstu2() {
        String result = Dstu2CdaFhirUtilities.getEmailXml(null);
        assertEquals("<telecom nullFlavor=\"NI\"/>", result.trim());
    }

    @Test
    public void testGetEmailXmlWithOnlyPhoneNumbers() {
        List<ContactPointDt> cps = new ArrayList<>();

        ContactPointDt cp1 = new ContactPointDt();
        cp1.setUse(ContactPointUseEnum.HOME);
        cp1.setSystem(ContactPointSystemEnum.PHONE);
        cp1.setValue("1234567890");
        cps.add(cp1);

        ContactPointDt cp2 = new ContactPointDt();
        cp2.setUse(ContactPointUseEnum.MOBILE);
        cp2.setSystem(ContactPointSystemEnum.PHONE);
        cp2.setValue("0987654321");
        cps.add(cp2);

        // No email should be found, so nullFlavor="NI" should be returned
        String result = Dstu2CdaFhirUtilities.getEmailXml(cps);
        assertEquals("", result.trim());
    }

    @Test
    public void testGetEmailXmlWithSingleEmail() {
        List<ContactPointDt> cps = new ArrayList<>();

        ContactPointDt cp1 = new ContactPointDt();
        cp1.setUse(ContactPointUseEnum.WORK);
        cp1.setSystem(ContactPointSystemEnum.EMAIL);
        cp1.setValue("a@b.com");
        cps.add(cp1);

        String result = Dstu2CdaFhirUtilities.getEmailXml(cps);
        assertEquals("<telecom value=\"mailto:a@b.com\" use=\"WP\"/>", result.trim());
    }

    @Test
    public void testGetEmailXmlWithMultipleEmails() {
        List<ContactPointDt> cps = new ArrayList<>();

        ContactPointDt cp1 = new ContactPointDt();
        cp1.setUse(ContactPointUseEnum.HOME);
        cp1.setSystem(ContactPointSystemEnum.EMAIL);
        cp1.setValue("home@example.com");
        cps.add(cp1);

        ContactPointDt cp2 = new ContactPointDt();
        cp2.setUse(ContactPointUseEnum.WORK);
        cp2.setSystem(ContactPointSystemEnum.EMAIL);
        cp2.setValue("work@example.com");
        cps.add(cp2);

        // Should only return the first email found (home@example.com)
        String result = Dstu2CdaFhirUtilities.getEmailXml(cps);
        assertEquals("<telecom value=\"mailto:home@example.com\" use=\"HP\"/>", result.trim());
    }

    @Test
    public void testGetEmailXmlWithEmptyList() {
        List<ContactPointDt> cps = new ArrayList<>();
        String result = Dstu2CdaFhirUtilities.getEmailXml(cps);
        assertEquals("<telecom nullFlavor=\"NI\"/>", result.trim());
    }


    @Test
    public void testGetDateTimeTypeXmlWithValidDateTime() {
        DateTimeDt dateTime = new DateTimeDt("2023-04-03T15:30:00-04:00");
        String expectedXml = "<effectiveTime value=\"20230403153000-0400\"/>";
        String actualXml = Dstu2CdaFhirUtilities.getDateTimeTypeXml(dateTime, "effectiveTime");
        assertEquals(expectedXml.trim(), actualXml.trim());
    }

    @Test
    public void testGetDateTimeTypeXmlWithDateOnly() {
        DateTimeDt dateTime = new DateTimeDt("2023-04-03");
        String expectedXml = "<effectiveTime value=\"20230403\"/>";
        String actualXml = Dstu2CdaFhirUtilities.getDateTimeTypeXml(dateTime, "effectiveTime");
        assertEquals(expectedXml.trim(), actualXml.trim());
    }

    @Test
    public void testGetDateTimeTypeXmlWithNullDateTime() {
        String expectedXml = "<effectiveTime nullFlavor=\"NI\"/>";
        String actualXml = Dstu2CdaFhirUtilities.getDateTimeTypeXml(null, "effectiveTime");
        assertEquals(expectedXml.trim(), actualXml.trim());
    }

    @Test
    public void testGetDateTimeTypeXmlWithEmptyDate() {
        DateTimeDt dateTime = new DateTimeDt();
        String expectedXml = "<effectiveTime nullFlavor=\"NI\"/>";
        String actualXml = Dstu2CdaFhirUtilities.getDateTimeTypeXml(dateTime, "effectiveTime");
        assertEquals(expectedXml.trim(), actualXml.trim());
    }
    @Test
    public void testGetDisplayStringForDateTimeType_Dstu2() {

        // Test with DateTimeDt including time zone
        DateTimeDt dateTimeDt = new DateTimeDt("2023-04-03T15:30:00-04:00");
        String expectedXml = "20230403153000-0400";
        String actualXml = Dstu2CdaFhirUtilities.getDisplayStringForDateTimeType(dateTimeDt);
        assertEquals(expectedXml.trim(), actualXml.trim());

        // Test with DateTimeDt that has only date (no time zone)
        DateTimeDt dateOnlyDt = new DateTimeDt("2023-04-03");
        expectedXml = "20230403"; // Expected format when only date is present
        actualXml = Dstu2CdaFhirUtilities.getDisplayStringForDateTimeType(dateOnlyDt);
        assertEquals(expectedXml.trim(), actualXml.trim());

        // Test with null DateTimeDt
        expectedXml = "Unknown";
        actualXml = Dstu2CdaFhirUtilities.getDisplayStringForDateTimeType(null);
        assertEquals(expectedXml.trim(), actualXml.trim());
    }

    @Test
    public void testGetXmlForCodingDt() {
        CodingDt coding = new CodingDt();
        coding.setSystem("http://loinc.org");
        coding.setCode("15074-8");
        coding.setDisplay("Glucose [Moles/volume] in Blood");

        String expected = "<code code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></code>";
        String actual = Dstu2CdaFhirUtilities.getXmlForType(coding, "code", false);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());

        expected = "<value xsi:type=\"CD\" code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></value>";
        actual = Dstu2CdaFhirUtilities.getXmlForType(coding, "value", true);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());
    }

    @Test
    public void testGetXmlForCodeableConceptDt() {
        CodingDt coding = new CodingDt();
        coding.setSystem("http://loinc.org");
        coding.setCode("15074-8");
        coding.setDisplay("Glucose [Moles/volume] in Blood");

        CodeableConceptDt concept = new CodeableConceptDt();
        concept.addCoding(coding);

        String expected = "<value xsi:type=\"CD\" code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></value>";
        String actual = Dstu2CdaFhirUtilities.getXmlForType(concept, "value", true);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());

        expected = "<code code=\"15074-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Glucose [Moles/volume] in Blood\"></code>";
        actual = Dstu2CdaFhirUtilities.getXmlForType(concept, "code", false);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());
    }

    @Test
    public void testGetXmlForQuantityDt() {
        QuantityDt quantity = new QuantityDt();
        quantity.setValue(new BigDecimal("10"));
        quantity.setCode("mg");
        quantity.setSystem("http://snomed.info/sct");

        String expected = "<value value=\"10\"/>";
        String actual = Dstu2CdaFhirUtilities.getXmlForType(quantity, "value", false);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());
    }

    @Test
    public void testGetXmlForCodeDt() {
        CodeDt codeDt = new CodeDt("code");

        String expected = "<code code=\"code\"/>";
        String actual = Dstu2CdaFhirUtilities.getXmlForType(codeDt, "code", false);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());

        expected = "<value xsi:type=\"ST\">code</value>";
        actual = Dstu2CdaFhirUtilities.getXmlForType(codeDt, "value", true);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());
    }

    @Test
    public void testGetXmlForDateTimeDt() {
        Date date = new Date(1640995200000L);
        DateTimeDt dateTimeDt = new DateTimeDt(date);

        String expected = "<effectiveTime value=\"20220101000000+0000\"/>";
        String actual = Dstu2CdaFhirUtilities.getXmlForType(dateTimeDt, "effectiveTime", false);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());
    }

    @Test
    public void testGetXmlForPeriodDt() {
        PeriodDt period = new PeriodDt();
        period.setStart(new DateTimeDt(new Date(2323223232L)));
        period.setEnd(new DateTimeDt(new Date(2523223232L)));

        String expected = "<effectiveTime> <low value=\"19700127212023+0000\"/> <high value=\"19700130045343+0000\"/> </effectiveTime>";
        String actual = Dstu2CdaFhirUtilities.getXmlForType(period, "effectiveTime", true);
        assertEquals(StringUtils.normalizeSpace(expected).trim(), StringUtils.normalizeSpace(actual).trim());
    }

    @Test
    public void testGetXmlForStringDt() {
        StringDt stringDt = new StringDt("string");

        String expected = "<code>string</code>";
        String actual = Dstu2CdaFhirUtilities.getXmlForType(stringDt, "code", false);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());

        expected = "<value xsi:type=\"ST\">string</value>";
        actual = Dstu2CdaFhirUtilities.getXmlForType(stringDt, "value", true);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());
    }

    @Test
    public void testGetXmlForNullValue() {
        String expected = "<code nullFlavor=\"NI\"/>";
        String actual = Dstu2CdaFhirUtilities.getXmlForType(null, "code", false);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());

        expected = "<value xsi:type=\"ST\" nullFlavor=\"NI\"/>";
        actual = Dstu2CdaFhirUtilities.getXmlForType(null, "value", true);
        assertEquals(expected.trim(), StringUtils.normalizeSpace(actual).trim());
    }

    @Test
    public void testGetXmlForTimingDt() {
        TimingDt timing = new TimingDt();
        TimingDt.Repeat repeat = new TimingDt.Repeat();
        repeat.setBounds(new TimingDt());
        timing.setRepeat(repeat);

        String expected = "";
        String actual = Dstu2CdaFhirUtilities.getXmlForType(timing, "effectiveTime", true);
        assertEquals(StringUtils.normalizeSpace(expected).trim(), StringUtils.normalizeSpace(actual).trim());
    }

    @Test
    public void testGetOrganization_withOrgEntry() {

        Organization org = new Organization();
        org.setId("ResId");

        Entry entry = new Entry();
        entry.setResource(org);
        List<Entry> entries = new ArrayList<>();
        entries.add(entry);


        Encounter encounter = new Encounter();

        ResourceReferenceDt serviceProvider = new ResourceReferenceDt();
        serviceProvider.setReference("Organization/ResId");
        encounter.setServiceProvider(serviceProvider);

        //  Call the method and assert the Organization is found
        Organization result = Dstu2CdaFhirUtilities.getOrganization(entries, encounter);
        assertNotNull(result);
        assertEquals("ResId", result.getId().getIdPart());

    }



    @Test
    public void testGetOrganization_nullBundleComponent() {

        Organization org = new Organization();
        org.setId("ResId");


        Entry entry = new Entry();
        entry.setResource(org);


        List<Entry> entries = new ArrayList<>();
        entries.add(entry);

        Encounter encounter = new Encounter();
        ResourceReferenceDt serviceProvider = new ResourceReferenceDt();
        serviceProvider.setReference("Organization/ResId_1");
        encounter.setServiceProvider(serviceProvider);


        Organization result = Dstu2CdaFhirUtilities.getOrganization(entries, encounter);
        assertNull(result);
    }

    @Test
    public void testGetOrganization_NullReference() {

        Encounter encounter = new Encounter();
        encounter.setServiceProvider(null);


        List<Entry> entries = new ArrayList<>();


        Organization result = Dstu2CdaFhirUtilities.getOrganization(entries, encounter);
        assertNull(result);
    }

    @Test
    public void testGetPeriodXmlForValueElement_WithStartAndEndDates() {
        PeriodDt period = new PeriodDt();
        period.setStart(new DateTimeDt("2023-01-01T00:00:00Z"));
        period.setEnd(new DateTimeDt("2023-12-31T23:59:59Z"));

        String actualXml = Dstu2CdaFhirUtilities.getPeriodXmlForValueElement(period, "effectiveTime");

        String expectedXml = "<effectiveTime xsi:type=\"IVL_TS\"><low value=\"20230101000000+0000\"/>\n" +
                "<high value=\"20231231235959+0000\"/>\n" +
                "</effectiveTime>";

        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void testGetPeriodXmlForValueElement_WithOnlyStartDate() {
        PeriodDt period = new PeriodDt();
        period.setStart(new DateTimeDt("2023-01-01T00:00:00Z"));

        String actualXml = Dstu2CdaFhirUtilities.getPeriodXmlForValueElement(period, "effectiveTime");

        String expectedXml = "<effectiveTime xsi:type=\"IVL_TS\"><low value=\"20230101000000+0000\"/>\n" +
                "<high nullFlavor=\"NI\"/>\n" +
                "</effectiveTime>";

        assertXmlEquals(expectedXml,actualXml);
    }

    @Test
    public void testGetPeriodXmlForValueElement_WithOnlyEndDate() {
        PeriodDt period = new PeriodDt();
        period.setEnd(new DateTimeDt("2023-12-31T23:59:59Z"));

        String actualXml = Dstu2CdaFhirUtilities.getPeriodXmlForValueElement(period, "effectiveTime");

        String expectedXml = "<effectiveTime xsi:type=\"IVL_TS\"><low nullFlavor=\"NI\"/>\n" +
                "<high value=\"20231231235959+0000\"/>\n" +
                "</effectiveTime>";

        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void testGetPeriodXmlForValueElement_WithNoStartOrEndDate() {
        PeriodDt period = new PeriodDt();

        String actualXml = Dstu2CdaFhirUtilities.getPeriodXmlForValueElement(period, "effectiveTime");

        String expectedXml = "<effectiveTime xsi:type=\"IVL_TS\"><low nullFlavor=\"NI\"/> <high nullFlavor=\"NI\"/> </effectiveTime>";

        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void testGetPeriodXmlForValueElement_NullPeriod() {


        String expectedXml = "<effectiveTime xsi:type=\"IVL_TS\"><low nullFlavor=\"NI\"/>\n" +
                "<high nullFlavor=\"NI\"/>\n" +
                "</effectiveTime>";

        String actualXml = Dstu2CdaFhirUtilities.getPeriodXmlForValueElement(null, "effectiveTime");

        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void testGetEncounterClassCodeXml_Ambulatory() {
        BoundCodeDt<EncounterClassEnum> encClass = new BoundCodeDt<>(EncounterClassEnum.VALUESET_BINDER, EncounterClassEnum.AMBULATORY);
        String expectedXml = CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.CDA_AMBULATORY_ENCOUNTER_CLASS,
                CdaGeneratorConstants.ACT_CODE_SYSTEM);

        String actualXml = Dstu2CdaFhirUtilities.getEncounterClassCodeXml(encClass);

        assertEquals(expectedXml, actualXml);
    }

    @Test
    public void testGetEncounterClassCodeXml_Outpatient() {
        BoundCodeDt<EncounterClassEnum> encClass = new BoundCodeDt<>(EncounterClassEnum.VALUESET_BINDER, EncounterClassEnum.OUTPATIENT);
        // Outpatient is mapped to the same value as Ambulatory
        String expectedXml = CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.CDA_AMBULATORY_ENCOUNTER_CLASS,
                CdaGeneratorConstants.ACT_CODE_SYSTEM);

        String actualXml = Dstu2CdaFhirUtilities.getEncounterClassCodeXml(encClass);

        assertEquals(expectedXml, actualXml);
    }

    @Test
    public void testGetEncounterClassCodeXml_Inpatient() {
        BoundCodeDt<EncounterClassEnum> encClass = new BoundCodeDt<>(EncounterClassEnum.VALUESET_BINDER, EncounterClassEnum.INPATIENT);
        String expectedXml = CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.CDA_INPATIENT_ENCOUNTER_CLASS,
                CdaGeneratorConstants.ACT_CODE_SYSTEM);

        String actualXml = Dstu2CdaFhirUtilities.getEncounterClassCodeXml(encClass);

        assertEquals(expectedXml, actualXml);
    }

    @Test
    public void testGetEncounterClassCodeXml_Emergency() {
        BoundCodeDt<EncounterClassEnum> encClass = new BoundCodeDt<>(EncounterClassEnum.VALUESET_BINDER, EncounterClassEnum.EMERGENCY);
        String expectedXml = CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.CDA_EMERGENCY_ENCOUNTER_CLASS,
                CdaGeneratorConstants.ACT_CODE_SYSTEM);

        String actualXml = Dstu2CdaFhirUtilities.getEncounterClassCodeXml(encClass);

        assertEquals(expectedXml, actualXml);
    }

    @Test
    public void testGetEncounterClassCodeXml_NullEncounter() {
        BoundCodeDt<EncounterClassEnum> encClass = null;
        String expectedXml = CdaGeneratorUtils.getXmlForNullCD(
                CdaGeneratorConstants.CODE_EL_NAME, CdaGeneratorConstants.NF_NI);

        String actualXml = Dstu2CdaFhirUtilities.getEncounterClassCodeXml(encClass);

        assertEquals(expectedXml, actualXml);
    }


        @Test
        public void testGetCodingDisplayForCodeSystem_WithDisplay() {

            List<CodingDt> codings = Arrays.asList(
                    new CodingDt("http://loinc.org", "1234").setDisplay("Test Loinc")
            );
            Pair<String, Boolean> result = Dstu2CdaFhirUtilities.getCodingDisplayForCodeSystem(codings, "http://loinc.org", false);
            assertEquals("Test Loinc", result.getValue0());
            assertTrue(result.getValue1());
        }

        @Test
        public void testGetCodingDisplayForCodeSystem_WithNoDisplay() {

            List<CodingDt> codingsNoDisplay = Arrays.asList(
                    new CodingDt("http://loinc.org", "1234"), // No display
                    new CodingDt("http://snomed.info", "5678")
            );
            Pair<String, Boolean> result = Dstu2CdaFhirUtilities.getCodingDisplayForCodeSystem(codingsNoDisplay, "http://loinc.org", false);
            assertEquals("", result.getValue0());  // No display text found
            assertTrue(result.getValue1());  // Found the system
        }

        @Test
        public void testGetCodingDisplayForCodeSystem_WithCsOptionalTrue() {
            List<CodingDt> codingsNoDisplay = Arrays.asList(
                    new CodingDt("http://loinc.org", "1234"), // No display
                    new CodingDt("http://snomed.info", "5678")
            );
            Pair<String, Boolean> result = Dstu2CdaFhirUtilities.getCodingDisplayForCodeSystem(codingsNoDisplay, "http://loinc.org", true);
            assertEquals("", result.getValue0());  // No display text found, but optional
            assertTrue(result.getValue1());  // Found the system
        }

        @Test
        public void testGetCodingDisplayForCodeSystem_WithEmptyCodeSystemUrl() {
            List<CodingDt> codings = Arrays.asList(
                    new CodingDt("http://loinc.org", "1234").setDisplay("Test Loinc")
            );
            Pair<String, Boolean> result = Dstu2CdaFhirUtilities.getCodingDisplayForCodeSystem(codings, "", true);
            assertEquals("Test Loinc", result.getValue0());  // No valid system
            assertFalse(result.getValue1());
        }

        @Test
        public void testGetCodingDisplayForCodeSystem_WithNoMatchingSystem() {
            List<CodingDt> codings = Arrays.asList(
                    new CodingDt("http://loinc.org", "1234").setDisplay("Test Loinc")
            );
            Pair<String, Boolean> result = Dstu2CdaFhirUtilities.getCodingDisplayForCodeSystem(codings, "http://example.com", false);
            assertEquals("", result.getValue0());  // No display text found
            assertFalse(result.getValue1());  // Did not find the system
        }

        @Test
        public void testGetCodingDisplayForCodeSystem_WithEmptyList() {

            Pair<String, Boolean> result = Dstu2CdaFhirUtilities.getCodingDisplayForCodeSystem(Collections.emptyList(), "http://loinc.org", false);
            assertEquals("", result.getValue0());  // No codings to process
            assertFalse(result.getValue1());  // Did not find the system
        }

        @Test
        public void testGetCodingDisplayForCodeSystem_WithNullList() {

            Pair<String, Boolean> result = Dstu2CdaFhirUtilities.getCodingDisplayForCodeSystem(null, "http://loinc.org", false);
            assertEquals("", result.getValue0());  // No codings to process
            assertFalse(result.getValue1());  // Did not find the system
        }

    @Test
    public void testGetIDataTypeXml_CodingDtWithDisplay() {
        CodingDt coding = new CodingDt("http://loinc.org", "1234").setDisplay("Test Loinc");

        String result = Dstu2CdaFhirUtilities.getIDataTypeXml(coding, elName, valFlag);

        String expected = "<value xsi:type=\"CD\" code=\"1234\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Test Loinc\"></value>";

        assertXmlEquals(expected, result);
    }

    // Test case 2: CodeableConceptDt
    @Test
    public void testGetIDataTypeXml_CodeableConceptDt() {
        CodeableConceptDt concept = new CodeableConceptDt();
        CodingDt coding = new CodingDt("http://snomed.info", "5678").setDisplay("Test Snomed");
        concept.setCoding(Arrays.asList(coding));

        String result = Dstu2CdaFhirUtilities.getIDataTypeXml(concept, VAL_EL_NAME, valFlag);

        String expected = "<value xsi:type=\"CD\" code=\"5678\" codeSystem=\"\" codeSystemName=\"\" displayName=\"Test Snomed\"></value>";

        assertXmlEquals(expected, result);
    }

    // Test case 3: QuantityDt
    @Test
    public void testGetIDataTypeXml_QuantityDt() {
        QuantityDt quantity = new QuantityDt();
        quantity.setValue(10.5);
        quantity.setUnit("kg");

        String result = Dstu2CdaFhirUtilities.getIDataTypeXml(quantity, elName, valFlag);

        String expected = "<value xsi:type=\"PQ\" value=\"10.5\" unit=\"kg\"/>";

        assertXmlEquals(expected, result);
    }

    // Test case 4: DateTimeDt
    @Test
    public void testGetIDataTypeXml_DateTimeDt() {
        DateTimeDt dateTime = new DateTimeDt("2025-02-20T12:00:00Z");

        String result = Dstu2CdaFhirUtilities.getIDataTypeXml(dateTime, EFF_TIME_EL_NAME, valFlag);

        String expected = "<effectiveTime value=\"20250220120000+0000\"/>";
        assertXmlEquals(expected, result);
    }
;
    // Test case 5: PeriodDt
    @Test
    public void testGetIDataTypeXml_PeriodDt() {
        PeriodDt period = new PeriodDt();
                period.setStart(new DateTimeDt("2025-02-20T12:00:00Z"));
                period.setEnd( new DateTimeDt("2025-02-21T12:00:00Z"));

        String result = Dstu2CdaFhirUtilities.getIDataTypeXml(period, EFF_TIME_EL_NAME, valFlag);

        String expected = "<effectiveTime>\n" +
                "<low value=\"20250220120000+0000\"/>\n" +
                "<high value=\"20250221120000+0000\"/>\n" +
                "</effectiveTime>";

        assertXmlEquals(expected, result);
    }

    // Test case 6: CodeDt
    @Test
    public void testGetIDataTypeXml_CodeDt() {
        CodeDt code = new CodeDt("testCode");

        String result = Dstu2CdaFhirUtilities.getIDataTypeXml(code, elName, valFlag);

        String expected = "<value xsi:type=\"CD\" nullFlavor=\"NI\"/>";

        assertXmlEquals(expected, result);
    }

    // Test case 7: StringDt
    @Test
    public void testGetIDataTypeXml_StringDt() {
        StringDt string = new StringDt("Test String");

        String result = Dstu2CdaFhirUtilities.getIDataTypeXml(string, elName, valFlag);

        String expected = "<value xsi:type=\"ST\">Test String</value>";

        assertXmlEquals(expected, result);
    }

    // Test case 8: Null Datatype
    @Test
    public void testGetIDataTypeXml_Null() {
        String result = Dstu2CdaFhirUtilities.getIDataTypeXml(null, elName, valFlag);

        assertXmlEquals("<value xsi:type=\"ST\" nullFlavor=\"NI\"/>", result);  // Expecting an empty string or empty XML tag
    }

    // Test case 9: CodingDt without Display
    @Test
    public void testGetIDataTypeXml_CodingDtWithoutDisplay() {
        CodingDt coding = new CodingDt("http://loinc.org", "1234"); // No display

        String result = Dstu2CdaFhirUtilities.getIDataTypeXml(coding, elName, valFlag);

        String expected = "<value xsi:type=\"CD\" code=\"1234\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\"></value>";

        assertXmlEquals(expected, result);
    }

    // Test case 10: ValFlag False
    @Test
    public void testGetIDataTypeXml_ValFlagFalse() {
        valFlag = false;
        CodingDt coding = new CodingDt("http://loinc.org", "1234").setDisplay("Test Loinc");

        String result = Dstu2CdaFhirUtilities.getIDataTypeXml(coding, "code", valFlag);

        String expected ="<code code=\"1234\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Test Loinc\"></code>";

        assertXmlEquals(expected, result);
    }



    @Test
    public void testGetCodingForValidCodeSystems() {
        // Create a CodeableConceptDt with valid LOINC code
        CodeableConceptDt concept = new CodeableConceptDt();
        CodingDt coding1 = new CodingDt(FHIR_LOINC_URL, "1234").setDisplay("Test Loinc Code");
        CodingDt coding2 = new CodingDt("http://example.com", "9999").setDisplay("Invalid Code");

        concept.setCoding(Arrays.asList(coding1, coding2));

        List<CodeableConceptDt> cds = Collections.singletonList(concept);

        List<CodingDt> result = Dstu2CdaFhirUtilities.getCodingForValidCodeSystems(cds);

        assertEquals(1, result.size());
        assertEquals("1234", result.get(0).getCode());
        assertEquals(FHIR_LOINC_URL, result.get(0).getSystem());
    }

    // Test case with multiple CodeableConceptDt objects and LOINC codes
    @Test
    public void testGetCodingForMultipleValidCodeSystems() {
        CodeableConceptDt concept1 = new CodeableConceptDt();
        CodingDt coding1 = new CodingDt(FHIR_LOINC_URL, "1234").setDisplay("Test Loinc Code");
        CodingDt coding2 = new CodingDt("http://example.com", "9999").setDisplay("Invalid Code");
        concept1.setCoding(Arrays.asList(coding1, coding2));

        CodeableConceptDt concept2 = new CodeableConceptDt();
        CodingDt coding3 = new CodingDt(FHIR_LOINC_URL, "5678").setDisplay("Another Loinc Code");
        CodingDt coding4 = new CodingDt("http://example.com", "8888").setDisplay("Another Invalid Code");
        concept2.setCoding(Arrays.asList(coding3, coding4));

        List<CodeableConceptDt> cds = Arrays.asList(concept1, concept2);

        List<CodingDt> result = Dstu2CdaFhirUtilities.getCodingForValidCodeSystems(cds);

        assertEquals(2, result.size());
        assertEquals("1234", result.get(0).getCode());
        assertEquals(FHIR_LOINC_URL, result.get(0).getSystem());
        assertEquals("5678", result.get(1).getCode());
        assertEquals(FHIR_LOINC_URL, result.get(1).getSystem());
    }

    // Test case with an empty list
    @Test
    public void testGetCodingForValidCodeSystems_EmptyList() {
        List<CodeableConceptDt> cds = Collections.emptyList();


        List<CodingDt> result = Dstu2CdaFhirUtilities.getCodingForValidCodeSystems(cds);

        assertTrue(result.isEmpty());
    }


    @Test
    public void testGetCodingForValidCodeSystems_NullList() {

        List<CodingDt> result = Dstu2CdaFhirUtilities.getCodingForValidCodeSystems(null);
        assertTrue(result.isEmpty());
    }
    @Test
    public void getCodeableConceptDisplayForCodeSystem() {
        String codeSystemUrl = "http://loinc.org";
        Boolean csOptional = false;
        List<CodingDt> codings = new ArrayList<>();
        codings.add(
                new CodingDt().setSystem("http://loinc.org").setCode("1234").setDisplay("Test Loinc"));
        codings.add(
                new CodingDt().setSystem("http://snomed.info").setCode("5678").setDisplay("Test Snomed"));
        codings.add(new CodingDt().setSystem("http://fhir.org").setCode("abcd").setDisplay("Test Fhir"));
        List<CodeableConceptDt> cds = new ArrayList<>();
        CodeableConceptDt codeableConceptDt = new CodeableConceptDt();
        codeableConceptDt.setCoding(codings);
        cds.add(codeableConceptDt);
        Pair<String, Boolean> result =
                Dstu2CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(cds, codeSystemUrl, csOptional);
        assertEquals("Test Loinc", result.getValue0());
    }

    @Test
    public void getCodeableConceptDisplayForCodeSystem_test() {
        String codeSystemUrl = "http://loinc.org";
        Boolean csOptional = false;
        List<CodingDt> codings = new ArrayList<>();
        codings.add(
                new CodingDt().setSystem("http://loinc.org").setCode("1234").setDisplay("Test Loinc"));
        codings.add(
                new CodingDt().setSystem("http://snomed.info").setCode("5678").setDisplay("Test Snomed"));
        codings.add(new CodingDt().setSystem("http://fhir.org").setCode("abcd").setDisplay("Test Fhir"));
        List<CodeableConceptDt> cds = new ArrayList<>();
        CodeableConceptDt codeableConceptDt = new CodeableConceptDt();
        codeableConceptDt.setCoding(codings);
        codeableConceptDt.setText("MockText");
        cds.add(codeableConceptDt);
        Pair<String, Boolean> result =
                Dstu2CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(cds, codeSystemUrl, csOptional);
        assertTrue(result.getValue1());
    }



    @Test
    public void testGetTelecomXml_ValidPhoneNumber() {
        ContactPointDt tel = new ContactPointDt();
        tel.setSystem(ContactPointSystemEnum.PHONE);
        tel.setValue("123-456-7890");
        tel.setUse(ContactPointUseEnum.HOME);

        List<ContactPointDt> tels = Collections.singletonList(tel);

        String result = Dstu2CdaFhirUtilities.getTelecomXml(tels);

        String expectedXml = "<telecom value=\"tel:(123)456-7890\" use=\"HP\"/>";

        assertXmlEquals(expectedXml, result);
    }

    @Test
    public void testGetTelecomXml_EmptyList() {
        List<ContactPointDt> tels = Collections.emptyList();

        String result = Dstu2CdaFhirUtilities.getTelecomXml(tels);

        String expectedXml = "<telecom nullFlavor=\"NI\"/>";

        assertXmlEquals(expectedXml, result);
    }

    @Test
    public void testGetTelecomXml_NullList() {
        List<ContactPointDt> tels = null;

        String result = Dstu2CdaFhirUtilities.getTelecomXml(tels);

        String expectedXml = "<telecom nullFlavor=\"NI\"/>";

        assertXmlEquals(expectedXml, result);
    }

    @Test
    public void testGetTelecomXml_InvalidSystem() {
        ContactPointDt tel = new ContactPointDt();
        tel.setSystem(ContactPointSystemEnum.EMAIL);
        tel.setValue("user@example.com");

        List<ContactPointDt> tels = Collections.singletonList(tel);

        String result = Dstu2CdaFhirUtilities.getTelecomXml(tels);


        assertTrue(result.isEmpty());
    }

    @Test
    public void testGetTelecomXml_MultipleEntries() {
        ContactPointDt tel1 = new ContactPointDt();
        tel1.setSystem(ContactPointSystemEnum.PHONE);
        tel1.setValue("123-456-7890");

        ContactPointDt tel2 = new ContactPointDt();
        tel2.setSystem(ContactPointSystemEnum.PHONE);
        tel2.setValue("987-654-3210");

        List<ContactPointDt> tels = Arrays.asList(tel1, tel2);

        String result = Dstu2CdaFhirUtilities.getTelecomXml(tels);

        String expectedXml = "<telecom value=\"tel:(123)456-7890\" use=\"HP\"/>";

        assertXmlEquals(expectedXml, result);
    }

    @Test
    public void getGuardianContact_test_Text() {
        contactList = new ArrayList<>();
        Patient.Contact contact = new Patient.Contact();
        List<CodeableConceptDt> relationships = new ArrayList<>();
        CodeableConceptDt guardianRelation = new CodeableConceptDt();
        guardianRelation.setText(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME);
        relationships.add(guardianRelation);
        contact.setRelationship(relationships);
        contactList.add(contact);

        // Call the method and assert the contact is returned
        Patient.Contact actual = Dstu2CdaFhirUtilities.getGuardianContact(contactList);
        assertEquals(contact, actual);
    }

    @Test
    public void getGuardianContact_test_Coding() {
        contactList = new ArrayList<>();
        Patient.Contact contact = new Patient.Contact();
        List<CodeableConceptDt> relationships = new ArrayList<>();
        CodeableConceptDt guardianRelation = new CodeableConceptDt();

        CodingDt coding = new CodingDt();
        coding.setSystem(CdaGeneratorConstants.FHIR_CONTACT_RELATIONSHIP_CODESYSTEM);
        coding.setCode(CdaGeneratorConstants.GUARDIAN_VALUE);
        guardianRelation.setCoding(Collections.singletonList(coding));

        relationships.add(guardianRelation);
        contact.setRelationship(relationships);
        contactList.add(contact);

        Patient.Contact actual = Dstu2CdaFhirUtilities.getGuardianContact(contactList);
        assertEquals(contact, actual);
    }

    @Test
    public void getGuardianContact_test_NoGuardian() {
        contactList = new ArrayList<>();
        Patient.Contact contact = new Patient.Contact();
        List<CodeableConceptDt> relationships = new ArrayList<>();
        CodeableConceptDt nonGuardianRelation = new CodeableConceptDt();
        nonGuardianRelation.setText("Non-guardian");
        relationships.add(nonGuardianRelation);
        contact.setRelationship(relationships);
        contactList.add(contact);

        Patient.Contact actual = Dstu2CdaFhirUtilities.getGuardianContact(contactList);
        assertNull(actual);
    }

    @Test
    public void getGuardianContact_test_EmptyList() {
        contactList = new ArrayList<>();
        Patient.Contact actual = Dstu2CdaFhirUtilities.getGuardianContact(contactList);
        assertNull(actual);
    }

    @Test
    public void getGuardianContact_test_NullList() {

        contactList = new ArrayList<>();
        Patient.Contact actual = Dstu2CdaFhirUtilities.getGuardianContact(null);
        assertNull(actual);
    }

    @Test
    public void getGuardianContact_test_MultipleContacts() {
        contactList = new ArrayList<>();
        // Create two contacts, one with a guardian and one without
        Patient.Contact guardianContact = new Patient.Contact();
        List<CodeableConceptDt> relationships = new ArrayList<>();
        CodeableConceptDt guardianRelation = new CodeableConceptDt();
        guardianRelation.setText(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME);
        relationships.add(guardianRelation);
        guardianContact.setRelationship(relationships);

        Patient.Contact nonGuardianContact = new Patient.Contact();
        List<CodeableConceptDt> nonGuardianRelationships = new ArrayList<>();
        CodeableConceptDt nonGuardianRelation = new CodeableConceptDt();
        nonGuardianRelation.setText("Non-guardian");
        nonGuardianRelationships.add(nonGuardianRelation);
        nonGuardianContact.setRelationship(nonGuardianRelationships);

        contactList.add(guardianContact);
        contactList.add(nonGuardianContact);

        // Call the method and assert the guardian contact is returned
        Patient.Contact actual = Dstu2CdaFhirUtilities.getGuardianContact(contactList);
        assertEquals(guardianContact, actual);
    }
    @Test
    public void test_isCodingPresentForCodeSystem_found() {
        List<CodingDt> codings = new ArrayList<>();
        CodingDt coding1 = new CodingDt();
        coding1.setSystem("http://hl7.org/fhir/sid/us-ssn");
        coding1.setCode("12345");

        CodingDt coding2 = new CodingDt();
        coding2.setSystem("http://loinc.org");
        coding2.setCode("23456");

        codings.add(coding1);
        codings.add(coding2);

        String codeSystemUrl = "http://hl7.org/fhir/sid/us-ssn";
        Boolean result = Dstu2CdaFhirUtilities.isCodingPresentForCodeSystem(codings, codeSystemUrl);

        assertTrue(result);
    }

    @Test
    public void test_isCodingPresentForCodeSystem_notFound() {
        List<CodingDt> codings = new ArrayList<>();
        CodingDt coding1 = new CodingDt();
        coding1.setSystem("http://loinc.org");
        coding1.setCode("23456");

        codings.add(coding1);

        String codeSystemUrl = "http://hl7.org/fhir/sid/us-ssn";
        Boolean result = Dstu2CdaFhirUtilities.isCodingPresentForCodeSystem(codings, codeSystemUrl);

        assertFalse(result);
    }

    @Test
    public void test_isCodingPresentForCodeSystem_emptyList() {
        List<CodingDt> codings = new ArrayList<>();
        String codeSystemUrl = "http://hl7.org/fhir/sid/us-ssn";
        Boolean result = Dstu2CdaFhirUtilities.isCodingPresentForCodeSystem(codings, codeSystemUrl);

        assertFalse(result);
    }


    @Test
    public void getStringForCoding_Test_Display() {
        CodingDt codingDt = new CodingDt();
        codingDt.setDisplay("Mock Display");


        String actual = Dstu2CdaFhirUtilities.getStringForCoding(codingDt);
        String Expected = "Mock Display";

        assertNotNull(codingDt);
        assertEquals(Expected, actual);
    }


    @Test
    public void getStringForCoding_Test_NoDisplay() {
        CodingDt codingDt = new CodingDt();
        codingDt.setCode("24566");
        codingDt.setSystem("Mock System");

        String actual = Dstu2CdaFhirUtilities.getStringForCoding(codingDt);
        String Expected = "Mock System|24566";
        assertEquals(Expected, actual);
    }


    @Test
    public void getStringForQuantity_Test() {
        QuantityDt quantity = new QuantityDt();
        quantity.setValue(9223372036854775808.0);
        quantity.setSystem("MockSystem");
        quantity.setUnit("MockUnit");

        String Actual = Dstu2CdaFhirUtilities.getStringForQuantity(quantity);
        String Expected = "9223372036854776000|MockSystem|MockUnit";
        assertEquals(Expected, Actual);
    }

    @Test
    public void getStringForQuantity_Test_Null() {
        QuantityDt quantity = new QuantityDt();

        String Actual = Dstu2CdaFhirUtilities.getStringForQuantity(quantity);

        assertEquals("Unknown", Actual);
    }


    @Test
    public void getStringForType_Test() {
        String actual = Dstu2CdaFhirUtilities.getStringForType(null);
        assertEquals("Unknown", actual);


    }

    @Test
  public   void getStringForType_Test_Display() {

        CodingDt codingDt = new CodingDt();
        codingDt.setDisplay("Mock Display");

        String actual = Dstu2CdaFhirUtilities.getStringForType(
                codingDt);
        assertEquals("Mock Display", actual);
    }

    @Test
   public void getStringForType_Test_CodeableConcept() {

        CodeableConceptDt codeableConceptDt = new CodeableConceptDt();
        codeableConceptDt.setText("MockText");
        String actual = Dstu2CdaFhirUtilities.getStringForType(
                codeableConceptDt);

        assertEquals("MockText", actual);
    }

    @Test
  public   void getStringForType_Test_Text_null() {

        CodeableConceptDt codeableConceptDt = new CodeableConceptDt();
        codeableConceptDt.setText("");
        String actual = Dstu2CdaFhirUtilities.getStringForType(
                codeableConceptDt);

        assertEquals("", actual);
    }


    @Test
  public   void getStringForType_Test_Quantity() {

        QuantityDt quantity = new QuantityDt();
        quantity.setValue(104.5);
        quantity.setUnit("mmHg");
        quantity.setCode("24455");
        String actual = Dstu2CdaFhirUtilities.getStringForType(quantity);
        assertEquals("104.5|null|mmHg", actual);
    }

    @Test
  public   void getStringForType_Test_CodeDt() {
        CodeDt codeDt = new CodeDt();
        codeDt.setValue("Mock Value");
        String actual = Dstu2CdaFhirUtilities.getStringForType(codeDt);
        assertEquals("Mock Value", actual);
    }

    @Test
   public void getStringForType_Test_StringDt() {
        StringDt stringDt = new StringDt();
        stringDt.setValue("36533");
        String actual = Dstu2CdaFhirUtilities.getStringForType(stringDt);
        assertEquals("36533", actual);
    }

    @Test
    public void getStringForType_Test_DateTimeDt() {

        DateTimeDt dateTimeDt = new DateTimeDt("2025-02-19T22:35:26");


        String actual = Dstu2CdaFhirUtilities.getStringForType(dateTimeDt);
        assertEquals("2025-02-19T22:35:26", actual);
    }

    @Test
    public void test_getPractitioner_found() {
        // Prepare Encounter with Participant
        Encounter encounter = new Encounter();
        List<Encounter.Participant> participants = new ArrayList<>();

        Encounter.Participant participant = new Encounter.Participant();
        participant.setIndividual(new ResourceReferenceDt("Practitioner/123"));

        BoundCodeableConceptDt<ParticipantTypeEnum> type= new BoundCodeableConceptDt<>();
        type.addCoding(new CodingDt().setCode("PPRF")); // Practitioner type

        participant.setType(new ArrayList<>());
        participant.getType().add(type);
        participants.add(participant);

        encounter.setParticipant(participants);

        // Prepare entries with Practitioner
        List<Entry> entries = new ArrayList<>();
        Entry practitionerEntry = new Entry();
        Practitioner practitioner = new Practitioner();
        practitioner.setId("123");
        practitionerEntry.setResource(practitioner);
        entries.add(practitionerEntry);

        // Call the method
        Practitioner actualPractitioner = Dstu2CdaFhirUtilities.getPractitioner(entries, encounter);

        // Assert
        assertNotNull(actualPractitioner);
        assertEquals("123", actualPractitioner.getId().toString());
    }

    @Test
    public void test_getPractitioner_notFound() {
        // Prepare Encounter with Participant
        Encounter encounter = new Encounter();
        List<Encounter.Participant> participants = new ArrayList<>();

        Encounter.Participant participant = new Encounter.Participant();
        participant.setIndividual(new ResourceReferenceDt("Practitioner/456"));

        BoundCodeableConceptDt<ParticipantTypeEnum> type = new BoundCodeableConceptDt<>();
        type.addCoding(new CodingDt().setCode("PPRF")); // Practitioner type

        participant.setType(new ArrayList<>());
        participant.getType().add(type);
        participants.add(participant);

        encounter.setParticipant(participants);


        List<Entry> entries = new ArrayList<>();

        Practitioner actualPractitioner = Dstu2CdaFhirUtilities.getPractitioner(entries, encounter);

        assertNull(actualPractitioner);
    }
         @Test
        public void test_getPractitioner_noParticipants() {
        Encounter encounter = new Encounter();
        encounter.setParticipant(new ArrayList<>());

        List<Entry> entries = new ArrayList<>();
        Entry practitionerEntry = new Entry();
        Practitioner practitioner = new Practitioner();
        practitioner.setId("123");
        practitionerEntry.setResource(practitioner);
        entries.add(practitionerEntry);


        Practitioner actualPractitioner = Dstu2CdaFhirUtilities.getPractitioner(entries, encounter);

        assertNull(actualPractitioner);
    }

    @Test
    public void testGetDateForDateDt() {
        DateDt dateDt = new DateDt("2025-02-20");
        Date expectedDate = dateDt.getValue();

        Date actualDate = Dstu2CdaFhirUtilities.getDateForDataType(dateDt);

        assertEquals(expectedDate, actualDate);
    }

    @Test
    public void testGetDateForDateTimeDt() {
        DateTimeDt dateTimeDt = new DateTimeDt("2025-02-20T10:00:00");
        Date expectedDateTime = dateTimeDt.getValue();

        Date actualDate = Dstu2CdaFhirUtilities.getDateForDataType(dateTimeDt);

        assertEquals(expectedDateTime, actualDate);
    }

    @Test
    public void testGetDateForPeriodDtStart() {
        PeriodDt periodDt = new PeriodDt();
        DateTimeDt dateDt = new DateTimeDt("2025-02-20T12:00:00");
        periodDt.setStart(dateDt);

        Date expectedDate = periodDt.getStart();

        Date actualDate = Dstu2CdaFhirUtilities.getDateForDataType(periodDt);

        assertEquals(expectedDate, actualDate);
    }

    @Test
    public void testGetDateForPeriodDtEnd() {
        PeriodDt periodDt = new PeriodDt();

        DateTimeDt dateDt = new DateTimeDt("2025-02-20T12:00:00");
        periodDt.setEnd(dateDt);
        Date expectedDate = periodDt.getEnd();

        Date actualDate = Dstu2CdaFhirUtilities.getDateForDataType(periodDt);

        assertEquals(expectedDate, actualDate);
    }

    @Test
    public void testGetDateForNullDateType() {
        Date actualDate = Dstu2CdaFhirUtilities.getDateForDataType(null);

        assertNull(actualDate);
    }

    @Test
    public void testGetCodeableConceptXml_withValidCodes() {
        CodingDt coding = new CodingDt();
        coding.setSystem(CODE_SYSTEM_URL);
        coding.setCode(CODE);
        CodeableConceptDt codeableConcept = new CodeableConceptDt();
        codeableConcept.addCoding(coding);
        List<CodeableConceptDt> cds = Arrays.asList(codeableConcept);

        String excepted="<value xsi:type=\"CD\" code=\"12345\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\"></value>\n";
        String result = Dstu2CdaFhirUtilities.getCodeableConceptXml(cds, CD_NAME, true);

        assertNotNull(result);
        assertXmlEquals(excepted,result);
    }

    @Test
    public void testGetCodingXml_withValidCodingList() {
        CodingDt coding = new CodingDt();
        coding.setSystem(CODE_SYSTEM_URL);
        coding.setCode(CODE);
        coding.setDisplay(DISPLAY);
        List<CodingDt> codes = Arrays.asList(coding);

        String excepted="<code code=\"12345\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Test Code Display\"></code>";
        String result = Dstu2CdaFhirUtilities.getCodingXml(codes, "code");

        assertNotNull(result);
        assertXmlEquals(excepted,result);
    }

    @Test
    public void testGetCodingXmlForValue_withValidCodingList() {
        CodingDt coding = new CodingDt();
        coding.setSystem(CODE_SYSTEM_URL);
        coding.setCode(CODE);
        coding.setDisplay(DISPLAY);
        List<CodingDt> codes = Arrays.asList(coding);

        String excepted="<value xsi:type=\"CD\" code=\"12345\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Test Code Display\"></value>";
        String result = Dstu2CdaFhirUtilities.getCodingXmlForValue(codes, CD_NAME);

        assertNotNull(result);
        assertXmlEquals(excepted,result);

    }

    @Test
    public void testGetCodeableConceptXml_withEmptyCodes() {
        List<CodeableConceptDt> cds = new ArrayList<>();

        String result = Dstu2CdaFhirUtilities.getCodeableConceptXml(cds, CD_NAME, false);

        assertNotNull(result);
        assertTrue(result.contains(CdaGeneratorUtils.getXmlForNullCD(CD_NAME, CdaGeneratorConstants.NF_NI)));
    }
    @Test
    public void testGetActualDate_withDateTimeDt() {
        DateTimeDt dateTimeDt = new DateTimeDt();
        Date date = new Date();
        dateTimeDt.setValue(date);

        Pair<Date, TimeZone> result = Dstu2CdaFhirUtilities.getActualDate(dateTimeDt);

        assertNotNull(result);

    }

    @Test
    public void testGetActualDate_withPeriodDt_startDate() {
        PeriodDt periodDt = new PeriodDt();

        periodDt.setStart(new DateTimeDt("2025-01-01T12:00:00"));

        Pair<Date, TimeZone> result = Dstu2CdaFhirUtilities.getActualDate(periodDt);

        assertNotNull(result);

    }

    @Test
    public void testGetActualDate_withPeriodDt_endDate() {
        PeriodDt periodDt = new PeriodDt();
        Date date = new Date();
        periodDt.setEnd(new DateTimeDt("2025-01-01T12:00:00"));

        Pair<Date, TimeZone> result = Dstu2CdaFhirUtilities.getActualDate(periodDt);

        assertNotNull(result);
        assertNull(result.getValue0());
        assertNull(result.getValue1());
    }

    @Test
    public void testGetActualDate_withInstantDt() {
        InstantDt instantDt = new InstantDt();
        instantDt.setValue(   new DateTimeDt("2025-01-01T12:00:00").getValue());

        Pair<Date, TimeZone> result = Dstu2CdaFhirUtilities.getActualDate(instantDt);

        assertNotNull(result);
       assertEquals("Wed Jan 01 12:00:00 UTC 2025",result.getValue0().toString());
        assertNotNull(result.getValue1());
    }

    @Test
    public void testGetActualDate_withTimingDt() {
        TimingDt timingDt = new TimingDt();
        TimingDt.Repeat repeat = new TimingDt.Repeat();
        PeriodDt periodDt = new PeriodDt();
        periodDt.setStart(new DateTimeDt("2025-01-12T12:00:00"));
        repeat.setBounds(periodDt);
        timingDt.setRepeat(repeat);

        Pair<Date, TimeZone> result = Dstu2CdaFhirUtilities.getActualDate(timingDt);

        assertNotNull(result);
        assertXmlEquals("Sun Jan 12 12:00:00 UTC 2025",result.getValue0().toString());
        assertNull(result.getValue1());
    }

    @Test
    public void testGetActualDate_withTimingDt_noRepeatBounds() {
        TimingDt timingDt = new TimingDt();

        Pair<Date, TimeZone> result = Dstu2CdaFhirUtilities.getActualDate(timingDt);

        assertNotNull(result);
        assertNull(result.getValue0());
        assertNull(result.getValue1());
    }


    @Test
    public void testGetDateTypeXml_withValidDateTime() {
        DateDt dateDt = new DateDt(2025,01,01);

        String result = Dstu2CdaFhirUtilities.getDateTypeXml(dateDt, "effectiveTime");

        String expected = "<effectiveTime value=\"20250201\"/>";
        assertXmlEquals(expected,result);
    }

    @Test
    public void testGetDateTypeXml_withNullDateTime() {
        DateDt dateDt= new DateDt(); // This will be null by default

        String result = Dstu2CdaFhirUtilities.getDateTypeXml(dateDt, "effectiveTime");

        String expected = "<effectiveTime nullFlavor=\"NI\"/>";
        assertTrue(result.contains(expected));
    }


}
