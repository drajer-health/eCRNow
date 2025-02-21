package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.api.IDatatype;
import ca.uhn.fhir.model.dstu2.composite.QuantityDt;
import ca.uhn.fhir.model.dstu2.composite.SimpleQuantityDt;
import ca.uhn.fhir.model.dstu2.resource.BaseResource;
import ca.uhn.fhir.model.dstu2.resource.Medication;
import ca.uhn.fhir.model.dstu2.resource.MedicationAdministration;
import ca.uhn.fhir.model.dstu2.resource.MedicationStatement;
import ca.uhn.fhir.model.primitive.DateTimeDt;
import ca.uhn.fhir.model.primitive.StringDt;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestUtils;
import com.helger.commons.text.display.IDisplayTextProvider;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class Dstu2CdaMedicationGeneratorTest extends BaseGenerator {
    private static String MEDICATION_SECTION = "CdaDstuTestData/Cda/Medication/MedicationSection.xml";
    private static String EMPTY_MEDICATION_SECTION = "CdaDstuTestData/Cda/Medication/EmptyMedicationSection.xml";

    @Test
    public void generateMedicationSectionTest() {
        String expectedXml = "<component>\n" +
                "<section>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.2.38\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.2.38\" extension=\"2014-06-09\"/>\n" +
                "<code code=\"29549-3\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Medications Administered\"/>\n" +
                "<title>Medications Administered</title>\n" +
                "<text>\n" +
                "<table border=\"1\" width=\"100%\">\n" +
                "<thead>\n" +
                "<tr>\n" +
                "<th>Medication Name</th>\n" +
                "<th>Medication Start Date</th>\n" +
                "</tr>\n" +
                "</thead>\n" +
                "<tbody>\n" +
                "<tr>\n" +
                "<td>\n" +
                "<content ID=\"medication1\"></content>\n" +
                "</td>\n" +
                "<td>\n" +
                "<content ID=\"medicationDate1\">null</content>\n" +
                "</td>\n" +
                "</tr>\n" +
                "<tr>\n" +
                "<td>\n" +
                "<content ID=\"medication2\">Unknown</content>\n" +
                "</td>\n" +
                "<td>\n" +
                "<content ID=\"medicationDate2\">null</content>\n" +
                "</td>\n" +
                "</tr>\n" +
                "</tbody>\n" +
                "</table>\n" +
                "</text>\n" +
                "<entry typeCode=\"DRIV\">\n" +
                "<substanceAdministration classCode=\"SBADM\" moodCode=\"EVN\">\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.16\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.16\" extension=\"2014-06-09\"/>\n" +
                "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"null\"/>\n" +
                "<statusCode code=\"completed\"/>\n" +
                "<effectiveTime xsi:type=\"IVL_TS\"><low nullFlavor=\"NI\"/>\n" +
                "<high nullFlavor=\"NI\"/>\n" +
                "</effectiveTime>\n" +
                "<doseQuantity value=\"2.5\"/>\n" +
                "<consumable>\n" +
                "<manufacturedProduct classCode=\"MANU\">\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.23\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.23\" extension=\"2014-06-09\"/>\n" +
                "<id root=\"3a43e5c7-8c24-46e7-8f82-50f7484b1f89\"/>\n" +
                "<manufacturedMaterial>\n" +
                "<code nullFlavor=\"NI\"/>\n" +
                "</manufacturedMaterial>\n" +
                "</manufacturedProduct>\n" +
                "</consumable>\n" +
                "</substanceAdministration>\n" +
                "</entry>\n" +
                "<entry typeCode=\"DRIV\">\n" +
                "<substanceAdministration classCode=\"SBADM\" moodCode=\"EVN\">\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.16\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.16\" extension=\"2014-06-09\"/>\n" +
                "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"null\"/>\n" +
                "<statusCode code=\"completed\"/>\n" +
                "<effectiveTime xsi:type=\"IVL_TS\"><low nullFlavor=\"NI\"/>\n" +
                "<high nullFlavor=\"NI\"/>\n" +
                "</effectiveTime>\n" +
                "<value xsi:type=\"PQ\" nullFlavor=\"NI\"/>\n" +
                "<consumable>\n" +
                "<manufacturedProduct classCode=\"MANU\">\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.23\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.23\" extension=\"2014-06-09\"/>\n" +
                "<id root=\"21bd644f-d94e-46f7-ab0f-2246c264cf4b\"/>\n" +
                "<manufacturedMaterial>\n" +
                "<code nullFlavor=\"NI\"/>\n" +
                "</manufacturedMaterial>\n" +
                "</manufacturedProduct>\n" +
                "</consumable>\n" +
                "</substanceAdministration>\n" +
                "</entry>\n" +
                "</section>\n" +
                "</component>";
        IDatatype medication = new StringDt("Asprin");;
        SimpleQuantityDt dose = new SimpleQuantityDt();
        dose.setCode("Dosage");
        dose.setValue(2.5);
        MedicationAdministration.Dosage dosage = new MedicationAdministration.Dosage();
        dosage.setQuantity(dose);
        MedicationAdministration medicationAdministration = new MedicationAdministration();
        medicationAdministration.setDosage(dosage);
        medicationAdministration.setMedication(medication);
        List<MedicationAdministration> medicationAdministrationList = new ArrayList<>();
        medicationAdministrationList.add(medicationAdministration);
        MedicationStatement medicationStatement = new MedicationStatement();
        List<MedicationStatement> medicationStatementList = new ArrayList<>();
        medicationStatementList.add(medicationStatement);
        String exceptedXml = TestUtils.getFileContentAsString(MEDICATION_SECTION);
        dstu2FhirDataForPatient.setMedicationAdministrations(medicationAdministrationList);
        dstu2FhirDataForPatient.setMedications(medicationStatementList);
        String actualXml = Dstu2CdaMedicationGenerator.generateMedicationSection(dstu2FhirDataForPatient, launchDetails);
        //System.out.println(actualXml);
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void getEntryForMedicationXMLTest(){
        String expectedXml = "<entry typeCode=\"DRIV\">\n" +
                "<substanceAdministration classCode=\"SBADM\" moodCode=\"Happy\">\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.16\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.16\" extension=\"2014-06-09\"/>\n" +
                "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"`123\"/>\n" +
                "<statusCode code=\"Active\"/>\n" +
                "<effectiveTime xsi:type=\"IVL_TS\"><low value=\"20250219122806+0000\"/>\n" +
                "<high nullFlavor=\"NI\"/>\n" +
                "</effectiveTime>\n" +
                "<doseQuantity value=\"1234\"/>\n" +
                "<consumable>\n" +
                "<manufacturedProduct classCode=\"MANU\">\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.23\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.23\" extension=\"2014-06-09\"/>\n" +
                "<id root=\"f57606ca-54c2-4994-9326-8f1ac2f53ba1\"/>\n" +
                "<manufacturedMaterial>\n" +
                "<code nullFlavor=\"NI\"/>\n" +
                "</manufacturedMaterial>\n" +
                "</manufacturedProduct>\n" +
                "</consumable>\n" +
                "</substanceAdministration>\n" +
                "</entry>";
        String id = "`123";
        IDatatype medication = new StringDt("Asprin");
        IDatatype effectiveTime = new StringDt("10:20");
        String medStatus = "Active";
        MedicationAdministration.Dosage dosage = new MedicationAdministration.Dosage();
        dosage.setText("Oral Dosage");
        QuantityDt dose = new QuantityDt();
        SimpleQuantityDt simpleQuantityDt = new SimpleQuantityDt();
        simpleQuantityDt.setCode("ABCMED");
        simpleQuantityDt.setValue(1234L);
        dosage.setQuantity(simpleQuantityDt);
        dose.setCode("BisIndie");
        DateTimeDt startDate = new DateTimeDt(new Date());
        String moodCode = "Happy";
        BaseResource res = new Medication();
        res.setId("1234");
        String actualXml = Dstu2CdaMedicationGenerator.getEntryForMedication(id, medication, effectiveTime, medStatus,dosage,
                launchDetails, dose, startDate, moodCode, res);
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void getXmlForMedicationTypeForCodeSystemXMLTest(){
        String expectedXml = "<value xsi:type=\"CD\" nullFlavor=\"NI\"/>";
        IDatatype medication = new StringDt("Asprin");
        MedicationAdministration.Dosage dosage = new MedicationAdministration.Dosage();
        dosage.setText("Oral Dosage");
        QuantityDt dose = new QuantityDt();
        SimpleQuantityDt simpleQuantityDt = new SimpleQuantityDt();
        simpleQuantityDt.setCode("ABCMED");
        simpleQuantityDt.setValue(1234L);
        dosage.setQuantity(simpleQuantityDt);
        dose.setCode("BisIndie");
        BaseResource res = new Medication();
        res.setId("1234");
        String actualXml = Dstu2CdaMedicationGenerator.getXmlForMedicationTypeForCodeSystem(medication, "foodAllergy", true, "https://abc.org.codeSystem", true, res);
        assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void generateEmptyMedicationSectionTest() {
        String exceptedXml = TestUtils.getFileContentAsString(EMPTY_MEDICATION_SECTION);
        String actualXml = Dstu2CdaMedicationGenerator.generateEmptyMedications();
        assertXmlEquals(exceptedXml, actualXml);
    }
}
