package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.resource.Immunization;
import ca.uhn.fhir.model.primitive.CodeDt;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.test.util.TestUtils;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class Dstu2CdaImmunizationGeneratorTest extends BaseGenerator {

    private static String IMMUNIZTION_RES_FILE = "CdaDstuTestData/Immunization/Immunization.json";
    private static String IMMUNIZTION_SECTION = "CdaDstuTestData/Cda/Immunization/ImmunizationSection.xml";
    private static String EMPTY_IMMUNIZTION_SECTION = "CdaDstuTestData/Cda/Immunization/EmptyImmunizationSection.xml";

    @Test
    public void generateImmunizationSectionTest() {
        Immunization immunization = loadResourceDataFromFile(Immunization.class, IMMUNIZTION_RES_FILE);
        CodeDt codeDt = new CodeDt();
        codeDt.setValue("completed");
        immunization.setStatus(codeDt);
        String expectedXml = "<component>\n" +
                "<section>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.2.2.1\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.2.2.1\" extension=\"2015-08-01\"/>\n" +
                "<code code=\"11369-6\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"History of immunizations\"/>\n" +
                "<title>IMMUNIZATIONS</title>\n" +
                "<text>\n" +
                "<table border=\"1\" width=\"100%\">\n" +
                "<thead>\n" +
                "<tr>\n" +
                "<th>Vaccine Name</th>\n" +
                "<th>Vaccination Date</th>\n" +
                "</tr>\n" +
                "</thead>\n" +
                "<tbody>\n" +
                "<tr>\n" +
                "<td>\n" +
                "<content ID=\"vaccine1\">Unknown</content>\n" +
                "</td>\n" +
                "<td>\n" +
                "<content ID=\"vaccinationDate1\">Thu Jan 10 00:00:00 UTC 2013</content>\n" +
                "</td>\n" +
                "</tr>\n" +
                "</tbody>\n" +
                "</table>\n" +
                "</text>\n" +
                "<entry typeCode=\"DRIV\">\n" +
                "<substanceAdministration classCode=\"SBADM\" moodCode=\"EVN\" negationInd=\"false\" >\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.52\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.52\" extension=\"2015-08-01\"/>\n" +
                "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"example\"/>\n" +
                "<statusCode code=\"completed\"/>\n" +
                "<effectiveTime value=\"20130110\"/>\n" +
                "<consumable>\n" +
                "<manufacturedProduct classCode=\"MANU\">\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.54\"/>\n" +
                "<templateId root=\"2.16.840.1.113883.10.20.22.4.54\" extension=\"2014-06-09\"/>\n" +
                "<id root=\"68590af1-b46a-4529-8395-897994407075\"/>\n" +
                "<manufacturedMaterial>\n" +
                "<code nullFlavor=\"NI\"/>\n" +
                "</manufacturedMaterial>\n" +
                "</manufacturedProduct>\n" +
                "</consumable>\n" +
                "</substanceAdministration>\n" +
                "</entry>\n" +
                "</section>\n" +
                "</component>";
        Dstu2FhirData dstu2FhirData = new Dstu2FhirData();
        List<Immunization> immunizationList = new ArrayList<>();
        immunizationList.add(immunization);
        dstu2FhirData.setImmunizations(immunizationList);
        String actualXml = Dstu2CdaImmunizationGenerator.generateImmunizationSection(dstu2FhirData, launchDetails);
        //System.out.println(actualXml);
        Assert.assertNotNull(actualXml);
        //assertXmlEquals(expectedXml, actualXml);
    }

    @Test
    public void generateEmptyImmunizationSectionTest() {
        String exceptedXml = TestUtils.getFileContentAsString(EMPTY_IMMUNIZTION_SECTION);
        String actualXml = Dstu2CdaImmunizationGenerator.generateEmptyImmunizations();
        assertXmlEquals(exceptedXml, actualXml);
    }
}
