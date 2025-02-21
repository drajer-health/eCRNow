package com.drajer.cdafromdstu2;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.resource.*;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.simulator.ContentDataSimulator;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.junit.Before;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.InputStream;
import java.util.TimeZone;

import static org.junit.Assert.assertEquals;

@PowerMockIgnore({"javax.crypto.*"})
public class BaseGenerator {

    protected static final String PATIENT_CDA_FILE = "CdaDstuTestData/Cda/Patient/Patient.xml";
    protected Patient patient;
    protected Bundle bundle;
    public static final Logger logger =
            LoggerFactory.getLogger(com.drajer.cdafromr4.BaseGeneratorTest.class);
    public static final FhirContext fhirContext = FhirContext.forDstu2();
    public static final String EXCEPTION_READING_FILE = "Exception Reading File";

    public static final String LAUNCH_DETAILS_FILENAME =
            "CdaDstuTestData/LaunchDetails/LaunchDetails.json";
    public static final String BUNDLE_RES_FILENAME = "CdaTestData/Bundle/Bundle.json";
    public static final String PATIENT_RES_FILENAME = "CdaTestData/patient/Patient.json";
    static final String ECIR_DETAILS_FILENAME = "R4/Misc/eicr.json";
    public static final String XML_FOR_II_USING_GUID =
            "<id root=\"b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9\"/>";

    Dstu2FhirData dstu2FhirDataForPatient;
    Dstu2FhirData dstu2FhirDataForBundle;
    LaunchDetails launchDetails = loadLaunchDetailsFromFile();
    Eicr eicr = loadEicrDetailsFromFile();

    @Before
    public void setupTestCase() {
        TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
        patient = loadResourceDataFromFile(Patient.class, PATIENT_RES_FILENAME);
        //bundle = loadResourceDataFromFile(Bundle.class, BUNDLE_RES_FILENAME);
        bundle = new Bundle();
        dstu2FhirDataForPatient = new Dstu2FhirData();
        dstu2FhirDataForBundle = new Dstu2FhirData();
        dstu2FhirDataForPatient.setPatient(patient);
        dstu2FhirDataForBundle.setPatient(patient);
        dstu2FhirDataForBundle.setData(bundle);
    }

    public LaunchDetails loadLaunchDetailsFromFile() {
        ReflectionTestUtils.setField(AESEncryption.class, "secretKey", "123");
        return TestUtils.readFileContents(
                LAUNCH_DETAILS_FILENAME, new TypeReference<LaunchDetails>() {
                });
    }

    public Eicr loadEicrDetailsFromFile() {

        return TestUtils.readFileContents(ECIR_DETAILS_FILENAME, new TypeReference<Eicr>() {
        });
    }

    public <T extends IBaseResource> T loadResourceDataFromFile(
            Class<T> resourceType, String filename) {

        try (InputStream in = new ClassPathResource(filename).getInputStream()) {

            return fhirContext.newJsonParser().parseResource(resourceType, in);

        } catch (Exception e) {
            logger.error(EXCEPTION_READING_FILE, e);
        }
        return null;
    }

    public void assertXmlEquals(String expectedXml, String actualXml) {
        expectedXml = StringUtils.normalizeSpace(expectedXml).trim();
        actualXml = StringUtils.normalizeSpace(actualXml).trim();
        assertEquals(expectedXml, actualXml);
    }

    public Bundle loadBundleFromFile(String filename) {
        try (InputStream in = new ClassPathResource(filename).getInputStream()) {
            return fhirContext.newJsonParser().parseResource(Bundle.class, in);
        } catch (Exception e) {
            ContentDataSimulator.logger.error(EXCEPTION_READING_FILE, e);
            return null;
        }
    }

    public static void addResourceToFhirData(Bundle bundle, Dstu2FhirData data) {
        if (bundle != null && bundle.getEntry() != null) {
            for (Bundle.Entry entry : bundle.getEntry()) {
                if (entry.getResource() instanceof Patient) {
                    data.setPatient((Patient) entry.getResource());
                } else if (entry.getResource() instanceof Practitioner) {
                    data.setPractitioner((Practitioner) entry.getResource());
                } else if (entry.getResource() instanceof Encounter) {
                    data.setEncounter((Encounter) entry.getResource());
                } else if (entry.getResource() instanceof Location) {
                    data.setLocation((Location) entry.getResource());
                } else if (entry.getResource() instanceof Organization) {
                    data.setOrganization((Organization) entry.getResource());
                }
            }
        }
    }
}
