package com.drajer.test.simulator;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.model.dstu2.resource.Location;
import ca.uhn.fhir.model.dstu2.resource.MedicationStatement;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import ca.uhn.fhir.model.dstu2.resource.Patient;
import ca.uhn.fhir.model.dstu2.resource.Practitioner;
import ca.uhn.fhir.parser.IParser;
import com.drajer.cdafromdstu2.Dstu2CdaResultGenerator;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

@ContextConfiguration(classes = SpringConfiguration.class)
@RunWith(SpringRunner.class)
@SpringBootTest
@Transactional
@ActiveProfiles("test")
public class EhrDataTester {

  public static final Logger logger = LoggerFactory.getLogger(EhrDataTester.class);

  Dstu2FhirData dstu2FhirData;
  LaunchDetails launchDetails;

  FhirContext fhirContext;

  IParser dstu2Parser;

  @Autowired ApplicationUtils appUtils;

  @Before
  public void setUp() {
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    fhirContext = FhirContext.forDstu2();
    dstu2Parser = fhirContext.newJsonParser();
  }

  @Test
  public void testObsTriggerCodeXml() {

    logger.info(" Running the test ");

    Bundle b =
        appUtils.readDstu2BundleFromFile(
            "src/test/resources/SampleTestData/dstu2-loading-query-bundle-sample1.json");

    if (b != null) {
      logger.info(" Found Bundle ");

      Dstu2FhirData dstu2Data = new Dstu2FhirData();
      List<Observation> observations = new ArrayList();

      List<MedicationStatement> medicationStatements = new ArrayList();
      List<Entry> entries = b.getEntry();
      for (Entry ent : entries) {

        if (ent.getResource().getResourceName().equalsIgnoreCase("Patient")) {
          Patient patient = (Patient) ent.getResource();
          dstu2Data.setPatient(patient);
          dstu2Parser.encodeResourceToString(patient);
          logger.info(" Patient = {}", dstu2Parser.encodeResourceToString(patient));
        } else if (ent.getResource().getResourceName().equalsIgnoreCase("Practitioner")) {
          Practitioner practitioner = (Practitioner) ent.getResource();
          dstu2Data.setPractitioner(practitioner);
          dstu2Parser.encodeResourceToString(practitioner);
          logger.info(" Practitioner = {}", dstu2Parser.encodeResourceToString(practitioner));
        } else if (ent.getResource().getResourceName().equalsIgnoreCase("Location")) {
          Location location = (Location) ent.getResource();
          dstu2Data.setLocation(location);
          dstu2Parser.encodeResourceToString(location);
          logger.info(" Location = {}", dstu2Parser.encodeResourceToString(location));
        } else if (ent.getResource().getResourceName().equalsIgnoreCase("Encounter")) {
          Encounter encounter = (Encounter) ent.getResource();
          dstu2Data.setEncounter(encounter);
          dstu2Parser.encodeResourceToString(encounter);
          logger.info(" Encounter = {}", dstu2Parser.encodeResourceToString(encounter));
        } else if (ent.getResource().getResourceName().equalsIgnoreCase("Observation")) {
          observations.add((Observation) ent.getResource());
        } else if (ent.getResource().getResourceName().equalsIgnoreCase("MedicationStatement")) {
          medicationStatements.add((MedicationStatement) ent.getResource());
        }
      }

      dstu2Data.setLabResults(observations);
      dstu2Data.setMedications(medicationStatements);

      StringBuilder codeXml = new StringBuilder(200);
      StringBuilder valXml = new StringBuilder(200);

      MatchedTriggerCodes mtc = new MatchedTriggerCodes();

      Set<String> codes = new HashSet<>();
      String c = "http://loinc.org|11475-1";
      codes.add(c);
      mtc.setMatchedCodes(codes);
      mtc.setMatchedPath("Observation.code");
      for (Observation obs : observations) {

        Dstu2CdaResultGenerator.doesTriggerCodesMatchObservation(obs, mtc, codeXml, valXml);
        assert (codeXml.toString().length() > 0);
        assert (valXml.toString().length() > 0);
      }

    } else {
      logger.info(" Bundle is null ");
    }
  }
}
