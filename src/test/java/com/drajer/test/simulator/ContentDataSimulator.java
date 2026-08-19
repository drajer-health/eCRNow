package com.drajer.test.simulator;

import static org.junit.Assert.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import com.drajer.cdafromr4.CdaMedicationGenerator;
import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.List;
import java.util.TimeZone;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.ResourceType;
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
public class ContentDataSimulator {

  public static final Logger logger = LoggerFactory.getLogger(ContentDataSimulator.class);

  R4FhirData r4FhirData;
  LaunchDetails launchDetails;

  FhirContext fhirContext;

  IParser r4Parser;

  @Autowired ApplicationUtils appUtils;

  @Before
  public void setUp() {
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    fhirContext = FhirContext.forR4();
    r4Parser = fhirContext.newJsonParser();
  }

  @Test
  public void testGenerationOfCdaSections() {

    logger.info("Running the test");

    Bundle b =
        appUtils.readBundleFromFile(
            "src/test/resources/SampleTestData/r4-loading-query-bundle-sample1.json");

    assertNotNull("Bundle should not be null", b);
    assertFalse("Bundle should have entries", b.getEntry().isEmpty());

    logger.info("Found Bundle with {} entries", b.getEntry().size());

    R4FhirData r4Data = new R4FhirData();
    List<Observation> observations = new ArrayList<>();
    List<MedicationStatement> medicationStatements = new ArrayList<>();
    List<MedicationRequest> mrs = new ArrayList<>();
    List<Medication> medications = new ArrayList<>();

    List<BundleEntryComponent> entries = b.getEntry();

    for (BundleEntryComponent ent : entries) {
      if (ent.getResource().getResourceType() == ResourceType.Patient) {
        Patient patient = (Patient) ent.getResource();
        r4Data.setPatient(patient);
        logger.info("Patient = {}", patient.getNameFirstRep().getFamily());
      } else if (ent.getResource().getResourceType() == ResourceType.Practitioner) {
        Practitioner practitioner = (Practitioner) ent.getResource();
        r4Data.setPractitioner(practitioner);
      } else if (ent.getResource().getResourceType() == ResourceType.Location) {
        Location location = (Location) ent.getResource();
        r4Data.setLocation(location);
      } else if (ent.getResource().getResourceType() == ResourceType.Encounter) {
        Encounter encounter = (Encounter) ent.getResource();
        r4Data.setEncounter(encounter);
      } else if (ent.getResource().getResourceType() == ResourceType.Observation) {
        observations.add((Observation) ent.getResource());
      } else if (ent.getResource().getResourceType() == ResourceType.MedicationStatement) {
        medicationStatements.add((MedicationStatement) ent.getResource());
      } else if (ent.getResource().getResourceType() == ResourceType.MedicationRequest) {
        mrs.add((MedicationRequest) ent.getResource());
      } else if (ent.getResource().getResourceType() == ResourceType.Medication) {
        medications.add((Medication) ent.getResource());
      }
    }

    r4Data.setLabResults(observations);
    r4Data.setMedicationStatements(medicationStatements);
    r4Data.setMedicationRequests(mrs);
    r4Data.setMedicationList(medications);

    assertNotNull("Patient should be set", r4Data.getPatient());
    assertNotNull("Encounter should be set", r4Data.getEncounter());

    LaunchDetails ld = new LaunchDetails();
    ld.setLaunchPatientId(r4Data.getPatient().getIdElement().getId());
    ld.setEncounterId(r4Data.getEncounter().getIdElement().getId());

    assertNotNull("LaunchDetails patientId should not be null", ld.getLaunchPatientId());
    assertNotNull("LaunchDetails encounterId should not be null", ld.getEncounterId());

    String secXml = CdaMedicationGenerator.generateMedicationSection(r4Data, ld, "");

    assertNotNull("Generated XML section should not be null", secXml);
    assertFalse("Generated XML section should not be empty", secXml.isEmpty());
    assertTrue("Generated XML should contain valid XML content", secXml.contains("<"));

    logger.info("Generated XML = {} chars", secXml.length());

    List<MedicationRequest> mrList =
        CdaMedicationGenerator.getValidMedicationRequests(r4Data, medications);

    assertNotNull("Medication request list should not be null", mrList);

    logger.info("Valid medication requests count: {}", mrList.size());
  }
}
