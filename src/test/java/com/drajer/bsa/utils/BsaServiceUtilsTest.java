package com.drajer.bsa.utils;

import static org.junit.Assert.assertEquals;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.service.KarParser;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.*;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@TestPropertySource("classpath:application-test.properties")
@ComponentScan(
    basePackages = {
      "com.drajer.bsa.service",
      "com.drajer.bsa.dao",
      "com.drajer.bsa.dao.impl",
      "org.hibernate",
      "com.drajer.ecrapp.config",
      "com.drajer.bsa.kar.model"
    })
@EnableAutoConfiguration
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class BsaServiceUtilsTest {
  private static final Logger logger = LoggerFactory.getLogger(BsaServiceUtilsTest.class);
  @Autowired KarParser parser;

  @Autowired KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;

  private static Set<Resource> resources = new HashSet<>();

  @BeforeClass
  public static void initializeTestData() {
    // read in all of the bundles in the
    FhirContext ctx = FhirContext.forR4();
    Bundle bundle =
        ctx.newJsonParser()
            .parseResource(
                Bundle.class,
                BsaServiceUtilsTest.class.getResourceAsStream(
                    "/R4/Condition/ConditionWithAndWithoutTriggerCodes.json"));
    bundle
        .getEntry()
        .forEach(
            (e) -> {
              resources.add(e.getResource());
            });
    logger.info("Test Data {}", resources);
  }

  @Test
  public void filterResourcesWithCodeTest() {

    KnowledgeArtifact kar =
        knowledgeArtifactRepositorySystem.getById("cancer-specification-bundle-example");
    KarProcessingData kd = new KarProcessingData();
    kd.setKar(kar);
    DataRequirement dr = new DataRequirement();
    DataRequirement.DataRequirementCodeFilterComponent codeFilter = dr.addCodeFilter();
    codeFilter.setPath("clinicalStatus");
    List<Coding> codes = new ArrayList<>();
    codes.add(
        new Coding()
            .setCode("resolved")
            .setSystem("http://terminology.hl7.org/CodeSystem/condition-clinical"));
    codeFilter.setCode(codes);

    Set<Resource> filtered = BsaServiceUtils.filterResources(resources, dr, kd);

    assertEquals(8, resources.size());
    assertEquals(2, filtered.size());
  }

  @Test
  public void filterResourcesWithValuesetTest() {
    KnowledgeArtifact kar =
        knowledgeArtifactRepositorySystem.getById("bundle-ersd-specification-bundle|2");
    KarProcessingData kd = new KarProcessingData();
    kd.setKar(kar);
    DataRequirement dr = new DataRequirement();
    DataRequirement.DataRequirementCodeFilterComponent codeFilter = dr.addCodeFilter();
    codeFilter.setPath("code");
    codeFilter.setValueSet("http://cts.nlm.nih.gov/fhir/ValueSet/2.16.840.1.113762.1.4.1146.1124");
    Set<Resource> filtered = BsaServiceUtils.filterResources(resources, dr, kd);

    assertEquals(8, resources.size());
    assertEquals(1, filtered.size());
  }

  //    @Test
  //    public void filterResourcesByDateDurationTest() {
  //      KarProcessingData kd = new KarProcessingData();
  //      DataRequirement dr = new DataRequirement();
  //      DataRequirement.DataRequirementDateFilterComponent dateFilter = dr.addDateFilter();
  //      dateFilter.setValue(new Duration().setUnit());
  //      Set<Resource> resources = new HashSet<Resource>();
  //      Set<Resource> filtered = BsaServiceUtils.filterResources(resources,dr,kd);
  //
  //      assertEquals(10, resources.size());
  //      assertEquals(2,filtered.size());
  //
  //    }
  //  //
  //
  //  @Test
  //  public void filterResourcesByDateRangeTest() {
  //    KarProcessingData kd = new KarProcessingData();
  //    DataRequirement dr = new DataRequirement();
  //    DataRequirement.DataRequirementDateFilterComponent dateFilter = dr.addDateFilter();
  //    dateFilter.setValue(new Period());
  //    Set<Resource> resources = new HashSet<Resource>();
  //    Set<Resource> filtered = BsaServiceUtils.filterResources(resources,dr,kd);
  //
  //    assertEquals(10, resources.size());
  //    assertEquals(2,filtered.size());
  //
  //  }
  //
  //  @Test
  //  public void filterResourcesByDateTimeTest() {
  //    KarProcessingData kd = new KarProcessingData();
  //    DataRequirement dr = new DataRequirement();
  //    DataRequirement.DataRequirementDateFilterComponent dateFilter = dr.addDateFilter();
  //    dateFilter.setValue(new DateTimeType());
  //    Set<Resource> resources = new HashSet<Resource>();
  //    Set<Resource> filtered = BsaServiceUtils.filterResources(resources,dr,kd);
  //
  //    assertEquals(10, resources.size());
  //    assertEquals(2,filtered.size());
  //  }
  //
  //  private Set<Resource> generateResources(){
  //    Set<Resource> resources = new HashSet<>();
  //
  //    Observation ob = new Observation();
  //    resources.add(ob);
  //
  //    return resources;
  //  }

}
