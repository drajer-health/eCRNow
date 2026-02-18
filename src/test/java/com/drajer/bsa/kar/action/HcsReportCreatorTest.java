package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.ehr.service.impl.EhrFhirR4QueryServiceImpl;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.InputStream;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;
import org.springframework.core.io.ClassPathResource;

public class HcsReportCreatorTest {

  FhirContext r4Context = FhirContext.forR4();
  KarProcessingData karProcessingData;
  EhrQueryService ehrQueryService;
  public static final String PROFILE =
      "http//://hl7.org/fhir/us/health-care-surveys-reporting/StructureDefinition/hcs-reporting-bundle";
  HcsReportCreator hcsReportCreator;

  @Before
  public void setUp() {
    hcsReportCreator = new HcsReportCreator();
    ehrQueryService = new EhrFhirR4QueryServiceImpl();
    karProcessingData = new KarProcessingData();
    karProcessingData.setKarStatus(getKnowledgeArtifactStatus());
    karProcessingData.setPhm(null);
    NotificationContext notificationContext = getNotificationContext();
    karProcessingData.setNotificationContext(notificationContext);
    karProcessingData.setHealthcareSetting(getHealthcareSetting());
    karProcessingData.setxRequestId("32");
    karProcessingData.setxCorrelationId(null);
    karProcessingData.setNotificationBundle(
        (Bundle)
            r4Context.newJsonParser().parseResource(notificationContext.getNotificationData()));
    karProcessingData.setFhirInputDataByType(getFilteredByType("/R4/LoadingQueryBundle.json"));
    karProcessingData.setKar(getKnowledgeArtifact());
  }

  @Test
  public void testCreateReport() {

    karProcessingData.setKarStatus(getKnowledgeArtifactStatus());
    Set<Resource> inputData = new HashSet<>();
    Resource resource =
        TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    inputData.add(resource);
    BsaAction bsaAction = getBsaAction();
    Bundle actualReport =
        (Bundle)
            hcsReportCreator.createReport(
                karProcessingData, ehrQueryService, "example", PROFILE, bsaAction);
    assertTrue(actualReport.hasId());
    Bundle exceptedReport = TestUtils.loadBundleFromFile("Bsa/report/Hcs-report/report.json");
    assertEquals(exceptedReport.getEntry().size(), actualReport.getEntry().size());

    String actualComposition = getComposition(actualReport);
    String expectedComposition = getComposition(exceptedReport);
    assertEquals(expectedComposition, actualComposition);
  }

  @Test
  public void testreferenceTo() {
    Resource patient = TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    hcsReportCreator.referenceTo(patient);
    assertTrue(patient.hasId());
    assertNotNull(patient);
  }

  @Test
  public void testpopulateReasonForVisitNarrative() {
    Composition.SectionComponent sectionComponent = new Composition.SectionComponent();
    sectionComponent.fhirType();
    hcsReportCreator.populateReasonForVisitNarrative(sectionComponent, karProcessingData);
  }

  private BsaAction getBsaAction() {
    BsaAction action = new CreateReport();
    HashMap<String, String> inputRelatedData = new HashMap<>();
    inputRelatedData.put("1234", "1234");
    inputRelatedData.put("123", "123");
    action.setInputDataIdToRelatedDataIdMap(inputRelatedData);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setType("Patient");
    dataRequirement.setId("1234");
    DataRequirement dataRequirementEncounter = new DataRequirement();
    dataRequirementEncounter.setType("Encounter");
    dataRequirementEncounter.setId("123");

    List<DataRequirement> dataRequirements = new ArrayList<>();
    dataRequirements.add(dataRequirement);
    dataRequirements.add(dataRequirementEncounter);
    action.setInputData(dataRequirements);

    HashMap<String, Set<Resource>> actionOutputDataById = new HashMap<>();
    Set<Resource> res = new HashSet<>();
    res.add(TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json"));
    actionOutputDataById.put("1234", res);
    karProcessingData.setActionOutputDataById(actionOutputDataById);
    Set<Resource> resEnc = new HashSet<>();
    resEnc.add(
        TestUtils.loadResourceDataFromFile(
            Encounter.class, "R4/Encounter/Encounter_97953900.json"));
    actionOutputDataById.put("123", resEnc);
    karProcessingData.setActionOutputDataById(actionOutputDataById);
    return action;
  }

  private HashMap<ResourceType, Set<Resource>> getFilteredByType(String filePath) {
    HashMap<ResourceType, Set<Resource>> groupedResources = new HashMap<>();
    try {
      FhirContext ctx = FhirContext.forR4();
      Bundle bundle = loadBundleFromFile(filePath);

      for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
        Resource resource = entry.getResource();
        if (resource != null) {

          groupedResources
              .computeIfAbsent(resource.getResourceType(), k -> new HashSet<>())
              .add(resource);
        }
      }

    } catch (Exception e) {
      e.printStackTrace();
    }
    return groupedResources;
  }

  private Bundle loadBundleFromFile(String filename) {
    try (InputStream in = new ClassPathResource(filename).getInputStream()) {
      return r4Context.newJsonParser().parseResource(Bundle.class, in);
    } catch (Exception e) {
      return null;
    }
  }

  private KnowledgeArtifactStatus getKnowledgeArtifactStatus() {
    KnowledgeArtifactStatus status = new KnowledgeArtifactStatus();
    status.setId(1);
    status.setIsActive(true);
    status.setOutputFormat(BsaTypes.OutputContentType.FHIR);
    status.setKarVersion("1.0.0");
    status.setKarId("healthcare-survey-specification-bundle-example");
    status.setLastActivationDate(new Date());
    status.setSubscriptionsEnabled(false);
    status.setCovidOnly(false);
    return status;
  }

  private HealthcareSetting getHealthcareSetting() {
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting =
        (HealthcareSetting)
            TestUtils.getResourceAsObject("Bsa/HealthCareSettings.json", HealthcareSetting.class);
    return healthcareSetting;
  }

  private KnowledgeArtifact getKnowledgeArtifact() {
    KnowledgeArtifact knowledgeArtifact = new KnowledgeArtifact();
    Set<UriType> receiverAddresses = new HashSet<>();
    receiverAddresses.add(new UriType("http://receiver1.example.com"));
    receiverAddresses.add(new UriType("http://receiver2.example.com"));
    knowledgeArtifact.setReceiverAddresses(receiverAddresses);
    return knowledgeArtifact;
  }

  private NotificationContext getNotificationContext() {
    NotificationContext context =
        TestUtils.readFileContents(
            "Bsa/NotificationContext/NotificationContext.json",
            new TypeReference<NotificationContext>() {});
    Bundle nb = loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");
    context.setNotificationData(r4Context.newJsonParser().encodeResourceToString(nb));
    context.setNotificationResourceType("Encounter");

    return context;
  }

  public String getComposition(Bundle bundle) {
    for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
      if (entry.getResource() instanceof Composition) {
        return r4Context.newJsonParser().encodeResourceToString((Composition) entry.getResource());
      }
    }
    return null;
  }
}
