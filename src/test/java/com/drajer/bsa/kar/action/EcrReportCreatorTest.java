package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.Mockito.doReturn;
import static org.powermock.api.mockito.PowerMockito.mock;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import java.util.HashSet;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class EcrReportCreatorTest {

  @Spy @InjectMocks EcrReportCreator reportCreator;

  @Test
  public void createReport_shouldGenerateCdaR11Bundle_withMessageHeaderAndContentBundle() {
    String dataRequirementId = "report-123";
    String profile = "http://example.org/fhir/StructureDefinition/sample-report-profile";
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    KnowledgeArtifactStatus art = new KnowledgeArtifactStatus();
    art.setVersionUniqueKarId("id|r4");
    art.setOutputFormat(BsaTypes.OutputContentType.CDA_R11);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    data.setKarStatus(art);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    BsaAction bsaAction = mock(BsaAction.class);
    Set<Resource> inputData = new HashSet<>();
    Patient patient = new Patient();
    patient.setId("patient-1");
    patient.addName().setFamily("Doe").addGiven("John");
    Encounter encounter = new Encounter();
    encounter.setId("enc-1");
    encounter.setStatus(Encounter.EncounterStatus.FINISHED);
    inputData.add(patient);
    inputData.add(encounter);
    Bundle mockContentBundle = mock(Bundle.class);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    data.setHealthcareSetting(healthcareSetting);

    doReturn(mockContentBundle)
        .when(reportCreator)
        .getCdaR11Report(data, ehrService, dataRequirementId, profile, bsaAction);

    Resource result =
        reportCreator.createReport(
            data, ehrService, inputData, dataRequirementId, profile, bsaAction);

    assertNotNull(result);
    assertTrue(result instanceof Bundle);
    Bundle resultBundle = (Bundle) result;
    assertEquals(2, resultBundle.getEntry().size());
  }

  @Test
  public void createReport_shouldGenerateCdaR30Bundle_withMessageHeaderAndContentBundle() {
    String dataRequirementId = "report-123";
    String profile = "http://example.org/fhir/StructureDefinition/sample-report-profile";
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    KnowledgeArtifactStatus art = new KnowledgeArtifactStatus();
    art.setVersionUniqueKarId("id|r4");
    art.setOutputFormat(BsaTypes.OutputContentType.CDA_R30);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    data.setKarStatus(art);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    BsaAction bsaAction = mock(BsaAction.class);
    Set<Resource> inputData = new HashSet<>();
    Patient patient = new Patient();
    patient.setId("patient-1");
    patient.addName().setFamily("Doe").addGiven("John");
    Encounter encounter = new Encounter();
    encounter.setId("enc-1");
    encounter.setStatus(Encounter.EncounterStatus.FINISHED);
    inputData.add(patient);
    inputData.add(encounter);
    Bundle mockContentBundle = mock(Bundle.class);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    data.setHealthcareSetting(healthcareSetting);

    doReturn(mockContentBundle)
        .when(reportCreator)
        .getCdaR31Report(data, ehrService, dataRequirementId, profile, bsaAction);

    Resource result =
        reportCreator.createReport(
            data, ehrService, inputData, dataRequirementId, profile, bsaAction);

    assertNotNull(result);
    assertTrue(result instanceof Bundle);
    Bundle resultBundle = (Bundle) result;
    assertEquals(2, resultBundle.getEntry().size());
  }

  @Test
  public void createReport_shouldGenerateBothBundle_withMessageHeaderAndContentBundle() {
    String dataRequirementId = "report-123";
    String profile = "http://example.org/fhir/StructureDefinition/sample-report-profile";
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    KnowledgeArtifactStatus art = new KnowledgeArtifactStatus();
    art.setVersionUniqueKarId("id|r4");
    art.setOutputFormat(BsaTypes.OutputContentType.BOTH);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    data.setKarStatus(art);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    BsaAction bsaAction = mock(BsaAction.class);
    Set<Resource> inputData = new HashSet<>();
    Patient patient = new Patient();
    patient.setId("patient-1");
    patient.addName().setFamily("Doe").addGiven("John");
    Encounter encounter = new Encounter();
    encounter.setId("enc-1");
    encounter.setStatus(Encounter.EncounterStatus.FINISHED);
    inputData.add(patient);
    inputData.add(encounter);
    Bundle mockContentBundle = mock(Bundle.class);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    data.setHealthcareSetting(healthcareSetting);

    doReturn(mockContentBundle)
        .when(reportCreator)
        .getCdaR31Report(data, ehrService, dataRequirementId, profile, bsaAction);
    doReturn(mockContentBundle)
        .when(reportCreator)
        .getCdaR11Report(data, ehrService, dataRequirementId, profile, bsaAction);

    doReturn(mockContentBundle)
        .when(reportCreator)
        .getFhirReport(data, ehrService, dataRequirementId, profile, bsaAction);

    Resource result =
        reportCreator.createReport(
            data, ehrService, inputData, dataRequirementId, profile, bsaAction);

    assertNotNull(result);
    assertTrue(result instanceof Bundle);
    Bundle resultBundle = (Bundle) result;
    assertEquals(4, resultBundle.getEntry().size());
  }
}
