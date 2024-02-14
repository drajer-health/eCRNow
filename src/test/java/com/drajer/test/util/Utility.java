package com.drajer.test.util;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatusList;
import com.drajer.bsa.kar.action.SubmitReport;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.cda.parser.CdaCode;
import com.drajer.cda.parser.CdaIi;
import com.drajer.cda.parser.CdaRrModel;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ValueSet;
import org.springframework.core.io.ClassPathResource;

public interface Utility {

  static CdaRrModel getCdaRrModel() {
    CdaRrModel cdaRrModel = new CdaRrModel();
    CdaIi cdaIi = new CdaIi();
    cdaIi.setLineNumber(1);
    cdaIi.setValue("123");
    cdaIi.setRootValue("root");
    cdaRrModel.setErrors("error");
    cdaIi.setExtValue("59662");
    cdaRrModel.setReportableType("RRVS1");
    cdaRrModel.setEicrDocId(cdaIi);
    cdaRrModel.setRrDocId(cdaIi);

    List<CdaIi> cdaIiList = new ArrayList<>();
    cdaIiList.add(cdaIi);
    cdaRrModel.setPatientId(cdaIiList);
    cdaRrModel.setEncounterId(cdaIiList);

    CdaCode cdaCode = new CdaCode();
    cdaCode.setCode("AN");
    cdaCode.setCodeSystem("http://terminology.hl7.org/CodeSystem/v2-0203");

    cdaRrModel.setReportableStatus(cdaCode);
    return cdaRrModel;
  }

  static CdaRrModel cdaRrModel() {
    CdaRrModel cdaRrModel = new CdaRrModel();
    CdaIi cdaIi = new CdaIi();
    cdaIi.setLineNumber(1);
    cdaIi.setValue("123");
    cdaIi.setRootValue("root");
    cdaRrModel.setErrors("error");
    cdaRrModel.setEicrDocId(cdaIi);
    cdaRrModel.setRrDocId(cdaIi);

    CdaCode cdaCode = new CdaCode();
    cdaCode.setCode("AN");
    cdaCode.setCodeSystem("http://terminology.hl7.org/CodeSystem/v2-0203");
    return cdaRrModel;
  }

  static CheckTriggerCodeStatusList getCheckTriggerCodeStatusList() {

    CheckTriggerCodeStatusList checkTriggerCodeStatusList = new CheckTriggerCodeStatusList();
    CheckTriggerCodeStatus status = new CheckTriggerCodeStatus();
    status.setActionId("123487248364587346593465936324923293");
    status.setActionStatus(BsaActionStatusType.COMPLETED);
    status.setTriggerMatchStatus(true);
    status.setActionType(ActionType.CHECK_TRIGGER_CODES);
    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setValueSet("value set");
    matchedTriggerCodes.setValueSetVersion("");
    List<MatchedTriggerCodes> list = new ArrayList<>();
    list.add(matchedTriggerCodes);
    status.setMatchedCodes(list);
    Set<CheckTriggerCodeStatus> checkTriggerCodeStatus = new HashSet<CheckTriggerCodeStatus>();
    checkTriggerCodeStatus.add(status);
    checkTriggerCodeStatusList.setStatuses(checkTriggerCodeStatus);
    return checkTriggerCodeStatusList;
  }

  static KarProcessingData getKarProcessingData() throws IOException {
    KarProcessingData karProcessingData = new KarProcessingData();
    karProcessingData.setCdaResponseData("Reportability Response Received from AIMS");
    karProcessingData.setExecutionSequenceId("86574");
    karProcessingData.setFhirResponseData("Reportability Response Received from fhir");
    karProcessingData.setxCorrelationId("ecrunittest_id");
    karProcessingData.setxRequestId("ecrUnitTestCorrelationID");

    KnowledgeArtifact knowledgeArtifact = new KnowledgeArtifact();
    knowledgeArtifact.setKarId("765");
    knowledgeArtifact.setKarVersion("1.0.0");
    knowledgeArtifact.initializeRelatedDataIds();
    HashMap<ResourceType, HashMap<String, Resource>> dependencies = new HashMap<>();
    HashMap<String, Resource> resource = new HashMap<>();
    FhirContext fhirContext = FhirContext.forR4();
    ValueSet valueSet =
        fhirContext
            .newJsonParser()
            .parseResource(ValueSet.class, Utility.class.getResourceAsStream("/Bsa/ValueSet.json"));
    resource.put("ValueSet", valueSet);
    dependencies.put(ResourceType.ValueSet, resource);
    knowledgeArtifact.addDependentResource(valueSet);
    karProcessingData.setKar(knowledgeArtifact);

    BsaActionStatus bsaActionStatus = new CheckTriggerCodeStatus();
    bsaActionStatus.setActionId("8723");
    bsaActionStatus.setActionStatus(BsaActionStatusType.SCHEDULED);
    bsaActionStatus.setActionType(ActionType.CHECK_TRIGGER_CODES);
    karProcessingData.addActionStatus("id", bsaActionStatus);

    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setEhrAccessToken(
        "eyJraWQiOiIyMDIwLTA5LTA4VDE4OjE2OjExLjAxNC5lYyIsInR5cCI6IkpXVCIsImFsZyI6IkVTMjU2In0.eyJpc3MiOiJodHRwczpcL1wvYXV0aG9yaXphdGlvbi5jZXJuZXIuY29tXC8iLCJleHAiOjE1OTk3MjQ1MTEsImlhdCI6MTU5OTcyMzkxMSwianRpIjoiNGEyZDg3YWItNjVlNC00ZWEwLTliMWItNDFlMjcwZjM1MmMwIiwidXJuOmNlcm5lcjphdXRob3JpemF0aW9uOmNsYWltczp2ZXJzaW9uOjEiOnsidmVyIjoiMS4wIiwicHJvZmlsZXMiOnsic21hcnQtdjEiOnsiYXpzIjoic3lzdGVtXC9DYXJlUGxhbi5yZWFkIHN5c3RlbVwvQ29uZGl0aW9uLnJlYWQgc3lzdGVtXC9FbmNvdW50ZXIucmVhZCBzeXN0ZW1cL09ic2VydmF0aW9uLnJlYWQgc3lzdGVtXC9QYXRpZW50LnJlYWQgc3lzdGVtXC9QcmFjdGl0aW9uZXIucmVhZCBzeXN0ZW1cL1Byb2NlZHVyZS5yZWFkIn19LCJjbGllbnQiOnsibmFtZSI6ImVDUiBOT1cgU3lzdGVtIiwiaWQiOiI1OTQzZDY5Yy02NjBhLTRlZjMtODkzOC0xMTgwMmZhNGQyMzAifSwidGVuYW50IjoiZWMyNDU4ZjItMWUyNC00MWM4LWI3MWItMGU3MDFhZjc1ODNkIn19.wR1qxo5M3LVGCzW4QbQBwI3D2p5IzYJJrIP0yhsR7wxNrspTTStgYQuIoZjzncfhrAZ5rIjIM2VRT37FADG4lw");
    notificationContext.setFhirServerBaseUrl("http://ecrunitest/ecr/dao");
    notificationContext.setPatientId("59662");
    notificationContext.setNotificationResourceId("677");
    karProcessingData.setNotificationContext(notificationContext);

    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setEncounterId("bp-report-denom-exc-in");
    publicHealthMessage.setFhirServerBaseUrl("http://ecrunitest/ecr/dao");
    publicHealthMessage.setSubmittedVersionNumber(1);
    karProcessingData.setPhm(publicHealthMessage);

    String healthcareSettingJson =
        new String(
            Files.readAllBytes(
                new ClassPathResource("/Bsa/HealthCareSettings.json").getFile().toPath()));
    ObjectMapper objectMapper = new ObjectMapper();
    HealthcareSetting healthcareSetting =
        objectMapper.readValue(healthcareSettingJson, HealthcareSetting.class);
    karProcessingData.setHealthcareSetting(healthcareSetting);

    return karProcessingData;
  }

  static BsaAction getBsaAction() {
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setDisallowExtensions(true);
    dataRequirement.setId("7654");
    dataRequirement.setLimit(30);
    DataRequirement.DataRequirementCodeFilterComponent codeFilter =
        new DataRequirement.DataRequirementCodeFilterComponent();
    codeFilter.setPath("gender");
    codeFilter.setValueSetElement(
        new CanonicalType("http://hl7.org/fhir/ValueSet/administrative-gender"));
    dataRequirement.getCodeFilter().add(codeFilter);
    List<DataRequirement> dataRequirementList = new ArrayList<>();
    dataRequirementList.add(dataRequirement);

    BsaAction bsaAction = new SubmitReport();
    bsaAction.setInputData(dataRequirementList);
    bsaAction.addRelatedDataId("7654", "54785");
    return bsaAction;
  }

  static KarProcessingData karProcessingData() {
    KnowledgeArtifact knowledgeArtifact = new KnowledgeArtifact();
    knowledgeArtifact.setKarId("7645673");
    knowledgeArtifact.setKarName("AdultOutpatientEncountersFHIR4");
    knowledgeArtifact.setKarVersion("v5.0.0");

    FhirContext fhirContext = FhirContext.forR4();
    ValueSet valueSet =
        fhirContext
            .newJsonParser()
            .parseResource(ValueSet.class, Utility.class.getResourceAsStream("/Bsa/ValueSet.json"));

    HashMap<ResourceType, HashMap<String, Resource>> dependencies = new HashMap<>();
    HashMap<String, Resource> resource = new HashMap<>();
    resource.put("ValueSet", valueSet);
    dependencies.put(ResourceType.ValueSet, resource);
    knowledgeArtifact.setDependencies(dependencies);
    knowledgeArtifact.addDependentResource(valueSet);
    KarProcessingData karProcessingData = new KarProcessingData();
    karProcessingData.setKar(knowledgeArtifact);

    Map<String, String> mdcContext = new HashMap<>();
    mdcContext.put("key1", "value1");
    mdcContext.put("key2", "value2");

    /*   ScheduledJobData jobData =
        new ScheduledJobData(
            UUID.randomUUID(),
            "action123",
            ActionType.CHECK_TRIGGER_CODES,
            Instant.now(),
            "job456",
            "xRequestId123",
            BsaJobType.IMMEDIATE_REPORTING,
            mdcContext);

    karProcessingData.setScheduledJobData(jobData); */
    return karProcessingData;
  }
}
