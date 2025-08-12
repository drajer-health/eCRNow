package com.drajer.bsa.service.impl;

import static org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType.BEFORESTART;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.dao.TimeZoneDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.ehr.subscriptions.SubscriptionGeneratorService;
import com.drajer.bsa.kar.action.ExecuteReportingActions;
import com.drajer.bsa.kar.action.SubmitReport;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.routing.impl.DirectTransportImpl;
import com.drajer.bsa.routing.impl.RestfulTransportImpl;
import com.drajer.bsa.scheduler.BsaScheduler;
import com.drajer.bsa.service.KarService;
import com.drajer.bsa.service.PublicHealthAuthorityService;
import com.drajer.bsa.utils.BsaConstants;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.sof.utils.FhirContextInitializer;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.opencds.cqf.cql.evaluator.expression.ExpressionEvaluator;
import org.opencds.cqf.cql.evaluator.library.LibraryProcessor;
import org.opencds.cqf.cql.evaluator.measure.r4.R4MeasureProcessor;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.test.util.ReflectionTestUtils;

public class KarParserImplTest {
  @Mock BsaServiceUtils utils;
  @Mock KnowledgeArtifact mocknowlwdgeArtifact;
  @Mock BsaScheduler scheduler;
  @Mock KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;
  @Mock R4MeasureProcessor measureProcessor;
  @Mock ExpressionEvaluator expressionEvaluator;
  @Mock LibraryProcessor libraryProcessor;
  @Mock PublicHealthMessagesDao phDao;
  @Mock HealthcareSettingsDao hsDao;
  @Mock SubscriptionGeneratorService subscriptionGeneratorService;
  @Mock KarService karService;
  @Mock EhrQueryService ehrInterface;
  @Mock DirectTransportImpl directInterface;
  @Mock RestfulTransportImpl restSubmitter;
  @Mock AuthorizationUtils authUtils;
  @Mock FhirContextInitializer fhirContextInitializer;
  @Mock PublicHealthAuthorityService publicHealthAuthorityService;
  @Mock TimeZoneDao timezoneDao;
  @Mock AutowireCapableBeanFactory beanFactory;
  @InjectMocks KarParserImpl karParser;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testgetAction_withInstantBean() {
    ExecuteReportingActions mockExecutionReportingAction = mock(ExecuteReportingActions.class);
    String actionId = "execute-reporting-workflow";

    HashMap<String, String> actionclass = new HashMap<>();
    actionclass.put(actionId, "com.drajer.bsa.kar.action.ExecuteReportingActions");
    ReflectionTestUtils.setField(karParser, "actionClasses", actionclass);

    when(beanFactory.getBean(eq(ExecuteReportingActions.class)))
        .thenReturn(mockExecutionReportingAction);

    BsaAction result = karParser.getAction(actionId);

    assertNotNull(result);
    assertTrue(result instanceof ExecuteReportingActions);
    verify(beanFactory, times(2)).getBean(eq(ExecuteReportingActions.class));
  }

  @Test
  public void testinitializeRepository() {
    String karDirectory = "src/test/resources/Bsa";
    Bundle karBundle = new Bundle();
    karBundle.setType(Bundle.BundleType.COLLECTION);
    karBundle.setId("urn:1234");

    Meta meta = new Meta();
    meta.setVersionId("1");

    Meta meta1 = new Meta();
    meta1.setVersionId("2");

    karBundle.setMeta(meta);
    List<Bundle.BundleEntryComponent> bundleEntryComponentList = new ArrayList<>();

    Bundle.BundleEntryComponent comp1 = new Bundle.BundleEntryComponent();
    Bundle.BundleEntryComponent comp2 = new Bundle.BundleEntryComponent();
    Bundle.BundleEntryComponent comp3 = new Bundle.BundleEntryComponent();
    Bundle.BundleEntryComponent comp4 = new Bundle.BundleEntryComponent();
    Bundle.BundleEntryComponent comp5 = new Bundle.BundleEntryComponent();

    ValueSet valueSet = new ValueSet();
    valueSet.setId(UUID.randomUUID().toString());
    PlanDefinition planDefinition = new PlanDefinition();
    planDefinition.setId(UUID.randomUUID().toString());
    Extension extension1 = new Extension();
    extension1.setUrl(
        "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-receiver-endpoint");

    Extension extension2 = new Extension();
    extension2.setUrl("http://hl7.org/fhir/StructureDefinition/variable");
    Expression expression = new Expression();
    expression.setName("Example");
    expression.setExpression("ExpressionExample");
    extension2.setValue(expression);
    Extension extension3 = new Extension();
    extension3.setUrl(
        "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-fhirquerypattern-extension");
    extension3.setValue(new StringType("FHIRQueryPattern"));
    Extension extension4 = new Extension();
    extension4.setUrl("http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-relateddata-extension");
    Extension extension5 = new Extension();
    extension5.setUrl(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL);
    Expression expressionTextCql = new Expression();
    expressionTextCql.setLanguage("text/cql");
    extension5.setValue(expressionTextCql);

    UriType uriType = new UriType();
    uriType.setId(UUID.randomUUID().toString());
    extension1.setValue(uriType);

    List<Extension> extensionList = new ArrayList<>();
    extensionList.add(extension1);
    extensionList.add(extension2);
    extensionList.add(extension3);
    extensionList.add(extension4);
    extensionList.add(extension5);

    planDefinition.setExtension(extensionList);

    PlanDefinition.PlanDefinitionActionComponent planDefinitionActionComponent =
        new PlanDefinition.PlanDefinitionActionComponent();
    CodeableConcept codeableConcept = new CodeableConcept();
    List<CodeableConcept> cdslist = new ArrayList<>();

    Coding coding = new Coding();
    coding.setSystem("http://terminology.hl7.org/CodeSystem/plan-definition-type");
    coding.setDisplay("Action");
    coding.setCode("execute-reporting-workflow");

    codeableConcept.addCoding(coding);

    cdslist.add(codeableConcept);
    planDefinitionActionComponent.setCode(cdslist);

    List<PlanDefinition.PlanDefinitionActionComponent> planDefinitionActionComponentList =
        new ArrayList<>();
    planDefinitionActionComponentList.add(planDefinitionActionComponent);

    TriggerDefinition triggerDefinition = new TriggerDefinition();
    triggerDefinition.setType(TriggerDefinition.TriggerType.NAMEDEVENT);
    List<TriggerDefinition> triggerDefinitions = new ArrayList<>();
    triggerDefinitions.add(triggerDefinition);
    planDefinitionActionComponent.setTrigger(triggerDefinitions);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setId(UUID.randomUUID().toString());
    dataRequirement.setType("AllergyIntolerance");

    dataRequirement.setExtension(extensionList);
    List<DataRequirement> dataRequirementList = new ArrayList<>();
    dataRequirementList.add(dataRequirement);
    planDefinitionActionComponent.setInput(dataRequirementList);
    planDefinitionActionComponent.setOutput(dataRequirementList);

    PlanDefinition.PlanDefinitionActionConditionComponent planDefinitionActionConditionComponent =
        new PlanDefinition.PlanDefinitionActionConditionComponent();

    PlanDefinition.PlanDefinitionActionConditionComponent planDefinitionActionConditionComponent2 =
        new PlanDefinition.PlanDefinitionActionConditionComponent();
    planDefinitionActionConditionComponent.setId(UUID.randomUUID().toString());
    Expression expression1 = new Expression();
    expression1.setLanguage("text/cql.expression");
    planDefinitionActionConditionComponent.setExpression(expression1);

    List<Extension> extensions = new ArrayList<>();
    Extension extension = new Extension();
    extension.setUrl(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL);
    extensions.add(extension);

    planDefinitionActionConditionComponent.setExtension(extensions);

    Expression expression2 = new Expression();
    expression2.setExtension(extensionList);
    expression2.setLanguage("text/fhirpath");
    planDefinitionActionConditionComponent2.setExpression(expression2);

    List<PlanDefinition.PlanDefinitionActionConditionComponent> conds = new ArrayList<>();
    conds.add(planDefinitionActionConditionComponent);
    conds.add(planDefinitionActionConditionComponent2);

    planDefinitionActionComponent.setCondition(conds);
    PlanDefinition.PlanDefinitionActionRelatedActionComponent
        planDefinitionActionRelatedActionComponent =
            new PlanDefinition.PlanDefinitionActionRelatedActionComponent();
    planDefinitionActionRelatedActionComponent.setRelationship(BEFORESTART);
    List<PlanDefinition.PlanDefinitionActionRelatedActionComponent> relatedAction =
        new ArrayList<>();
    relatedAction.add(planDefinitionActionRelatedActionComponent);
    planDefinitionActionComponent.setRelatedAction(relatedAction);

    UriType uriType1 = new UriType();
    uriType.setId(UUID.randomUUID().toString());
    planDefinitionActionComponent.setDefinition(uriType1);

    ReflectionTestUtils.setField(karParser, "cqlEnabled", true);
    ReflectionTestUtils.setField(karParser, "fhirpathEnabled", true);

    Library library = new Library();
    library.setId("rctc");
    library.setMeta(meta);

    Library library1 = new Library();
    library1.setId("lib456");
    library1.setMeta(meta1);

    Patient patient = new Patient();
    patient.setId("1");

    comp1.setResource(valueSet);
    comp2.setResource(planDefinition);
    comp3.setResource(library);
    comp4.setResource(library1);
    comp5.setResource(patient);

    bundleEntryComponentList.add(comp1);
    bundleEntryComponentList.add(comp2);
    bundleEntryComponentList.add(comp3);
    bundleEntryComponentList.add(comp4);
    bundleEntryComponentList.add(comp5);

    List<CanonicalType> profiles1 = new ArrayList<>();

    CanonicalType canonicalType1 = new CanonicalType();
    canonicalType1.setValue(
        "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-specification-library");

    profiles1.add(canonicalType1);
    meta.setProfile(profiles1);
    meta.setVersionId("3.1");

    List<CanonicalType> profiles2 = new ArrayList<>();
    CanonicalType canonicalType2 = new CanonicalType();
    canonicalType2.setValue(
        "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-specification-library");
    library1.setVersion("3.2");
    profiles2.add(canonicalType2);
    meta1.setProfile(profiles2);

    planDefinition.setLibrary(profiles1);
    planDefinition.setAction(planDefinitionActionComponentList);
    Identifier identifier = new Identifier();
    identifier.setSystem("RCTC_DEFAULT_SYSTEM");
    identifier.setValue("RCTC_DEFAULT_VALUE");
    List<Identifier> ids = new ArrayList<>();
    ids.add(identifier);
    library.setIdentifier(ids);

    karBundle.setEntry(bundleEntryComponentList);

    ReflectionTestUtils.setField(karParser, "karDirectory", karDirectory);
    when(utils.readKarFromFile(any())).thenReturn(karBundle);

    karParser.initializeRepository();

    assertNotNull(karParser.localKars);
    assertFalse(karParser.localKars.isEmpty());
    assertTrue(karParser.localKars.containsKey("http://localhost"));
    assertNotNull(karParser.localKars.get("http://localhost"));
    assertEquals(
        "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-specification-library",
        profiles1.get(0).getValue());
    assertEquals(profiles1, meta.getProfile());
    assertEquals("3.1", meta.getVersionId());
  }

  @Test
  public void testpopulateCheckResponseAction() {
    SubmitReport baseAction = new SubmitReport();
    KnowledgeArtifact art = new KnowledgeArtifact();
    PlanDefinition plan = new PlanDefinition();
    karParser.populateCheckResponseAction(baseAction, art, plan);
    assertEquals(1, art.getFirstLevelActions().size());
  }
}
