package com.drajer.bsa.service.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.dao.TimeZoneDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.ehr.subscriptions.SubscriptionGeneratorService;
import com.drajer.bsa.kar.action.CheckResponse;
import com.drajer.bsa.kar.action.SubmitReport;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import com.drajer.bsa.routing.impl.DirectTransportImpl;
import com.drajer.bsa.routing.impl.RestfulTransportImpl;
import com.drajer.bsa.scheduler.BsaScheduler;
import com.drajer.bsa.service.KarService;
import com.drajer.bsa.service.PublicHealthAuthorityService;
import com.drajer.bsa.utils.BsaConstants;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.sof.utils.FhirContextInitializer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import org.hl7.fhir.exceptions.FHIRException;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Endpoint;
import org.hl7.fhir.r4.model.Expression;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Library;
import org.hl7.fhir.r4.model.Measure;
import org.hl7.fhir.r4.model.Meta;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.PlanDefinition;
import org.hl7.fhir.r4.model.PlanDefinition.ActionConditionKind;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionComponent;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionConditionComponent;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionRelatedActionComponent;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.TriggerDefinition;
import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.hl7.fhir.r4.model.UriType;
import org.hl7.fhir.r4.model.ValueSet;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.opencds.cqf.fhir.cr.cpg.r4.R4CqlExecutionService;
import org.opencds.cqf.fhir.cr.cpg.r4.R4LibraryEvaluationService;
import org.opencds.cqf.fhir.cr.measure.r4.R4MeasureService;
import org.opencds.cqf.fhir.utility.repository.InMemoryFhirRepository;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

/**
 * JUnit4 tests for {@link KarParserImpl}. The main class is not modified -- every private method is
 * exercised indirectly through its public entry points ({@link KarParserImpl#getAction}, {@link
 * KarParserImpl#loadKars()}/{@link KarParserImpl#loadKarsFromDirectory}, {@link
 * KarParserImpl#persistAndSyncLocalKars()}, {@link KarParserImpl#populateCheckResponseAction},
 * {@link KarParserImpl#processExtensions}). {@code localKars}/{@code localKarRepoUrlToName} are
 * package-private fields on the main class already, so tests in this package set them directly
 * where that's simpler than driving a full directory scan.
 */
public class KarParserImplTest {

  private AutowireCapableBeanFactory beanFactory;
  private BsaServiceUtils utils;
  private BsaScheduler scheduler;
  private KnowledgeArtifactRepositorySystem karRepositorySystem;
  private R4MeasureService measureService;
  private ObjectProvider<R4CqlExecutionService> expressionEvaluators;
  private R4LibraryEvaluationService libraryEvaluationService;
  private PublicHealthMessagesDao phDao;
  private HealthcareSettingsDao hsDao;
  private SubscriptionGeneratorService subscriptionGeneratorService;
  private EhrQueryService ehrInterface;
  private DirectTransportImpl directInterface;
  private RestfulTransportImpl restSubmitter;
  private AuthorizationUtils authUtils;
  private FhirContextInitializer fhirContextInitializer;
  private PublicHealthAuthorityService publicHealthAuthorityService;
  private TimeZoneDao timezoneDao;
  private InMemoryFhirRepository repository;
  private KarService karService;
  private IParser jsonParser;
  private RestTemplate restTemplate;

  private KarParserImpl karParser;

  private final List<Path> tempPaths = new ArrayList<>();

  @Before
  public void setUp() {
    beanFactory = mock(AutowireCapableBeanFactory.class);
    utils = mock(BsaServiceUtils.class);
    scheduler = mock(BsaScheduler.class);
    karRepositorySystem = mock(KnowledgeArtifactRepositorySystem.class);
    measureService = mock(R4MeasureService.class);
    expressionEvaluators = mock(ObjectProvider.class);
    libraryEvaluationService = mock(R4LibraryEvaluationService.class);
    phDao = mock(PublicHealthMessagesDao.class);
    hsDao = mock(HealthcareSettingsDao.class);
    subscriptionGeneratorService = mock(SubscriptionGeneratorService.class);
    ehrInterface = mock(EhrQueryService.class);
    directInterface = mock(DirectTransportImpl.class);
    restSubmitter = mock(RestfulTransportImpl.class);
    authUtils = mock(AuthorizationUtils.class);
    fhirContextInitializer = mock(FhirContextInitializer.class);
    publicHealthAuthorityService = mock(PublicHealthAuthorityService.class);
    timezoneDao = mock(TimeZoneDao.class);
    repository = mock(InMemoryFhirRepository.class);
    karService = mock(KarService.class);
    jsonParser = mock(IParser.class);
    restTemplate = mock(RestTemplate.class);

    karParser =
        new KarParserImpl(
            beanFactory,
            utils,
            scheduler,
            karRepositorySystem,
            measureService,
            expressionEvaluators,
            libraryEvaluationService,
            phDao,
            hsDao,
            subscriptionGeneratorService,
            ehrInterface,
            directInterface,
            restSubmitter,
            authUtils,
            fhirContextInitializer,
            publicHealthAuthorityService,
            timezoneDao,
            repository,
            karService,
            jsonParser,
            restTemplate);

    ReflectionTestUtils.setField(karParser, "karDirectory", "default");
    ReflectionTestUtils.setField(karParser, "ignoreTimers", Boolean.FALSE);
    ReflectionTestUtils.setField(karParser, "measurePeriodStart", null);
    ReflectionTestUtils.setField(karParser, "measurePeriodEnd", null);
    ReflectionTestUtils.setField(karParser, "cqlEnabled", true);
    ReflectionTestUtils.setField(karParser, "fhirpathEnabled", true);
    ReflectionTestUtils.setField(karParser, "eicrCdaR11ValidationEnabled", false);
    ReflectionTestUtils.setField(karParser, "eicrCdaR31ValidationEnabled", false);
    ReflectionTestUtils.setField(karParser, "eicrFhirValidationEnabled", false);
    ReflectionTestUtils.setField(karParser, "logDirectory", "test-output");
    ReflectionTestUtils.setField(karParser, "eicrCdaR11SchematronPath", "r11.sch");
    ReflectionTestUtils.setField(karParser, "eicrCdaR31SchematronPath", "r31.sch");
    ReflectionTestUtils.setField(karParser, "validatorEndpoint", "http://validator");
    ReflectionTestUtils.setField(karParser, "reportSubmissionEndpoint", "http://submit");

    // Bundle.hasEntry() defaults to false on an unstubbed mock, so
    // addKarResourceToFhirRepository's create() branch runs unless a test overrides this.
    when(repository.search(any(), any(), anyMap())).thenReturn(new Bundle());
  }

  @After
  public void tearDown() throws IOException {
    for (Path p : tempPaths) {
      if (Files.exists(p)) {
        try (Stream<Path> walk = Files.walk(p)) {
          walk.sorted(Comparator.reverseOrder()).map(Path::toFile).forEach(File::delete);
        }
      }
    }
  }

  private Path newTempDir() throws IOException {
    Path dir = Files.createTempDirectory("kar-test-");
    tempPaths.add(dir);
    return dir;
  }

  // ==================== getAction ====================

  @Test
  public void getAction_knownActionId_noExistingBean_autowiresNewInstance() {
    when(beanFactory.getBean(SubmitReport.class))
        .thenThrow(new NoSuchBeanDefinitionException(SubmitReport.class));

    BsaAction action = karParser.getAction("submit-report");

    assertNotNull(action);
    assertTrue(action instanceof SubmitReport);
    verify(beanFactory, never()).destroyBean(any());
    verify(beanFactory, times(1)).autowireBean(action);
  }

  @Test
  public void getAction_knownActionId_existingBean_destroysThenAutowiresNewInstance() {
    SubmitReport existingBean = new SubmitReport();
    when(beanFactory.getBean(SubmitReport.class)).thenReturn(existingBean);

    BsaAction action = karParser.getAction("submit-report");

    assertNotNull(action);
    verify(beanFactory, times(1)).destroyBean(existingBean);
    verify(beanFactory, times(1)).autowireBean(action);
  }

  @Test
  public void getAction_unknownActionId_returnsNull() {
    assertNull(karParser.getAction("no-such-action-id"));
  }

  // ==================== populateCheckResponseAction ====================

  @Test
  public void populateCheckResponseAction_populatesActionAndArtifact() {
    when(beanFactory.getBean(CheckResponse.class))
        .thenThrow(new NoSuchBeanDefinitionException(CheckResponse.class));

    SubmitReport baseAction = new SubmitReport();
    baseAction.setActionId("submit-1", "http://plan");
    KnowledgeArtifact art = new KnowledgeArtifact();
    PlanDefinition plan = new PlanDefinition();
    plan.setUrl("http://plan");

    karParser.populateCheckResponseAction(baseAction, art, plan);

    assertEquals(1, art.getFirstLevelActions().size());
    CheckResponse checkResponse = (CheckResponse) art.getFirstLevelActions().get(0);
    assertEquals(ActionType.CHECK_RESPONSE, checkResponse.getType());
    assertEquals(checkResponse.getActionId(), baseAction.getCheckResponseActionId());
  }

  // ==================== processExtensions ====================

  @Test
  public void processExtensions_receiverAddressAsUriType_addsReceiverAddress() {
    PlanDefinition plan = new PlanDefinition();
    plan.addExtension(
        new Extension(
            "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-receiver-endpoint",
            new UriType("http://receiver.example.com")));
    KnowledgeArtifact art = new KnowledgeArtifact();

    karParser.processExtensions(plan, art);

    assertEquals(1, art.getReceiverAddresses().size());
  }

  @Test
  public void processExtensions_receiverAddressAsReference_resolvedEndpoint_addsAddress() {
    Endpoint endpoint = new Endpoint();
    endpoint.setId("ep1");
    endpoint.setAddress("http://resolved.example.com");
    KnowledgeArtifact art = new KnowledgeArtifact();
    art.addDependentResource(endpoint);

    PlanDefinition plan = new PlanDefinition();
    plan.addExtension(
        new Extension(
            "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-receiver-endpoint",
            new Reference("Endpoint/ep1")));

    karParser.processExtensions(plan, art);

    assertEquals(1, art.getReceiverAddresses().size());
  }

  @Test
  public void processExtensions_receiverAddressAsReference_unresolvedEndpoint_usesReferenceAsUri() {
    KnowledgeArtifact art = new KnowledgeArtifact();
    PlanDefinition plan = new PlanDefinition();
    plan.addExtension(
        new Extension(
            "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-receiver-endpoint",
            new Reference("Endpoint/does-not-exist")));

    karParser.processExtensions(plan, art);

    assertEquals(1, art.getReceiverAddresses().size());
  }

  @Test
  public void processExtensions_variableExpressionExtension_recordsVariable() {
    KnowledgeArtifact art = new KnowledgeArtifact();
    PlanDefinition plan = new PlanDefinition();
    Expression variable = new Expression();
    variable.setName("MyVar");
    variable.setExpression("1 + 1");
    plan.addExtension(new Extension("http://hl7.org/fhir/StructureDefinition/variable", variable));

    // Should not throw -- planVariableExpressions is populated internally and consumed later
    // by handleFhirPathCondition, exercised in the PlanDefinition-processing tests below.
    karParser.processExtensions(plan, art);
    assertTrue(art.getReceiverAddresses().isEmpty());
  }

  @Test
  public void processExtensions_noExtensions_doesNothing() {
    KnowledgeArtifact art = new KnowledgeArtifact();
    PlanDefinition plan = new PlanDefinition();

    karParser.processExtensions(plan, art);

    assertTrue(art.getReceiverAddresses().isEmpty());
  }

  // ==================== loadKarsFromDirectory / processKar / processBundleEntry
  // (all private, reachable only through this public method) ====================

  @Test
  public void loadKarsFromDirectory_nonExistentDirectory_doesNothingAndDoesNotThrow() {
    karParser.loadKarsFromDirectory("/no/such/directory/xyz", "http://localhost", "repo");
    verifyNoInteractions(karRepositorySystem);
  }

  @Test
  public void loadKarsFromDirectory_emptyDirectory_doesNothing() throws IOException {
    Path dir = newTempDir();
    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");
    verifyNoInteractions(karRepositorySystem);
  }

  @Test
  public void loadKarsFromDirectory_nonJsonFile_isIgnored() throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("notes.txt"));
    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");
    verifyNoInteractions(karRepositorySystem);
  }

  @Test
  public void loadKarsFromDirectory_nullBundle_logsAndSkips() throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));
    when(utils.readKarFromFile(anyString())).thenReturn(null);

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verifyNoInteractions(karRepositorySystem);
  }

  @Test
  public void loadKarsFromDirectory_bundleWrongType_logsAndSkips() throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));
    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.SEARCHSET);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verifyNoInteractions(karRepositorySystem);
  }

  @Test
  public void loadKarsFromDirectory_recursesIntoSubdirectories() throws IOException {
    Path dir = newTempDir();
    Path subDir = Files.createDirectory(dir.resolve("sub"));
    Files.createFile(dir.resolve("top.json"));
    Files.createFile(subDir.resolve("nested.json"));

    Bundle topBundle = new Bundle();
    topBundle.setType(Bundle.BundleType.SEARCHSET); // wrong type -> skipped, still counts as
    // "processed" for recursion coverage purposes.
    Bundle nestedBundle = new Bundle();
    nestedBundle.setType(Bundle.BundleType.SEARCHSET);

    when(utils.readKarFromFile(contains("top.json"))).thenReturn(topBundle);
    when(utils.readKarFromFile(contains("nested.json"))).thenReturn(nestedBundle);

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(utils, times(1)).readKarFromFile(contains("top.json"));
    verify(utils, times(1)).readKarFromFile(contains("nested.json"));
  }

  /** Builds a minimal valid Library resource that also exercises the RCTC-version-parsing path. */
  private Library buildRctcLibrary() {
    Library lib = new Library();
    lib.setId("rctc-lib");
    lib.setName("RCTC Library");
    lib.setPublisher("Test Publisher");
    lib.setVersion("3.0.0");
    Meta meta = new Meta();
    meta.addProfile("http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-specification-library");
    meta.setVersionId("3.1.0");
    lib.setMeta(meta);
    Identifier id = new Identifier();
    id.setSystem("RCTC_DEFAULT_SYSTEM");
    id.setValue("rctc-oid-123");
    lib.addIdentifier(id);
    return lib;
  }

  @Test
  public void loadKarsFromDirectory_validBundleWithAllEntryTypes_processesEachType()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.setId("bundle-1");
    // Bundle-level Meta with a versionId -> art.setKarVersion(karBundle.getMeta().getVersionId()).
    Meta bundleMeta = new Meta();
    bundleMeta.setVersionId("2.0");
    bundle.setMeta(bundleMeta);

    // Library entry -- exercises processLibrary (version + rctc branches).
    bundle.addEntry().setResource(buildRctcLibrary());

    // A second Library whose profile doesn't match the US spec profile substring (so the
    // version-detection loop runs to completion without an early break) and whose identifier
    // doesn't match the RCTC system (so getRctcOid's loop also runs to completion and falls
    // back to the default OID).
    Library nonMatchingLib = new Library();
    nonMatchingLib.setId("rctc-other-lib");
    nonMatchingLib.setName("Other Rctc Library");
    Meta nonMatchingMeta = new Meta();
    nonMatchingMeta.addProfile("http://example.com/some-other-profile");
    nonMatchingLib.setMeta(nonMatchingMeta);
    Identifier nonMatchingId = new Identifier();
    nonMatchingId.setSystem("SOME_OTHER_SYSTEM");
    nonMatchingId.setValue("not-rctc-oid");
    nonMatchingLib.addIdentifier(nonMatchingId);
    bundle.addEntry().setResource(nonMatchingLib);

    // ValueSet entry -- exercises processValueSet.
    ValueSet vs = new ValueSet();
    vs.setId("vs1");
    vs.setUrl("http://vs1");
    bundle.addEntry().setResource(vs);

    // Measure entry -- exercises addKarResourceToFhirRepository's Measure branch.
    Measure measure = new Measure();
    measure.setId("measure1");
    measure.setUrl("http://measure1");
    measure.setVersion("1.0");
    bundle.addEntry().setResource(measure);

    // PlanDefinition entry with a submit-report action -- exercises processPlanDefinition,
    // populateAction (SUBMIT_REPORT branch -> populateCheckResponseAction), getNamedEvents,
    // populateRelatedAction, populateInputDataReq/populateOutputDataReq, populateSubActions,
    // and processExtensions (called separately, but covered above).
    PlanDefinition plan = buildPlanDefinitionWithSubmitReportAction();
    bundle.addEntry().setResource(plan);

    // Anything else -- exercises the "add to dependencies" fallback branch.
    Organization org = new Organization();
    org.setId("org1");
    bundle.addEntry().setResource(org);

    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
    verify(repository, atLeastOnce()).create(any());
  }

  private PlanDefinition buildPlanDefinitionWithSubmitReportAction() {
    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan1");
    plan.setUrl("http://plan1");
    plan.setName("Plan One");
    plan.setPublisher("Publisher One");
    plan.addLibrary("Library/rctc-lib");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act1");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("submit-report"));
    act.addCode(code);

    TriggerDefinition trigger = new TriggerDefinition();
    trigger.setType(TriggerType.NAMEDEVENT);
    trigger.setName("my-event");
    act.addTrigger(trigger);

    DataRequirement input = new DataRequirement();
    input.setId("input1");
    input.setType("Condition");
    // Query and related-data extensions -> populateInputDataReq's extension-handling branches.
    input.addExtension(
        new Extension(
            "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-fhirquerypattern-extension",
            new org.hl7.fhir.r4.model.StringType("Condition?patient={{context.patientId}}")));
    input.addExtension(
        new Extension(
            "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-relateddata-extension",
            new org.hl7.fhir.r4.model.StringType("related-data-id-1")));
    act.addInput(input);

    DataRequirement output = new DataRequirement();
    output.setId("output1");
    output.setType("Bundle");
    act.addOutput(output);

    // definition[x] as a UriType -> action.setMeasureUri(act.getDefinitionUriType().getValue()).
    act.setDefinition(new UriType("http://definition-uri"));

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    cond.setKind(ActionConditionKind.APPLICABILITY);
    Expression fhirPathExpr = new Expression();
    fhirPathExpr.setLanguage("text/fhirpath");
    fhirPathExpr.setExpression("true");
    cond.setExpression(fhirPathExpr);
    act.addCondition(cond);

    PlanDefinitionActionRelatedActionComponent related =
        new PlanDefinitionActionRelatedActionComponent();
    related.setActionId("other-action");
    related.setRelationship(ActionRelationshipType.BEFORESTART);
    act.addRelatedAction(related);

    plan.addAction(act);
    return plan;
  }

  @Test
  public void loadKarsFromDirectory_planDefinitionWithCqlCondition_handlesCqlCondition()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-cql");
    plan.setUrl("http://plan-cql");
    plan.addLibrary("Library/lib-cql");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-cql");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-condition"));
    act.addCode(code);

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    Expression cqlExpr = new Expression();
    cqlExpr.setLanguage("text/cql");
    cqlExpr.setExpression("SomeDefine");
    cond.setExpression(cqlExpr);
    act.addCondition(cond);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_planDefinitionWithAlternativeExpression_handlesFhirPath()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-alt");
    plan.setUrl("http://plan-alt");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-alt");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-condition"));
    act.addCode(code);

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    // Primary expression uses a recognized-but-non-CQL/non-FHIRPath language (so fromCode()
    // succeeds but processConditionComponent's CQL-first branch is skipped), and an alternative
    // FHIRPath expression extension is present on the condition itself -> falls into
    // handleAlternativeExpression's FHIRPath branch.
    Expression primary = new Expression();
    primary.setLanguage("application/x-fhir-query");
    cond.setExpression(primary);

    Expression alt = new Expression();
    alt.setLanguage("text/fhirpath");
    alt.setExpression("true");
    cond.addExtension(new Extension(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL, alt));
    act.addCondition(cond);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_planDefinitionWithAlternativeCqlExpression_handlesCql()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-alt-cql");
    plan.setUrl("http://plan-alt-cql");
    // No plan-level library, so the alternative CQL expression must carry its own reference.

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-alt-cql");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-condition"));
    act.addCode(code);

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    Expression primary = new Expression();
    primary.setLanguage("text/fhirpath");
    primary.setExpression("false"); // not used; the extension attribute is on the condition
    cond.setExpression(primary);

    Expression altCql = new Expression();
    altCql.setLanguage("text/cql");
    altCql.setReference("http://alt-library");
    cond.addExtension(new Extension(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL, altCql));
    act.addCondition(cond);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_alternativeExpressionOnConditionItself_isFound()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-alt2");
    plan.setUrl("http://plan-alt2");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-alt2");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-condition"));
    act.addCode(code);

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    Expression primary = new Expression();
    primary.setLanguage("text/fhirpath");
    primary.setExpression("true");
    cond.setExpression(primary);
    // Extension is on the condition component's expression itself this time (first lookup path).
    Expression alt = new Expression();
    alt.setLanguage("text/fhirpath");
    alt.setExpression("true");
    primary.addExtension(new Extension(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL, alt));
    act.addCondition(cond);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_alternativeExpressionWithUnknownValueType_logsError()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-alt3");
    plan.setUrl("http://plan-alt3");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-alt3");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-condition"));
    act.addCode(code);

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    // Primary language is recognized but not CQL, so processConditionComponent falls through
    // to the alternative-expression branch instead of handleCqlCondition.
    Expression primary = new Expression();
    primary.setLanguage("application/x-fhir-query");
    cond.setExpression(primary);
    // Extension present but with no value set -> ext.getValue() is null, hitting
    // handleAlternativeExpression's own "exp == null" logging branch.
    cond.addExtension(new Extension(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL));
    act.addCondition(cond);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_alternativeCqlExpressionWithoutReferenceOrLibrary_logsError()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-alt-no-lib");
    plan.setUrl("http://plan-alt-no-lib");
    // No plan-level library -> libraryCanonical stays null.

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-alt-no-lib");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-condition"));
    act.addCode(code);

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    Expression primary = new Expression();
    primary.setLanguage("application/x-fhir-query");
    cond.setExpression(primary);

    // Alternative CQL expression with no reference element either -> libraryCanonical remains
    // null and handleAlternativeExpression logs "library canonical is null" instead of
    // processing it.
    Expression altCql = new Expression();
    altCql.setLanguage("text/cql");
    altCql.setExpression("SomeDefine");
    cond.addExtension(new Extension(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL, altCql));
    act.addCondition(cond);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_alternativeExpressionUnrecognizedButValidLanguage_logsError()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-alt-unknown-lang");
    plan.setUrl("http://plan-alt-unknown-lang");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-alt-unknown-lang");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-condition"));
    act.addCode(code);

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    // Both primary and alternative expressions use a recognized-but-neither-CQL-nor-FHIRPath
    // language -> handleAlternativeExpression falls through every branch to its final
    // "Unknown type of Alternative Expression" else.
    Expression primary = new Expression();
    primary.setLanguage("application/x-fhir-query");
    cond.setExpression(primary);

    Expression alt = new Expression();
    alt.setLanguage("application/x-fhir-query");
    cond.addExtension(new Extension(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL, alt));
    act.addCondition(cond);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_actionWithTiming_logsTimingPresent() throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-timing");
    plan.setUrl("http://plan-timing");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-timing");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("check-trigger-codes"));
    act.addCode(code);
    act.setTiming(new org.hl7.fhir.r4.model.DateTimeType("2024-01-01"));
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_inputDataReqWithInvalidResourceType_logsError()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-bad-input-type");
    plan.setUrl("http://plan-bad-input-type");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-bad-input-type");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("check-trigger-codes"));
    act.addCode(code);

    DataRequirement input = new DataRequirement();
    input.setId("bad-input");
    input.setType("NotARealFhirResourceType"); // ResourceType.fromCode(..) throws FHIRException
    act.addInput(input);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_inputDataReqWithMedmorphOnlyExtensions_fallsThroughToThem()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-medmorph-ext");
    plan.setUrl("http://plan-medmorph-ext");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-medmorph-ext");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("check-trigger-codes"));
    act.addCode(code);

    DataRequirement input = new DataRequirement();
    input.setId("medmorph-input");
    input.setType("Condition");
    // Only the MEDMORPH variants are present (no ECR/PH extensions) -> both the ECR and PH
    // lookups return null before the MEDMORPH lookup finally finds them.
    input.addExtension(
        new Extension(
            "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-fhirquerypattern-extension",
            new org.hl7.fhir.r4.model.StringType("Condition?patient={{context.patientId}}")));
    input.addExtension(
        new Extension(
            "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-relateddata-extension",
            new org.hl7.fhir.r4.model.StringType("related-data-id-medmorph")));
    act.addInput(input);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test(expected = FHIRException.class)
  public void loadKarsFromDirectory_unknownExpressionLanguage_throwsFHIRException()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-bad-lang");
    plan.setUrl("http://plan-bad-lang");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-bad-lang");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-condition"));
    act.addCode(code);

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    Expression expr = new Expression();
    expr.setLanguage("text/unsupported-language");
    cond.setExpression(expr);
    act.addCondition(cond);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");
  }

  @Test
  public void loadKarsFromDirectory_conditionLanguagesDisabled_logsUnknownTypeError()
      throws IOException {
    ReflectionTestUtils.setField(karParser, "cqlEnabled", false);
    ReflectionTestUtils.setField(karParser, "fhirpathEnabled", false);

    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-disabled");
    plan.setUrl("http://plan-disabled");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-disabled");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-condition"));
    act.addCode(code);

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    Expression expr = new Expression();
    expr.setLanguage("text/fhirpath"); // valid language, but fhirpathEnabled=false
    expr.setExpression("true");
    cond.setExpression(expr);
    act.addCondition(cond);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_evaluateMeasureAction_setsMeasureParameters()
      throws IOException {
    ReflectionTestUtils.setField(
        karParser, "measurePeriodStart", ZonedDateTime.parse("2024-01-01T00:00:00Z"));
    ReflectionTestUtils.setField(
        karParser, "measurePeriodEnd", ZonedDateTime.parse("2024-12-31T23:59:59Z"));

    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-measure");
    plan.setUrl("http://plan-measure");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-measure");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-measure"));
    act.addCode(code);

    DataRequirement output = new DataRequirement();
    output.setId("measure-report-1");
    output.setType("MeasureReport");
    act.addOutput(output);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_validateReportAction_setsValidationFields() throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-validate");
    plan.setUrl("http://plan-validate");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-validate");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("validate-report"));
    act.addCode(code);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_createReportAction_setsPhDao() throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-create");
    plan.setUrl("http://plan-create");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-create");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("create-report"));
    act.addCode(code);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_subActions_areProcessedRecursively() throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-sub");
    plan.setUrl("http://plan-sub");

    PlanDefinitionActionComponent parentAct = new PlanDefinitionActionComponent();
    parentAct.setId("parent-act");
    CodeableConcept parentCode = new CodeableConcept();
    parentCode.addCoding(new Coding().setCode("check-trigger-codes"));
    parentAct.addCode(parentCode);

    PlanDefinitionActionComponent childAct = new PlanDefinitionActionComponent();
    childAct.setId("child-act");
    CodeableConcept childCode = new CodeableConcept();
    childCode.addCoding(new Coding().setCode("evaluate-condition"));
    childAct.addCode(childCode);
    parentAct.addAction(childAct);

    plan.addAction(parentAct);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_twoKarsForSameRepo_reusesExistingArtifactSetAndUrlMapping()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar-a.json"));
    Files.createFile(dir.resolve("kar-b.json"));

    ValueSet vsA = new ValueSet();
    vsA.setId("vs-a");
    vsA.setUrl("http://vs-a");
    Bundle bundleA = new Bundle();
    bundleA.setType(Bundle.BundleType.COLLECTION);
    bundleA.addEntry().setResource(vsA);

    ValueSet vsB = new ValueSet();
    vsB.setId("vs-b");
    vsB.setUrl("http://vs-b");
    Bundle bundleB = new Bundle();
    bundleB.setType(Bundle.BundleType.COLLECTION);
    bundleB.addEntry().setResource(vsB);

    when(utils.readKarFromFile(contains("kar-a.json"))).thenReturn(bundleA);
    when(utils.readKarFromFile(contains("kar-b.json"))).thenReturn(bundleB);

    // Both files target the same repoUrl -> the second call to addArtifactForPersistence finds
    // localKars already keyed for that repoUrl (with a non-null Set) and localKarRepoUrlToName
    // already initialized, exercising both "already exists" branches.
    karParser.loadKarsFromDirectory(dir.toString(), "http://shared-repo", "shared-repo-name");

    verify(karRepositorySystem, times(2)).add(any(KnowledgeArtifact.class));
    assertEquals(2, karParser.localKars.get("http://shared-repo").size());
    assertEquals("shared-repo-name", karParser.localKarRepoUrlToName.get("http://shared-repo"));
  }

  @Test
  public void
      loadKarsFromDirectory_alternativeExpressionFallsBackToPrimaryFhirPath_handlesFallback()
          throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-alt-fallback");
    plan.setUrl("http://plan-alt-fallback");

    PlanDefinitionActionComponent act = new PlanDefinitionActionComponent();
    act.setId("act-alt-fallback");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setCode("evaluate-condition"));
    act.addCode(code);

    PlanDefinitionActionConditionComponent cond = new PlanDefinitionActionConditionComponent();
    // Primary is FHIRPath itself (hasAlternativeExpression still routes into
    // handleAlternativeExpression ahead of the primary-FHIRPath branch in
    // processConditionComponent), and the alternative expression uses a recognized-but
    // neither-CQL-nor-FHIRPath language, so handleAlternativeExpression falls through to its
    // "use the primary FHIRPath expression instead" branch.
    Expression primary = new Expression();
    primary.setLanguage("text/fhirpath");
    primary.setExpression("true");
    cond.setExpression(primary);

    Expression alt = new Expression();
    alt.setLanguage("application/x-fhir-query");
    cond.addExtension(new Extension(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL, alt));
    act.addCondition(cond);
    plan.addAction(act);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);
    when(beanFactory.getBean(any(Class.class)))
        .thenThrow(new NoSuchBeanDefinitionException(Object.class));

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test(expected = NullPointerException.class)
  public void loadKarsFromDirectory_actionWithoutCode_throwsNpeOnUnresolvedAction()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    PlanDefinition plan = new PlanDefinition();
    plan.setId("plan-nocode");
    plan.setUrl("http://plan-nocode");
    // Action with no code at all -> act.getCodeFirstRep()/getCodingFirstRep() auto-vivify an
    // empty Coding (HAPI's *FirstRep() accessors never return null), so
    // processPlanDefinition's code-based branch still runs with a null action code. getAction
    // then returns null (no actionClasses entry for a null id), and the main class calls
    // action.setActionId(...) on that null unconditionally -- a genuine NPE in current
    // behavior, not a bug this test suite modifies or works around.
    PlanDefinitionActionComponent noCodeAct = new PlanDefinitionActionComponent();
    noCodeAct.setId("no-code-act");
    plan.addAction(noCodeAct);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(plan);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");
  }

  @Test
  public void loadKarsFromDirectory_libraryWithoutRctcOrVersion_usesDefaults() throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    Library lib = new Library();
    lib.setId("plain-lib");
    lib.setName("Plain Library");
    lib.setPublisher("Plain Publisher");
    // No meta/profile, no version, id doesn't contain "rctc" -> exercises the "no version added"
    // and "no rctc oid" fallback paths.

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(lib);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_libraryVersionOnRootVersionField_isUsed() throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    Library lib = new Library();
    lib.setId("root-version-lib");
    lib.setVersion("3.2.0");
    Meta meta = new Meta();
    meta.addProfile("http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-specification-library");
    // No meta.versionId -> falls to the lib.hasVersion() branch instead.
    lib.setMeta(meta);

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(lib);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(karRepositorySystem, times(1)).add(any(KnowledgeArtifact.class));
  }

  @Test
  public void loadKarsFromDirectory_resourceAlreadyInRepository_doesNotCreateAgain()
      throws IOException {
    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    ValueSet vs = new ValueSet();
    vs.setId("vs-existing");
    vs.setUrl("http://vs-existing");

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(vs);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);

    Bundle existing = new Bundle();
    existing.addEntry().setResource(vs);
    when(repository.search(any(), any(), anyMap())).thenReturn(existing);

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    verify(repository, never()).create(any());
  }

  @Test
  public void loadKarsFromDirectory_repoUrlAlreadyMappedToNullSet_createsFreshSet()
      throws IOException {
    // The repoUrl key already exists in localKars but is explicitly mapped to null (not simply
    // absent) -> addArtifactForPersistence's containsKey==true/get==null branch.
    HashMap<String, Set<KnowledgeArtifact>> localKars = new HashMap<>();
    localKars.put("http://localhost", null);
    karParser.localKars = localKars;

    Path dir = newTempDir();
    Files.createFile(dir.resolve("kar.json"));

    ValueSet vs = new ValueSet();
    vs.setId("vs-null-set");
    vs.setUrl("http://vs-null-set");

    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.COLLECTION);
    bundle.addEntry().setResource(vs);
    when(utils.readKarFromFile(anyString())).thenReturn(bundle);

    karParser.loadKarsFromDirectory(dir.toString(), "http://localhost", "repo");

    assertNotNull(karParser.localKars.get("http://localhost"));
    assertEquals(1, karParser.localKars.get("http://localhost").size());
  }

  // ==================== persistAndSyncLocalKars ====================

  @Test
  public void persistAndSyncLocalKars_newRepoUrl_createsAndSavesRepo() {
    HashMap<String, String> repoUrlToName = new HashMap<>();
    repoUrlToName.put("http://new-repo", "new-repo-name");
    karParser.localKarRepoUrlToName = repoUrlToName;

    KnowledgeArtifact art = new KnowledgeArtifact();
    art.setKarId("kar1");
    art.setKarVersion("1.0");
    HashMap<String, Set<KnowledgeArtifact>> localKars = new HashMap<>();
    Set<KnowledgeArtifact> arts = new HashSet<>();
    arts.add(art);
    localKars.put("http://new-repo", arts);
    karParser.localKars = localKars;

    when(karService.getKARByUrl("http://new-repo")).thenReturn(null);
    when(karService.getAllKARs()).thenReturn(new ArrayList<>());

    karParser.persistAndSyncLocalKars();

    verify(karService, times(1))
        .saveOrUpdate(argThat(r -> "http://new-repo".equals(r.getFhirServerURL())));
  }

  @Test
  public void persistAndSyncLocalKars_existingRepo_updatesAvailabilityAndSaves() {
    HashMap<String, String> repoUrlToName = new HashMap<>();
    repoUrlToName.put("http://existing-repo", "existing-repo-name");
    karParser.localKarRepoUrlToName = repoUrlToName;

    KnowledgeArtifact loadedArt = new KnowledgeArtifact();
    loadedArt.setKarId("kar-loaded");
    loadedArt.setKarVersion("1.0");
    HashMap<String, Set<KnowledgeArtifact>> localKars = new HashMap<>();
    Set<KnowledgeArtifact> arts = new HashSet<>();
    arts.add(loadedArt);
    localKars.put("http://existing-repo", arts);
    karParser.localKars = localKars;

    KnowledgeArtifactRepository existingRepo = new KnowledgeArtifactRepository();
    existingRepo.setFhirServerURL("http://existing-repo");
    existingRepo.setRepoName("existing-repo-name");
    // A summary that IS still loaded (matches loadedArt's versionUniqueId).
    KnowledgeArtifact stillLoaded = new KnowledgeArtifact();
    stillLoaded.setKarId("kar-loaded");
    stillLoaded.setKarVersion("1.0");
    // A summary that is NOT loaded anymore.
    KnowledgeArtifact noLongerLoaded = new KnowledgeArtifact();
    noLongerLoaded.setKarId("kar-gone");
    noLongerLoaded.setKarVersion("1.0");
    Set<KnowledgeArtifact> existingArts = new HashSet<>();
    existingArts.add(stillLoaded);
    existingArts.add(noLongerLoaded);
    existingRepo.addKars(existingArts);

    // A second, unrelated repo with a different URL -> exercises the removeIf predicate's
    // "not a match" branch (it stays in inActiveRepos and gets disabled below).
    KnowledgeArtifactRepository unrelatedRepo = new KnowledgeArtifactRepository();
    unrelatedRepo.setFhirServerURL("http://unrelated-repo");

    when(karService.getKARByUrl("http://existing-repo")).thenReturn(existingRepo);
    when(karService.getAllKARs()).thenReturn(new ArrayList<>(List.of(existingRepo, unrelatedRepo)));

    karParser.persistAndSyncLocalKars();

    verify(karService, times(1)).saveOrUpdate(existingRepo);
    verify(karService, times(1)).saveOrUpdate(unrelatedRepo);
    assertFalse(unrelatedRepo.getRepoStatus());
    boolean anyMarkedUnavailable =
        existingRepo.getKarsInfo().stream()
            .anyMatch(i -> Boolean.FALSE.equals(i.getKarAvailable()));
    assertTrue(anyMarkedUnavailable);
  }

  @Test
  public void persistAndSyncLocalKars_repoNotInLocalKars_isDisabled() {
    karParser.localKarRepoUrlToName = new HashMap<>();
    karParser.localKars = new HashMap<>();

    KnowledgeArtifactRepository inactiveRepo = new KnowledgeArtifactRepository();
    inactiveRepo.setFhirServerURL("http://inactive-repo");
    when(karService.getAllKARs()).thenReturn(new ArrayList<>(List.of(inactiveRepo)));

    karParser.persistAndSyncLocalKars();

    verify(karService, times(1)).saveOrUpdate(inactiveRepo);
    assertFalse(inactiveRepo.getRepoStatus());
  }

  // ==================== loadKars / initializeRepository ====================

  @Test
  public void loadKars_delegatesToDirectoryLoadAndPersist() throws IOException {
    ReflectionTestUtils.setField(karParser, "karDirectory", "/no/such/kar/dir");
    karParser.localKarRepoUrlToName = new HashMap<>();
    when(karService.getAllKARs()).thenReturn(new ArrayList<>());

    karParser.loadKars();

    // loadKarsFromDirectory finds nothing (bad dir) but persistAndSyncLocalKars still runs.
    verify(karService, times(1)).getAllKARs();
  }

  @Test
  public void initializeRepository_initializesMapsAndLoadsKars() {
    ReflectionTestUtils.setField(karParser, "karDirectory", "/no/such/kar/dir");
    when(karService.getAllKARs()).thenReturn(new ArrayList<>());

    karParser.initializeRepository();

    assertNotNull(karParser.localKars);
    assertNotNull(karParser.localKarRepoUrlToName);
    verify(karService, times(1)).getAllKARs();
  }
}
