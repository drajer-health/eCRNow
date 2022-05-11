package com.drajer.bsa.service.impl;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.ehr.subscriptions.SubscriptionGeneratorService;
import com.drajer.bsa.kar.action.CheckResponse;
import com.drajer.bsa.kar.action.CreateReport;
import com.drajer.bsa.kar.action.EvaluateMeasure;
import com.drajer.bsa.kar.action.SubmitReport;
import com.drajer.bsa.kar.action.ValidateReport;
import com.drajer.bsa.kar.condition.BsaCqlCondition;
import com.drajer.bsa.kar.condition.BsaFhirPathCondition;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaRelatedAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.routing.impl.DirectTransportImpl;
import com.drajer.bsa.scheduler.BsaScheduler;
import com.drajer.bsa.service.KarParser;
import com.drajer.bsa.service.KarService;
import com.drajer.bsa.utils.BsaConstants;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.bsa.utils.SubscriptionUtils;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import javax.annotation.PostConstruct;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.hl7.fhir.exceptions.FHIRException;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Endpoint;
import org.hl7.fhir.r4.model.Expression;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.PlanDefinition;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionComponent;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionConditionComponent;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionRelatedActionComponent;
import org.hl7.fhir.r4.model.PrimitiveType;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.TriggerDefinition;
import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.hl7.fhir.r4.model.Type;
import org.hl7.fhir.r4.model.UriType;
import org.hl7.fhir.r4.model.ValueSet;
import org.opencds.cqf.cql.evaluator.expression.ExpressionEvaluator;
import org.opencds.cqf.cql.evaluator.library.LibraryProcessor;
import org.opencds.cqf.cql.evaluator.measure.r4.R4MeasureProcessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

/**
 *
 *
 * <h1>KarParserImpl</h1>
 *
 * This is an implementation class for the KarParser Interface.
 *
 * @author nbashyam
 */
@Service
@Transactional
public class KarParserImpl implements KarParser {

  private final Logger logger = LoggerFactory.getLogger(KarParserImpl.class);
  private static final Logger logger2 = LoggerFactory.getLogger(KarParserImpl.class);

  @Autowired AutowireCapableBeanFactory beanFactory;

  @Value("${kar.directory}")
  String karDirectory;

  @Value("${ignore.timers}")
  Boolean ignoreTimers;

  @Value("${measure-reporting-period.start}")
  String measurePeriodStart;

  @Value("${measure-reporting-period.end}")
  String measurePeriodEnd;

  @Value("${cql.enabled:true}")
  Boolean cqlEnabled;

  @Value("${fhirpath.enabled:true}")
  Boolean fhirpathEnabled;

  @Value("${bsa.output.directory}")
  String logDirectory;

  @Autowired BsaServiceUtils utils;

  // Autowired to pass to action processors.
  @Autowired BsaScheduler scheduler;

  // Autowired to pass to action processors.
  @Autowired R4MeasureProcessor measureProcessor;

  // Autowired to pass to CqlProcessors.
  @Autowired ExpressionEvaluator expressionEvaluator;

  // Autowired to pass to FhirPathProcessors.
  @Autowired LibraryProcessor libraryProcessor;

  // Autowired to pass to Actions
  @Autowired PublicHealthMessagesDao phDao;

  @Autowired HealthcareSettingsDao hsDao;

  @Autowired SubscriptionGeneratorService subscriptionGeneratorService;

  @Autowired EhrQueryService ehrInterface;

  @Autowired DirectTransportImpl directInterface;

  // Autowired to update Persistent Kar Repos
  @Autowired KarService karService;
  HashMap<String, Set<KnowledgeArtifact>> localKars;
  HashMap<String, String> localKarRepoUrlToName;

  // Autowired to pass to actions
  @Autowired
  @Qualifier("jsonParser")
  IParser jsonParser;

  @Autowired RestTemplate restTemplate;

  @Value("${report-validator.endpoint}")
  private String validatorEndpoint;

  @Value("${report-submission.endpoint}")
  private String reportSubmissionEndpoint;

  private static String[] KAR_FILE_EXT = {"json"};
  private static String JSON_KAR_EXT = "json";
  private static String RECEIVER_ADDRESS_URL =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/ext-receiverAddress";

  private static String LOCAL_HOST_REPO_BASE_URL = "http://localhost";
  private static String LOCAL_HOST_REPO_NAME = "local-repo";
  private static HashMap<String, String> actionClasses = new HashMap<>();

  // Load the Topic to Named Event Map.
  static {
    try (InputStream input =
        SubscriptionUtils.class.getClassLoader().getResourceAsStream("action-classes.properties")) {

      Properties prop = new Properties();
      prop.load(input);

      prop.forEach((key, value) -> actionClasses.put((String) key, (String) value));

    } catch (IOException ex) {
      logger2.error("Error while loading Action Classes from Properties File ");
    }
  }

  /**
   * The method is used to find the specific Action class for an action id present in the
   * PlanDefinition instance in accordance with the IG.
   *
   * @param actionId
   * @return Action class for the specific type of action defined in the IG.
   */
  public BsaAction getAction(String actionId) {

    BsaAction instance = null;
    if (actionClasses != null && actionClasses.containsKey(actionId)) {
      try {
        instance = (BsaAction) (Class.forName(actionClasses.get(actionId)).newInstance());
        BsaAction instanceBean = null;
        try {
          instanceBean = beanFactory.getBean(instance.getClass());
        } catch (NoSuchBeanDefinitionException e) {
          logger.debug(
              String.format(
                  "No such bean definition found for action %s, so creating a new instance",
                  actionId));
        }
        if (instanceBean != null) {
          beanFactory.destroyBean(beanFactory.getBean(instance.getClass()));
        }
        beanFactory.autowireBean(instance);
      } catch (InstantiationException e) {
        logger.error(" Error instantiating the object {}", e);
      } catch (IllegalAccessException e) {
        logger.error(" Error instantiating the object {}", e);
      } catch (ClassNotFoundException e) {
        logger.error(" Error instantiating the object {}", e);
      }
    }

    return instance;
  }

  @PostConstruct
  public void initializeRepository() {
    localKarRepoUrlToName = new HashMap<String, String>();
    localKars = new HashMap<String, Set<KnowledgeArtifact>>();
    loadKars();
  }

  @Override
  public void loadKars() {
    loadKarsFromDirectory(karDirectory, LOCAL_HOST_REPO_BASE_URL, LOCAL_HOST_REPO_NAME);
    persistLocalKars();
  }

  public void persistLocalKars() {

    logger.info(" Persisting Kars for each Repo ");

    for (Map.Entry<String, String> entry : localKarRepoUrlToName.entrySet()) {

      logger.info("Adding entry for Url {} and Name {}", entry.getKey(), entry.getValue());

      KnowledgeArtifactRepository repo = karService.getKARByUrl(entry.getKey());

      if (repo != null) {
        logger.info(" Adding Artifacts to existing repo {}", entry.getKey());
        repo.addKars(localKars.get(entry.getKey()));
      } else {

        logger.info(" Repository for Url {} does not exist ", entry.getKey());
        repo = new KnowledgeArtifactRepository();
        repo.setFhirServerURL(entry.getKey());
        repo.setRepoName(entry.getValue());
        repo.addKars(localKars.get(entry.getKey()));
      }

      // Save the repo.
      karService.saveOrUpdate(repo);
    }
  }

  public void loadKarsFromDirectory(String dirName, String repoUrl, String repoName) {

    // Load each of the Knowledge Artifact Bundles.
    File folder = new File(dirName);

    File[] files = folder.listFiles((FileFilter) FileFilterUtils.fileFileFilter());

    for (File kar : files) {

      if (kar.isFile() && JSON_KAR_EXT.contentEquals(FilenameUtils.getExtension(kar.getName()))) {

        logger.info(" Processing KAR {}", kar.getName());
        processKar(kar, repoUrl, repoName);
      } // For a File
    }

    // Recursively process the directories also.
    File[] dirs = folder.listFiles((FileFilter) FileFilterUtils.directoryFileFilter());

    for (File dir : dirs) {

      logger.info(" About to process directory : {}", dir.getName());
      String url = repoUrl + "/" + dir.getName();
      String name = repoName + "-" + dir.getName();

      loadKarsFromDirectory(dir.getPath(), url, name);
    }
  }

  private void processKar(File kar, String repoUrl, String repoName) {

    logger.info(" Processing File : {}", kar);

    Bundle karBundle = utils.readKarFromFile(kar.getPath());

    if (karBundle != null && (karBundle.getType() == Bundle.BundleType.COLLECTION)) {

      logger.info(" Successfully read the KAR from File ");

      KnowledgeArtifact art = new KnowledgeArtifact();

      // Setup the Id.
      art.setKarId(karBundle.getId());
      List<HealthcareSetting> allHealthcareSettings = hsDao.getAllHealthcareSettings();

      // Setup Version.
      if (karBundle.getMeta() != null && karBundle.getMeta().getVersionId() != null)
        art.setKarVersion(karBundle.getMeta().getVersionId());

      // Set Bundle
      art.setOriginalKarBundle(karBundle);
      art.setKarPath(kar.getPath());

      List<BundleEntryComponent> entries = karBundle.getEntry();

      for (BundleEntryComponent comp : entries) {

        if (Optional.ofNullable(comp).isPresent()
            && comp.getResource().getResourceType() == ResourceType.ValueSet) {
          logger.debug(" Processing ValueSet ");
          processValueSet((ValueSet) comp.getResource(), art);
        } else if (Optional.ofNullable(comp).isPresent()
            && comp.getResource().getResourceType() == ResourceType.PlanDefinition) {
          logger.info(" Processing PlanDefinition ");
          processPlanDefinition((PlanDefinition) comp.getResource(), art, kar);
          art.initializeRelatedActions();
        } else if (Optional.ofNullable(comp).isPresent()
            && comp.getResource().getResourceType() == ResourceType.Library) {
          logger.info(" Processing Library");
        }
      }

      /* for (HealthcareSetting healthcareSetting : allHealthcareSettings) {
        KarProcessingData kd = makeData(healthcareSetting, art);
        subscriptionGeneratorService.createSubscriptions(kd);
      } */

      addArtifactForPersistence(art, repoUrl, repoName);
      KnowledgeArtifactRepositorySystem.getInstance().add(art);
      art.printKarSummary();

    } else {

      logger.error(
          " Bundle for Path : {} cannot be processed because it is either non existent or of the wrong bundle type.",
          kar);
    }
  }

  private KarProcessingData makeData(HealthcareSetting hs, KnowledgeArtifact art) {

    KarProcessingData kd = new KarProcessingData();
    kd.setHealthcareSetting(hs);
    kd.setKar(art);
    kd.setEhrQueryService(ehrInterface);
    kd.setNotificationContext(new NotificationContext());
    KnowledgeArtifactStatus knowledgeArtifactStatus = new KnowledgeArtifactStatus();
    knowledgeArtifactStatus.setIsActive(true);
    knowledgeArtifactStatus.setSubscriptionsEnabled(true);
    kd.setKarStatus(knowledgeArtifactStatus);
    return kd;
  }

  private void addArtifactForPersistence(KnowledgeArtifact art, String repoUrl, String repoName) {

    if (localKars == null) {
      localKars = new HashMap<String, Set<KnowledgeArtifact>>();
    }

    if (localKars.containsKey(repoUrl)) {

      Set<KnowledgeArtifact> arts = localKars.get(repoUrl);

      if (arts != null) {
        arts.add(art);
      } else {
        arts = new HashSet<KnowledgeArtifact>();
        arts.add(art);
      }

      localKars.put(repoUrl, arts);
    } else {
      Set<KnowledgeArtifact> arts = new HashSet<KnowledgeArtifact>();
      arts.add(art);
      localKars.put(repoUrl, arts);
    }

    if (localKarRepoUrlToName != null) {

      localKarRepoUrlToName.put(repoUrl, repoName);
    } else {
      localKarRepoUrlToName = new HashMap<String, String>();
      localKarRepoUrlToName.put(repoUrl, repoName);
    }
  }

  private void processPlanDefinition(
      PlanDefinition plan, KnowledgeArtifact art, File karBundleFile) {

    art.setKarName(plan.getName());
    art.setKarPublisher(plan.getPublisher());
    processExtensions(plan, art);
    List<PlanDefinitionActionComponent> actions = plan.getAction();

    for (PlanDefinitionActionComponent act : actions) {

      if (act.getCodeFirstRep() != null && act.getCodeFirstRep().getCodingFirstRep() != null) {

        Coding cd = act.getCodeFirstRep().getCodingFirstRep();

        BsaAction action = getAction(cd.getCode());
        action.setActionId(act.getId(), plan.getUrl());
        action.setScheduler(scheduler);
        action.setJsonParser(jsonParser);
        action.setRestTemplate(restTemplate);
        action.setIgnoreTimers(ignoreTimers);
        action.setType(BsaTypes.getActionType(cd.getCode()));
        action.setLogDirectory(logDirectory);

        populateAction(plan, act, action, karBundleFile, art);

        // This is being done currently since CheckResponse Action is not supported as of yet in the
        // KARs. Once it is supported this is not needed.
        if (action.getType() == ActionType.SubmitReport) {

          populateCheckResponseAction((SubmitReport) action, art, plan);
        }

        // Setup the artifact details.
        art.addAction(action);
        art.addFirstLevelAction(action);
        art.addTriggerEvent(action);
      }
    }
  }

  public void populateCheckResponseAction(
      SubmitReport baseAction, KnowledgeArtifact art, PlanDefinition plan) {

    CheckResponse action = (CheckResponse) getAction("check-response");
    action.setActionId("check-response", plan.getUrl());
    action.setScheduler(scheduler);
    action.setJsonParser(jsonParser);
    action.setRestTemplate(restTemplate);
    action.setIgnoreTimers(ignoreTimers);
    action.setType(ActionType.CheckResponse);
    action.setLogDirectory(logDirectory);
    action.setPhDao(phDao);
    action.setDirectReceiver(directInterface);
    baseAction.setCheckResponseActionId(action.getActionId());

    art.addAction(action);
    art.addFirstLevelAction(action);
    art.addTriggerEvent(action);
  }

  public void processExtensions(PlanDefinition plan, KnowledgeArtifact art) {

    if (plan.hasExtension()) {

      Extension ext = plan.getExtensionByUrl(RECEIVER_ADDRESS_URL);

      if (ext != null) {

        Type t = ext.getValue();
        if (t instanceof PrimitiveType) {
          PrimitiveType<?> i = (PrimitiveType<?>) t;
          if (i instanceof UriType) {

            logger.info(" Found Receiver Address {}", i.getValueAsString());
            art.addReceiverAddress((UriType) i);
          }
        }
      }
    }
  }

  private void populateOutputDataReq(PlanDefinitionActionComponent ac, BsaAction action) {

    List<DataRequirement> drs = ac.getOutput();
    action.setOutputData(drs);
  }

  private void populateInputDataReq(PlanDefinitionActionComponent ac, BsaAction action) {

    List<DataRequirement> drs = ac.getInput();
    action.setInputData(drs);

    for (DataRequirement dr : drs) {
      try {
        ResourceType rt = ResourceType.fromCode(dr.getType());
        action.addInputResourceType(dr.getId(), rt);
      } catch (FHIRException ex) {
        logger.error(" Type specified is not a resource Type {}", dr.getType());
      }
    }
  }

  private void populateAction(
      PlanDefinition plan,
      PlanDefinitionActionComponent act,
      BsaAction action,
      File karBundleFile,
      KnowledgeArtifact art) {

    if (act.hasTrigger()) {
      action.setNamedEventTriggers(getNamedEvents(act));
    }

    if (act.hasInput()) {
      populateInputDataReq(act, action);
    }

    if (act.hasOutput()) {
      populateOutputDataReq(act, action);
    }

    CanonicalType libraryCanonical = plan.hasLibrary() ? plan.getLibrary().get(0) : null;
    if (act.hasCondition()) {
      populateCondition(act, action, libraryCanonical, karBundleFile);
    }

    if (act.hasRelatedAction()) {
      populateRelatedAction(plan, act, action);
    }

    if (act.hasAction()) {
      populateSubActions(plan, act, action, karBundleFile, art);
    }

    if (act.hasDefinitionUriType()) {
      action.setMeasureUri(act.getDefinitionUriType().getValue());
    }

    if (act.hasTiming()) {

      // Todo - handle timing elements in the action itslef.
    }

    // TODO: Why are these populated at this point?
    action.setJsonParser(this.jsonParser);
    action.setIgnoreTimers(this.ignoreTimers);

    if (action.getType() == ActionType.EvaluateMeasure) {
      setMeasureParameters(act, action);
    } else if (action.getType() == ActionType.ValidateReport) {
      ValidateReport vr = (ValidateReport) (action);
      vr.setValidatorEndpoint(validatorEndpoint);
      vr.setPhDao(phDao);
    } else if (action.getType() == ActionType.SubmitReport) {
      SubmitReport sr = (SubmitReport) (action);
      sr.setSubmissionEndpoint(reportSubmissionEndpoint);
      sr.setPhDao(phDao);
      sr.setDirectSender(directInterface);
      populateCheckResponseAction(sr, art, plan);
    } else if (action.getType() == ActionType.CreateReport) {
      CreateReport cr = (CreateReport) action;
      cr.setPhDao(phDao);
    }
  }

  private void setMeasureParameters(PlanDefinitionActionComponent act, BsaAction action) {

    // Setup the MeasureReportId that is generated.
    if (act.hasOutput()) {

      for (DataRequirement dr : act.getOutput()) {

        if (dr.getType() != null
            && dr.getType().contentEquals(ResourceType.MeasureReport.toString())) {
          EvaluateMeasure em = (EvaluateMeasure) (action);
          em.setMeasureReportId(dr.getId());

          if (measurePeriodStart != null) {
            em.setPeriodStart(measurePeriodStart);
          }

          if (measurePeriodEnd != null) {
            em.setPeriodEnd(measurePeriodEnd);
          }

          // setup the Measure Processor
          em.setMeasureProcessor(measureProcessor);
        }
      }
    }
  }

  private void populateSubActions(
      PlanDefinition plan,
      PlanDefinitionActionComponent ac,
      BsaAction action,
      File karBundleFile,
      KnowledgeArtifact art) {

    List<PlanDefinitionActionComponent> actions = ac.getAction();

    if (actions != null && actions.size() > 0) {
      for (PlanDefinitionActionComponent act : actions) {

        if (act.getCodeFirstRep() != null && act.getCodeFirstRep().getCodingFirstRep() != null) {

          Coding cd = act.getCodeFirstRep().getCodingFirstRep();

          BsaAction subAction = getAction(cd.getCode());
          subAction.setActionId(act.getId(), plan.getUrl());
          subAction.setScheduler(scheduler);
          subAction.setJsonParser(jsonParser);
          subAction.setRestTemplate(restTemplate);
          subAction.setIgnoreTimers(ignoreTimers);
          subAction.setType(BsaTypes.getActionType(cd.getCode()));
          subAction.setLogDirectory(logDirectory);

          populateAction(plan, act, subAction, karBundleFile, art);

          // Setup the artifact details.
          action.addAction(subAction);
        }
      }
    }
  }

  private void populateCondition(
      PlanDefinitionActionComponent ac,
      BsaAction action,
      CanonicalType libraryCanonical,
      File karBundleFile) {

    List<PlanDefinitionActionConditionComponent> conds = ac.getCondition();

    for (PlanDefinitionActionConditionComponent con : conds) {

      if (con.getExpression() != null
          // Expression.ExpressionLanguage.fromCode does not support text/cql-identifier
          // so using
          // local fromCode for now
          && (fromCode(con.getExpression().getLanguage())
              .equals(Expression.ExpressionLanguage.TEXT_CQL))
          && cqlEnabled) {

        logger.info(" Found a CQL Expression ");
        BsaCqlCondition bc = new BsaCqlCondition();
        bc.setUrl(libraryCanonical.getValue());

        // Set location of eRSD bundle for loading terminology and library logic
        Endpoint libraryAndTerminologyEndpoint =
            new Endpoint()
                .setAddress(karBundleFile.getAbsolutePath())
                .setConnectionType(new Coding().setCode("hl7-fhir-files"));
        bc.setLibraryEndpoint(libraryAndTerminologyEndpoint);
        bc.setTerminologyEndpoint(libraryAndTerminologyEndpoint);
        bc.setLogicExpression(con.getExpression());
        bc.setLibraryProcessor(libraryProcessor);
        action.addCondition(bc);
      } else if (con.hasExtension(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL)
          && (cqlEnabled || fhirpathEnabled)) {
        Extension ext = con.getExtensionByUrl(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL);
        Expression exp = (Expression) ext.getValue();
        if (exp != null
            // Expression.ExpressionLanguage.fromCode does not support text/cql-identifier
            // so using
            // local fromCode for now
            && (fromCode(exp.getLanguage()).equals(Expression.ExpressionLanguage.TEXT_CQL))
            && cqlEnabled) {

          logger.info(" Found a CQL Expression from an alternative expression extension");
          BsaCqlCondition bc = new BsaCqlCondition();
          bc.setUrl(libraryCanonical.getValue());

          // Set location of eRSD bundle for loading terminology and library logic
          Endpoint libraryAndTerminologyEndpoint =
              new Endpoint()
                  .setAddress(karBundleFile.getAbsolutePath())
                  .setConnectionType(new Coding().setCode("hl7-fhir-files"));
          bc.setLibraryEndpoint(libraryAndTerminologyEndpoint);
          bc.setTerminologyEndpoint(libraryAndTerminologyEndpoint);
          bc.setLogicExpression(exp);
          bc.setLibraryProcessor(libraryProcessor);
          action.addCondition(bc);
        } else if (exp != null
            && (fromCode(exp.getLanguage()).equals(Expression.ExpressionLanguage.TEXT_FHIRPATH))
            && fhirpathEnabled) {

          logger.info(" Found a FHIR Path Expression from an alternative expression extension");
          BsaFhirPathCondition bc = new BsaFhirPathCondition();
          bc.setLogicExpression(exp);
          bc.setExpressionEvaluator(expressionEvaluator);
          action.addCondition(bc);
        } else {
          logger.error(" Unknown type of Alternative Expression passed, cannot process ");
        }
      } else if (con.getExpression() != null
          && (fromCode(con.getExpression().getLanguage())
              .equals(Expression.ExpressionLanguage.TEXT_FHIRPATH))
          && fhirpathEnabled) {

        logger.info(" Found a FHIR Path Expression ");
        BsaFhirPathCondition bc = new BsaFhirPathCondition();
        bc.setLogicExpression(con.getExpression());
        bc.setExpressionEvaluator(expressionEvaluator);
        action.addCondition(bc);
      } else {
        logger.error(" Unknown type of Expression passed, cannot process ");
      }
    }
  }

  private Expression.ExpressionLanguage fromCode(String language) {
    switch (language) {
      case "text/cql":
      case "text/cql.expression":
      case "text/cql-expression":
      case "text/cql-identifier":
      case "text/cql.identifier":
      case "text/cql.name":
      case "text/cql-name":
        return Expression.ExpressionLanguage.TEXT_CQL;
      case "text/fhirpath":
        return Expression.ExpressionLanguage.TEXT_FHIRPATH;
      case "application/x-fhir-query":
        return Expression.ExpressionLanguage.APPLICATION_XFHIRQUERY;
      default:
        throw new FHIRException("Unknown ExpressionLanguage code '" + language + "'");
    }
  }

  private void populateRelatedAction(
      PlanDefinition plan, PlanDefinitionActionComponent ac, BsaAction action) {

    List<PlanDefinitionActionRelatedActionComponent> racts = ac.getRelatedAction();

    for (PlanDefinitionActionRelatedActionComponent ract : racts) {

      if (ract.getRelationship() == ActionRelationshipType.BEFORESTART) {

        BsaRelatedAction bract = new BsaRelatedAction();
        bract.setRelationship(ract.getRelationship());
        bract.setRelatedActionId(ract.getActionId(), plan.getUrl());

        if (ract.getOffsetDuration() != null) {
          bract.setDuration(ract.getOffsetDuration());
        }
        action.addRelatedAction(bract);
      }
    }
  }

  private Set<String> getNamedEvents(PlanDefinitionActionComponent ac) {

    Set<String> events = new HashSet<String>();

    List<TriggerDefinition> triggers = ac.getTrigger();

    for (TriggerDefinition td : triggers) {
      if (td.getType() == TriggerType.NAMEDEVENT) events.add(td.getId());
    }

    return events;
  }

  private void processValueSet(ValueSet vs, KnowledgeArtifact art) {

    art.addDependentValueSet(vs);
  }
}
