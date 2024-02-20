package com.drajer.bsa.service.impl;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.auth.AuthorizationUtils;
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
import com.drajer.bsa.kar.model.FhirQueryFilter;
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
import com.drajer.bsa.routing.impl.RestfulTransportImpl;
import com.drajer.bsa.scheduler.BsaScheduler;
import com.drajer.bsa.service.KarParser;
import com.drajer.bsa.service.KarService;
import com.drajer.bsa.service.PublicHealthAuthorityService;
import com.drajer.bsa.utils.BsaConstants;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.bsa.utils.SubscriptionUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.sof.utils.FhirContextInitializer;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
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
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Library;
import org.hl7.fhir.r4.model.PlanDefinition;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionComponent;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionConditionComponent;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionRelatedActionComponent;
import org.hl7.fhir.r4.model.PrimitiveType;
import org.hl7.fhir.r4.model.Reference;
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

  /** */
  private static final String VARIABLE_EXTENSION_URL =
      "http://hl7.org/fhir/StructureDefinition/variable";

  private static final String RCTC_DEFAULT_SYSTEM = "urn:ietf:rfc:3986";

  private final Logger logger = LoggerFactory.getLogger(KarParserImpl.class);
  private static final Logger logger2 = LoggerFactory.getLogger(KarParserImpl.class);

  @Autowired AutowireCapableBeanFactory beanFactory;

  @Value("${kar.directory:default}")
  String karDirectory;

  @Value("${ignore.timers}")
  Boolean ignoreTimers;

  @Value("${measure-reporting-period.start}")
  String measurePeriodStart;

  @Value("${measure-reporting-period.end}")
  String measurePeriodEnd;

  @Value("${cql.enabled:false}")
  boolean cqlEnabled;

  @Value("${fhirpath.enabled:true}")
  boolean fhirpathEnabled;

  @Value("${bsa.output.directory:bsa-output}")
  String logDirectory;

  @Autowired BsaServiceUtils utils;

  // Autowired to pass to action processors.
  @Autowired BsaScheduler scheduler;

  @Autowired KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;

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

  @Autowired RestfulTransportImpl restSubmitter;

  @Autowired AuthorizationUtils authUtils;

  @Autowired FhirContextInitializer fhirContextInitializer;

  @Autowired PublicHealthAuthorityService publicHealthAuthorityService;

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

  private List<Expression> planVariableExpressions;

  private static final String JSON_KAR_EXT = "json";
  private static final String RECEIVER_ADDRESS_URL =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-receiver-endpoint";

  private static final String LOCAL_HOST_REPO_BASE_URL = "http://localhost";
  private static final String LOCAL_HOST_REPO_NAME = "local-repo";
  private static final String PH_QUERY_EXTENSION_URL =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-fhirquerypattern-extension";
  private static final String MEDMORPH_QUERY_EXTENSION_URL =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-fhirquerypattern-extension";
  private static final String PH_RELATED_DATA_EXTENSION_URL =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-relateddata-extension";
  private static final String MEDMORPH_RELATED_DATA_EXTENSION_URL =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-relateddata-extension";

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
      } catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
        logger.error("Error instantiating the object.", e);
      }
    }

    return instance;
  }

  @PostConstruct
  public void initializeRepository() {
    localKarRepoUrlToName = new HashMap<>();
    localKars = new HashMap<>();
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

    logger.debug("Scanning KARs folder {}", karDirectory);
    // Load each of the Knowledge Artifact Bundles.
    File folder = new File(dirName);

    File[] files = folder.listFiles((FileFilter) FileFilterUtils.fileFileFilter());

    if (files != null) {
      for (File kar : files) {

        if (kar.isFile() && JSON_KAR_EXT.contentEquals(FilenameUtils.getExtension(kar.getName()))) {

          logger.info(" Processing KAR {}", kar.getName());
          processKar(kar, repoUrl, repoName);
        } // For a File
      }
    }

    // Recursively process the directories also.
    File[] dirs = folder.listFiles((FileFilter) FileFilterUtils.directoryFileFilter());

    if (dirs != null) {
      for (File dir : dirs) {

        logger.info(" About to process directory : {}", dir.getName());
        String url = repoUrl + "/" + dir.getName();
        String name = repoName + "-" + dir.getName();

        loadKarsFromDirectory(dir.getPath(), url, name);
      }
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
          art.initializeRelatedDataIds();
        } else if (Optional.ofNullable(comp).isPresent()
            && comp.getResource().getResourceType() == ResourceType.Library) {
          logger.info(" Processing Library");

          Library lib = (Library) comp.getResource();
          if (art.getKarName() == null) {
            art.setKarName(lib.getName());
          }
          if (art.getKarPublisher() == null) {
            art.setKarPublisher(lib.getPublisher());
          }

          if (lib.getId().contains("rctc")) {

            logger.info(" Adding Rctc Version to the Action Repo {}", lib.getVersion());
            art.setRctcVersion(lib.getVersion());
            art.setRctcOid(getRctcOid(lib));
          }
        } else if (Optional.ofNullable(comp).isPresent()) {
          logger.info(" Adding resource to dependencies");
          art.addDependentResource(comp.getResource());
        }
      }

      /**
       * for (HealthcareSetting healthcareSetting : allHealthcareSettings) { KarProcessingData kd =
       * makeData(healthcareSetting, art); subscriptionGeneratorService.createSubscriptions(kd); }
       */
      addArtifactForPersistence(art, repoUrl, repoName);
      knowledgeArtifactRepositorySystem.add(art);
      art.printKarSummary();

    } else {

      if (karBundle == null) {
        logger.error(
            " Bundle for Path : {} cannot be processed because it is either non existent ", kar);
      } else {
        logger.error(" Bundle for Path : {} is of type {} ", kar, karBundle.getType().toString());
      }
    }
  }

  /**
   * Method to extract the RCTC OID from the library.
   *
   * @param lib
   * @return
   */
  private String getRctcOid(Library lib) {

    if (lib != null && lib.getId().contains("rctc")) {

      if (lib.hasIdentifier()) {

        List<Identifier> ids = lib.getIdentifier();

        for (Identifier id : ids) {

          if (id.hasSystem()
              && id.getSystem().contentEquals("RCTC_DEFAULT_SYSTEM")
              && id.hasValue()) {

            return id.getValue();
          }
        }
      }
    }

    return CdaGeneratorConstants.RCTC_OID;
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
      localKars = new HashMap<>();
    }

    if (localKars.containsKey(repoUrl)) {

      Set<KnowledgeArtifact> arts = localKars.get(repoUrl);

      if (arts != null) {
        arts.add(art);
      } else {
        arts = new HashSet<>();
        arts.add(art);
      }

      localKars.put(repoUrl, arts);
    } else {
      Set<KnowledgeArtifact> arts = new HashSet<>();
      arts.add(art);
      localKars.put(repoUrl, arts);
    }

    if (localKarRepoUrlToName != null) {

      localKarRepoUrlToName.put(repoUrl, repoName);
    } else {
      localKarRepoUrlToName = new HashMap<>();
      localKarRepoUrlToName.put(repoUrl, repoName);
    }
  }

  private void processPlanDefinition(
      PlanDefinition plan, KnowledgeArtifact art, File karBundleFile) {

    art.setKarName(plan.getName());
    art.setKarPublisher(plan.getPublisher());
    processExtensions(plan, art);

    /**
     * if (plan.hasExtension(VARIABLE_EXTENSION_URL)) { Extension variableExtension =
     * plan.getExtensionByUrl(VARIABLE_EXTENSION_URL); Type variable = variableExtension.getValue();
     * if (variable instanceof Expression) { planVariableExpression = (Expression) variable;
     * logger.debug("Found Variable Extension Expression"); } else { logger.debug("Found Variable
     * Extension, but expected Expression."); } }
     */
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
        if (action.getType() == ActionType.SUBMIT_REPORT) {

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
    action.setType(ActionType.CHECK_RESPONSE);
    action.setLogDirectory(logDirectory);
    action.setPhDao(phDao);
    action.setDirectReceiver(directInterface);
    baseAction.setCheckResponseActionId(action.getActionId());

    art.addAction(action);
    art.addFirstLevelAction(action);
    art.addTriggerEvent(action);
  }

  public void processExtensions(PlanDefinition plan, KnowledgeArtifact art) {

    planVariableExpressions = new ArrayList<>();

    if (plan.hasExtension()) {

      List<Extension> extensions = plan.getExtension();

      for (Extension ext : extensions) {

        if (ext.hasValue() && ext.getUrl().contentEquals(RECEIVER_ADDRESS_URL)) {
          logger.info(" Processing Receiver URL extension ");
          Type t = ext.getValue();
          if (t instanceof PrimitiveType) {
            PrimitiveType<?> i = (PrimitiveType<?>) t;
            if (i instanceof UriType) {

              logger.info(" Found Receiver Address {}", i.getValueAsString());
              art.addReceiverAddress((UriType) i);
            }
          } else if (t instanceof Reference) {
            Endpoint endpoint =
                (Endpoint)
                    art.getDependentResource(ResourceType.Endpoint, ((Reference) t).getReference());
            if (endpoint != null && endpoint.hasAddressElement()) {
              art.addReceiverAddress(endpoint.getAddressElement());
            } else {
              Reference r = (Reference) t;
              art.addReceiverAddress(new UriType(r.getReference()));
            }
          }
        } else if (ext.hasValue() && ext.getUrl().contentEquals(VARIABLE_EXTENSION_URL)) {
          logger.info(" Processing Variables ");

          Type variable = ext.getValue();
          if (variable instanceof Expression) {
            Expression exp = (Expression) variable;
            logger.info(
                "Found Variable Extension Expression with Name {} and expression {}",
                exp.getName(),
                exp.getExpression());
            planVariableExpressions.add(exp);
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

    logger.info(" Action Id {}", action.getActionId());
    List<DataRequirement> drs = ac.getInput();
    action.setInputData(drs);

    for (DataRequirement dr : drs) {
      try {
        ResourceType rt = ResourceType.fromCode(dr.getType());
        action.addInputResourceType(dr.getId(), rt);

        // Get Query Extensions to identify default queries.
        Extension queryExt = dr.getExtensionByUrl(PH_QUERY_EXTENSION_URL);

        if (queryExt == null) {
          queryExt = dr.getExtensionByUrl(MEDMORPH_QUERY_EXTENSION_URL);
        }

        FhirQueryFilter query = new FhirQueryFilter();
        query.setResourceType(rt);
        query.setDataReqId(dr.getId());

        if (queryExt != null && queryExt.getValue() != null) {

          logger.info(" Found a query extension for action Id {}", action.getActionId());
          String st = queryExt.getValueAsPrimitive().getValueAsString();

          query.setQueryString(st);
          action.addQueryFilter(dr.getId(), query);
        }

        // Get Related Data Ids to reuse data already accessed.
        Extension relatedDataExt = dr.getExtensionByUrl(PH_RELATED_DATA_EXTENSION_URL);

        if (relatedDataExt == null) {
          relatedDataExt = dr.getExtensionByUrl(MEDMORPH_RELATED_DATA_EXTENSION_URL);
        }

        if (relatedDataExt != null && relatedDataExt.getValue() != null) {

          logger.info(" Found a related data extension ");
          String st = relatedDataExt.getValueAsPrimitive().getValueAsString();

          query.setRelatedDataId(st);
          action.addRelatedDataId(dr.getId(), st);
        }

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

      logger.info("PlanDefinitionActionComponent have time");
    }

    action.setJsonParser(this.jsonParser);
    action.setIgnoreTimers(this.ignoreTimers);

    if (action.getType() == ActionType.EVALUATE_MEASURE) {
      setMeasureParameters(act, action);
    } else if (action.getType() == ActionType.VALIDATE_REPORT) {
      ValidateReport vr = (ValidateReport) (action);
      vr.setValidatorEndpoint(validatorEndpoint);
      vr.setPhDao(phDao);
    } else if (action.getType() == ActionType.SUBMIT_REPORT) {
      SubmitReport sr = (SubmitReport) (action);
      sr.setSubmissionEndpoint(reportSubmissionEndpoint);
      sr.setPhDao(phDao);
      sr.setDirectSender(directInterface);
      sr.setRestSubmitter(restSubmitter);
      sr.setFhirContextInitializer(fhirContextInitializer);
      sr.setAuthorizationUtils(authUtils);
      sr.setPublicHealthAuthorityService(publicHealthAuthorityService);
      populateCheckResponseAction(sr, art, plan);
    } else if (action.getType() == ActionType.CREATE_REPORT) {
      CreateReport cr = (CreateReport) action;
      cr.setPhDao(phDao);
    }

    art.populateDefaultQueries(action);
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

    if (actions != null && !actions.isEmpty()) {
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
        Endpoint karEndpoint =
            new Endpoint()
                // get the kar directory so that the Providers will bundle everything together i.e.
                // All Kar bundles
                .setAddress(karBundleFile.getParentFile().getAbsolutePath())
                .setConnectionType(new Coding().setCode("hl7-fhir-files"));
        bc.setLibraryEndpoint(karEndpoint);
        bc.setTerminologyEndpoint(karEndpoint);
        // Necessary for Cql Evaluation because of CodeSystem Retrieve
        bc.setDataEndpoint(karEndpoint);
        bc.setLogicExpression(con.getExpression());
        bc.setLibraryProcessor(libraryProcessor);
        bc.setNormalReportingDuration(null);
        action.addCondition(bc);
      } else if (con.getExpression().hasExtension(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL)
          || con.hasExtension(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL)
              && (cqlEnabled || fhirpathEnabled)) {
        Extension ext = con.getExtensionByUrl(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL);
        if (ext == null) {
          ext =
              con.getExpression()
                  .getExtensionByUrl(BsaConstants.ALTERNATIVE_EXPRESSION_EXTENSION_URL);
        }
        Expression exp = (Expression) ext.getValue();
        if (exp != null
            // Expression.ExpressionLanguage.fromCode does not support text/cql-identifier
            // so using
            // local fromCode for now
            && (fromCode(exp.getLanguage()).equals(Expression.ExpressionLanguage.TEXT_CQL))
            && cqlEnabled) {

          logger.info(" Found a CQL Expression from an alternative expression extension");
          BsaCqlCondition bc = new BsaCqlCondition();
          if (libraryCanonical == null && exp.hasReferenceElement()) {
            libraryCanonical = new CanonicalType(exp.getReference());
          }
          bc.setUrl(libraryCanonical.getValue());

          // Set location of eRSD bundle for loading terminology and library logic
          Endpoint karEndpoint =
              new Endpoint()
                  // get the kar directory so that the Providers will bundle everything together
                  // i.e. All Kar bundles
                  .setAddress(karBundleFile.getParentFile().getAbsolutePath())
                  .setConnectionType(new Coding().setCode("hl7-fhir-files"));
          bc.setLibraryEndpoint(karEndpoint);
          bc.setTerminologyEndpoint(karEndpoint);
          // Necessary for Cql Evaluation because of CodeSystem Retrieve
          bc.setDataEndpoint(karEndpoint);
          bc.setLogicExpression(exp);
          bc.setLibraryProcessor(libraryProcessor);
          action.addCondition(bc);
        } else if (exp != null
            && (fromCode(exp.getLanguage()).equals(Expression.ExpressionLanguage.TEXT_FHIRPATH))
            && fhirpathEnabled) {

          logger.info(" Found a FHIR Path Expression from an alternative expression extension");
          BsaFhirPathCondition bc = new BsaFhirPathCondition();
          if (planVariableExpressions != null) {
            bc.setVariables(planVariableExpressions);
          }
          bc.setLogicExpression(exp);
          bc.setExpressionEvaluator(expressionEvaluator);
          action.addCondition(bc);
        } else if (con.getExpression() != null
            && (fromCode(con.getExpression().getLanguage())
                .equals(Expression.ExpressionLanguage.TEXT_FHIRPATH))
            && fhirpathEnabled) {
          logger.info(
              " Cql disabled and found alternative cql expression therefor using primary fhirpath expression");
          BsaFhirPathCondition bc = new BsaFhirPathCondition();
          if (planVariableExpressions != null) {
            bc.setVariables(planVariableExpressions);
          }
          bc.setLogicExpression(con.getExpression());
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
        if (planVariableExpressions != null) {
          bc.setVariables(planVariableExpressions);
        }
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

    Set<String> events = new HashSet<>();

    List<TriggerDefinition> triggers = ac.getTrigger();

    for (TriggerDefinition td : triggers) {
      if (td.getType() == TriggerType.NAMEDEVENT) events.add(td.getName());
    }

    return events;
  }

  private void processValueSet(ValueSet vs, KnowledgeArtifact art) {

    art.addDependentValueSet(vs);
  }
}
