package com.drajer.sof.utils;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.context.PerformanceOptionsEnum;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.api.IRestfulClientFactory;
import ca.uhn.fhir.rest.client.api.ServerValidationModeEnum;
import ca.uhn.fhir.rest.client.interceptor.AdditionalRequestHeadersInterceptor;
import ca.uhn.fhir.rest.client.interceptor.BearerTokenAuthInterceptor;
import ca.uhn.fhir.rest.client.interceptor.LoggingInterceptor;
import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException;
import ca.uhn.fhir.rest.server.exceptions.ForbiddenOperationException;
import com.drajer.bsa.ehr.service.EhrHeaderInterceptorInterface;
import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplate;
import com.drajer.ecrapp.fhir.utils.ecrretry.EcrFhirRetryClient;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import org.apache.commons.text.StringEscapeUtils;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class FhirContextInitializer {

  private static final String DSTU2 = "DSTU2";
  private static final String DSTU2_1 = "DSTU2_1";
  private static final String DSTU3 = "DSTU3";
  private static final String R4 = "R4";
  private static final String QUERY_PATIENT = "?patient=";
  private static final Logger logger = LoggerFactory.getLogger(FhirContextInitializer.class);

  @Value("${ecr.fhir.pagecount.enabled:false}")
  private Boolean pagingCountEnabled;

  @Value("${ecr.fhir.pagecount.value:500}")
  private Integer pagingCount;

  @Value("${ecr.fhir.pagecount.resources:}")
  private String pagingResource;

  @Value("${ecr.fhir.query-by-period.enabled:false}")
  private Boolean queryByDateEnabled;

  @Value("${ecr.fhir.query-by-period.date.resources:}")
  private String dateResources;

  @Value("${ecr.fhir.query-by-period.lastupdated.resources:}")
  private String lastUpdatedResources;

  @Value("${ecr.fhir.query-by-encounter.enabled:false}")
  private Boolean queryByEncounterEnabled;

  @Value("${ecr.fhir.query-by-encounter.resources:}")
  private String resourcesByEncounter;

  @Value("${ecr.fhir.skip.resources:}")
  private String skipResource;

  @Value("${ecr.fhir.query-by-period.uselastquerytime:false}")
  private Boolean queryByLastTriggerDateEnabled;

  @Value("${ecr.fhir.skip.triggerquery.resources:}")
  private String skipTriggerResource;

  @Value("${socket.timeout:3}")
  private Integer socketTimeout;

  @Value("${connection.timeout:3}")
  private Integer connectionTimeout;

  @Value("${pool.max.per.route:10}")
  private Integer poolMaxPerRoute;

  @Value("${pool.max.total:100}")
  private Integer poolMaxTotal;

  @Value("${connection.request.time.out:30}")
  private Integer connectionReqTimeOut;

  @Autowired FHIRRetryTemplate retryTemplate;

  @Autowired EhrHeaderInterceptorInterface headerInterceptor;

  public FhirContextInitializer(FHIRRetryTemplate retryTemplate) {
    this.retryTemplate = retryTemplate;
  }

  public void setRetryTemplate(final FHIRRetryTemplate retryTemplate) {
    this.retryTemplate = retryTemplate;
  }

  public EhrHeaderInterceptorInterface getHeaderInterceptor() {
    return headerInterceptor;
  }

  public void setHeaderInterceptor(EhrHeaderInterceptorInterface headerInterceptor) {
    this.headerInterceptor = headerInterceptor;
  }

  /**
   * Get FhirContext appropriate to fhirVersion
   *
   * @param fhirVersion The FHIR Version to use, either as a fhir version or a package name.
   * @return The appropriate FhirContext to use for the server
   */
  public FhirContext getFhirContext(String fhirVersion) {
    switch (fhirVersion) {
      case DSTU2:
        return FhirContext.forCached(FhirVersionEnum.DSTU2);
      case DSTU2_1:
        return FhirContext.forCached(FhirVersionEnum.DSTU2_1);
      case DSTU3:
        return FhirContext.forCached(FhirVersionEnum.DSTU3);
      case R4:
        return FhirContext.forCached(FhirVersionEnum.R4);
      default:
        return FhirContext.forCached(FhirVersionEnum.DSTU2);
    }
  }

  /**
   * Creates a GenericClient with standard intercepters used throughout the services.
   *
   * @param url the base URL of the FHIR server to connect to
   * @param accessToken the name of the key to use to generate the token
   * @param requestId the prefix for all X-Request-ID values used with this new client
   * @return a Generic Client
   */
  public IGenericClient createClient(
      FhirContext context, String url, String accessToken, String requestId, String ehrContext) {
    logger.trace("Initializing the Client");

    FhirClient client =
        new FhirClient(context.newRestfulGenericClient(url), requestId, EventTypes.QueryType.NONE);

    context.getRestfulClientFactory().setSocketTimeout(60 * 1000);
    context.getRestfulClientFactory().setServerValidationMode(ServerValidationModeEnum.NEVER);
    context.setPerformanceOptions(PerformanceOptionsEnum.DEFERRED_MODEL_SCANNING);

    IRestfulClientFactory restfulClientFactory = context.getRestfulClientFactory();
    restfulClientFactory.setSocketTimeout(socketTimeout * 1000);
    restfulClientFactory.setConnectTimeout(connectionTimeout * 1000);
    restfulClientFactory.setPoolMaxPerRoute(poolMaxPerRoute * 1000);
    restfulClientFactory.setPoolMaxTotal(poolMaxTotal * 1000);
    restfulClientFactory.setConnectionRequestTimeout(connectionReqTimeOut * 1000);

    if (accessToken != null && !accessToken.equalsIgnoreCase("")) {
      client.registerInterceptor(new BearerTokenAuthInterceptor(accessToken));
      // client.registerInterceptor(new LoggingInterceptor(true));
    } else {
      logger.debug("AccessToken not supplied for %{}", StringEscapeUtils.escapeJava(url));
    }

    // Add HTTP Header Interceptor
    if (ehrContext != null && !ehrContext.isEmpty()) {
      AdditionalRequestHeadersInterceptor hi = headerInterceptor.getHeaderInterceptor(ehrContext);
      client.registerInterceptor(hi);
    }

    if (logger.isDebugEnabled()) {
      client.registerInterceptor(new LoggingInterceptor(true));
    }
    if (retryTemplate.isRetryEnabled()) {
      logger.info(
          "Initialized the Retryable Client with X-Request-ID: {}",
          StringEscapeUtils.escapeJava(requestId));
      return new EcrFhirRetryClient(client, retryTemplate, requestId, EventTypes.QueryType.NONE);
    }
    logger.trace(
        "Initialized the Client with X-Request-ID: {}",
        StringEscapeUtils.escapeJava(client.getHttpInterceptor().getXReqId()));
    return client;
  }

  public IGenericClient createClient(
      FhirContext context, LaunchDetails launchDetails, EventTypes.QueryType type) {
    logger.trace("Initializing the Client");
    context.getRestfulClientFactory().setServerValidationMode(ServerValidationModeEnum.NEVER);
    context.setPerformanceOptions(PerformanceOptionsEnum.DEFERRED_MODEL_SCANNING);
    FhirClient client =
        new FhirClient(
            context.newRestfulGenericClient(launchDetails.getEhrServerURL()),
            launchDetails.getxRequestId(),
            type);
    context.getRestfulClientFactory().setSocketTimeout(60 * 1000);

    BearerTokenAuthInterceptor bearerTokenAuthInterceptor =
        new EcrOAuthBearerTokenInterceptor(launchDetails);
    client.registerInterceptor(bearerTokenAuthInterceptor);

    if (logger.isDebugEnabled()) {
      client.registerInterceptor(new LoggingInterceptor(true));
    }
    if (retryTemplate.isRetryEnabled()) {
      logger.info(
          "Initialized the Retryable Client with X-Request-ID: {}",
          StringEscapeUtils.escapeJava(client.getHttpInterceptor().getXReqId()));
      return new EcrFhirRetryClient(
          client, retryTemplate, client.getHttpInterceptor().getXReqId(), type);
    }
    logger.trace(
        "Initialized the Client with X-Request-ID: {}",
        StringEscapeUtils.escapeJava(client.getHttpInterceptor().getXReqId()));

    return client;
  }

  public MethodOutcome submitResource(IGenericClient genericClient, Resource resource) {
    MethodOutcome outcome = null;
    try {
      outcome = genericClient.create().resource(resource).prettyPrint().encodedJson().execute();
    } catch (Exception e) {
      logger.error(
          "Error in Submitting the resource::::: {}",
          StringEscapeUtils.escapeJava(resource.getResourceType().name()),
          e);
    }

    return outcome;
  }

  public IBaseResource getResouceById(
      LaunchDetails authDetails,
      IGenericClient genericClient,
      FhirContext context,
      String resourceName,
      String resourceId) {
    IBaseResource resource = null;

    if (Boolean.TRUE.equals(checkSkipResource(resourceName, (FhirClient) genericClient))) {
      return resource;
    }

    try {
      logger.info("Getting {} data by ID {}", resourceName, resourceId);
      resource =
          (IBaseResource) genericClient.read().resource(resourceName).withId(resourceId).execute();
    } catch (ForbiddenOperationException scopeException) {
      logger.info(
          "Failed getting {} resource by Id: {}\n{}\nCurrent scope: {}",
          resourceName,
          resourceId,
          scopeException.getMessage(),
          authDetails.getScope());
    } catch (BaseServerResponseException responseException) {
      if (responseException.getOperationOutcome() != null) {
        logger.debug(
            context
                .newJsonParser()
                .encodeResourceToString(responseException.getOperationOutcome()));
      }
      logger.info(
          "Error in getting {} resource by Id: {}", resourceName, resourceId, responseException);
    } catch (Exception e) {
      logger.info("Error in getting {} resource by Id: {}", resourceName, resourceId, e);
    }
    return resource;
  }

  public IBaseBundle getResourceByPatientId(
      LaunchDetails authDetails,
      IGenericClient genericClient,
      FhirContext context,
      String resourceName) {
    IBaseBundle bundleResponse = null;
    if (Boolean.TRUE.equals(checkSkipResource(resourceName, (FhirClient) genericClient))) {
      return bundleResponse;
    }
    String url =
        authDetails.getEhrServerURL()
            + "/"
            + resourceName
            + QUERY_PATIENT
            + authDetails.getLaunchPatientId();
    url += getCustomQueryParameters(resourceName, authDetails, (FhirClient) genericClient);

    bundleResponse = getResourceBundleByUrl(authDetails, genericClient, context, resourceName, url);
    return bundleResponse;
  }

  protected IBaseBundle getObservationByPatientId(
      LaunchDetails authDetails,
      IGenericClient genericClient,
      FhirContext context,
      String resourceName,
      String category) {
    IBaseBundle bundleResponse = null;
    if (Boolean.TRUE.equals(checkSkipResource(resourceName, (FhirClient) genericClient))) {
      return bundleResponse;
    }
    String url =
        authDetails.getEhrServerURL()
            + "/"
            + resourceName
            + QUERY_PATIENT
            + authDetails.getLaunchPatientId()
            + "&category="
            + category;
    url += getCustomQueryParameters(resourceName, authDetails, (FhirClient) genericClient);

    bundleResponse = getResourceBundleByUrl(authDetails, genericClient, context, resourceName, url);
    return bundleResponse;
  }

  protected IBaseBundle getResourceByPatientIdAndCode(
      LaunchDetails authDetails,
      IGenericClient genericClient,
      FhirContext context,
      String resourceName,
      String code,
      String system) {
    IBaseBundle bundleResponse = null;
    if (Boolean.TRUE.equals(checkSkipResource(resourceName, (FhirClient) genericClient))) {
      return bundleResponse;
    }
    String url =
        authDetails.getEhrServerURL()
            + "/"
            + resourceName
            + QUERY_PATIENT
            + authDetails.getLaunchPatientId()
            + "&code="
            + system
            + "|"
            + code;
    bundleResponse = getResourceBundleByUrl(authDetails, genericClient, context, resourceName, url);
    return bundleResponse;
  }

  public static IBaseBundle getResourceBundleByUrl(
      LaunchDetails authDetails,
      IGenericClient genericClient,
      FhirContext context,
      String resourceName,
      String url) {

    IBaseBundle bundleResponse = null;
    try {
      logger.info(
          "Getting {} data using Patient Id {} by URL {}",
          resourceName,
          StringEscapeUtils.escapeJava(authDetails.getLaunchPatientId()),
          StringEscapeUtils.escapeJava(url));
      if (authDetails.getFhirVersion().equalsIgnoreCase(DSTU2)) {
        Bundle bundle = genericClient.search().byUrl(url).returnBundle(Bundle.class).execute();
        getAllDSTU2RecordsUsingPagination(genericClient, bundle);
        if (bundle != null && bundle.getEntry() != null) {
          logger.info(
              "Total No of {} received::::::::::::::::: {}",
              resourceName,
              bundle.getEntry().size());
        }
        bundleResponse = bundle;
      } else if (authDetails.getFhirVersion().equalsIgnoreCase(R4)) {
        org.hl7.fhir.r4.model.Bundle bundle =
            genericClient
                .search()
                .byUrl(url)
                .returnBundle(org.hl7.fhir.r4.model.Bundle.class)
                .execute();
        getAllR4RecordsUsingPagination(genericClient, bundle);
        if (bundle != null && bundle.getEntry() != null) {
          logger.info(
              "Total No of {} received::::::::::::::::: {}",
              resourceName,
              bundle.getEntry().size());
        }
        bundleResponse = bundle;
      }
    } catch (ForbiddenOperationException scopeException) {
      logger.info(
          "Failed getting {} resource by Patient Id: {}\n{}\nCurrent scope: {}",
          resourceName,
          authDetails.getLaunchPatientId(),
          scopeException.getMessage(),
          authDetails.getScope());
    } catch (BaseServerResponseException responseException) {
      if (responseException.getOperationOutcome() != null) {
        logger.debug(
            context
                .newJsonParser()
                .encodeResourceToString(responseException.getOperationOutcome()));
      }
      logger.info(
          "Error in getting {} resource by Patient Id: {}",
          resourceName,
          authDetails.getLaunchPatientId(),
          responseException);
    } catch (Exception e) {
      logger.info(
          "Error in getting {} resource by Patient Id: {}",
          resourceName,
          authDetails.getLaunchPatientId(),
          e);
    }

    return bundleResponse;
  }

  private static void getAllR4RecordsUsingPagination(
      IGenericClient genericClient, org.hl7.fhir.r4.model.Bundle bundle) {
    if (bundle != null && bundle.hasEntry()) {
      List<BundleEntryComponent> entriesList = bundle.getEntry();
      if (bundle.hasLink() && bundle.getLink(IBaseBundle.LINK_NEXT) != null) {
        logger.info(
            "Found Next Page in Bundle:::::{}", bundle.getLink(IBaseBundle.LINK_NEXT).getUrl());
        org.hl7.fhir.r4.model.Bundle nextPageBundleResults =
            genericClient.loadPage().next(bundle).execute();
        if (nextPageBundleResults != null) {
          entriesList.addAll(nextPageBundleResults.getEntry());
          nextPageBundleResults.setEntry(entriesList);
          getAllR4RecordsUsingPagination(genericClient, nextPageBundleResults);
        }
      }
    }
  }

  private static void getAllDSTU2RecordsUsingPagination(
      IGenericClient genericClient, Bundle bundle) {
    if (bundle != null && bundle.getEntry() != null) {
      List<Entry> entriesList = bundle.getEntry();
      if (bundle.getLink(IBaseBundle.LINK_NEXT) != null) {
        logger.info(
            "Found Next Page in Bundle:::::{}", bundle.getLink(IBaseBundle.LINK_NEXT).getUrl());
        Bundle nextPageBundleResults = genericClient.loadPage().next(bundle).execute();
        if (nextPageBundleResults != null) {
          entriesList.addAll(nextPageBundleResults.getEntry());
          nextPageBundleResults.setEntry(entriesList);
          getAllDSTU2RecordsUsingPagination(genericClient, nextPageBundleResults);
        }
      }
    }
  }

  private String getCustomQueryParameters(
      String resourceName, LaunchDetails launchDetails, FhirClient client) {
    String customQueryParam = "";

    if (Boolean.TRUE.equals(pagingCountEnabled) && resourceName.matches(pagingResource)) {
      customQueryParam += "&_count=" + pagingCount;
    }

    if (Boolean.TRUE.equals(queryByDateEnabled)) {
      String queryDateTime = getQueryDateTime(launchDetails, client);

      if (resourceName.matches(dateResources)) {
        customQueryParam += "&date=ge" + queryDateTime;
      } else if (resourceName.matches(lastUpdatedResources)) {
        customQueryParam += "&_lastUpdated=ge" + queryDateTime + "T00:00:00.000Z";
      }
    }

    if (Boolean.TRUE.equals(queryByEncounterEnabled)
        && resourceName.matches(resourcesByEncounter)) {
      customQueryParam += "&encounter=" + launchDetails.getEncounterId();
    }

    if (resourceName.equalsIgnoreCase("Condition")) {
      customQueryParam +=
          "&category=problem-list-item,encounter-diagnosis&clinical-status=active,recurrence,relapse";
    }

    return customQueryParam;
  }

  protected boolean checkSkipResource(String resourceName, FhirClient client) {
    if (!skipResource.isEmpty() && skipResource.contains(resourceName)) {
      logger.info("Resource {} is not called as it is configured to Skip", resourceName);
      return true;
    }
    if (client.queryType.equals(EventTypes.QueryType.TRIGGER_QUERY)) {
      return checkSkipTriggerResource(resourceName);
    }
    return false;
  }

  private boolean checkSkipTriggerResource(String resourceName) {
    if (!skipTriggerResource.isEmpty() && skipTriggerResource.contains(resourceName)) {
      logger.info(
          "Resource {} is not called as it is configured to Skip for TriggerQuery", resourceName);
      return true;
    }
    return false;
  }

  private String getQueryDateTime(LaunchDetails launchDetails, FhirClient client) {

    SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
    String queryStartDate = formatter.format(launchDetails.getStartDate());

    if (Boolean.TRUE.equals(queryByLastTriggerDateEnabled)
        && client.queryType.equals(EventTypes.QueryType.TRIGGER_QUERY)) {
      PatientExecutionState state = ApplicationUtils.getDetailStatus(launchDetails);
      if (state != null) {
        Date lastQueryDtTm = state.getMatchTriggerStatus().getTriggerLastExecutionDateTime();
        if (lastQueryDtTm != null) {
          queryStartDate = formatter.format(lastQueryDtTm);
        }
      }
    }
    return queryStartDate;
  }
}
