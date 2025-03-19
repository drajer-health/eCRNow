package com.drajer.ecrapp.config;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.interceptor.BearerTokenAuthInterceptor;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplateConfig;
import com.drajer.ecrapp.fhir.utils.ecrretry.RetryStatusCode;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.Collections;
import java.util.concurrent.TimeUnit;
import okhttp3.OkHttpClient;
import org.cqframework.cql.cql2elm.LibraryBuilder;
import org.cqframework.cql.cql2elm.LibrarySourceProvider;
import org.hl7.fhir.r4.model.Bundle;
import org.opencds.cqf.fhir.cql.EvaluationSettings;
import org.opencds.cqf.fhir.cql.cql2elm.content.RepositoryFhirLibrarySourceProvider;
import org.opencds.cqf.fhir.cql.cql2elm.util.LibraryVersionSelector;
import org.opencds.cqf.fhir.cql.engine.retrieve.RetrieveSettings;
import org.opencds.cqf.fhir.cql.engine.terminology.TerminologySettings;
import org.opencds.cqf.fhir.cr.cpg.r4.R4CqlExecutionService;
import org.opencds.cqf.fhir.cr.cpg.r4.R4LibraryEvaluationService;
import org.opencds.cqf.fhir.cr.measure.MeasureEvaluationOptions;
import org.opencds.cqf.fhir.cr.measure.common.MeasurePeriodValidator;
import org.opencds.cqf.fhir.cr.measure.r4.R4MeasureService;
import org.opencds.cqf.fhir.cr.measure.r4.R4RepositorySubjectProvider;
import org.opencds.cqf.fhir.cr.spring.EvaluatorConfiguration;
import org.opencds.cqf.fhir.utility.adapter.r4.AdapterFactory;
import org.opencds.cqf.fhir.utility.repository.FederatedRepository;
import org.opencds.cqf.fhir.utility.repository.InMemoryFhirRepository;
import org.opencds.cqf.fhir.utility.repository.RestRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.context.annotation.*;
import org.springframework.retry.support.RetryTemplate;

@ComponentScan(
    basePackages = {
      "com.drajer.ecrapp",
      "com.drajer.ersd",
      "com.drajer.sof",
      "com.drajer.eca",
      "com.drajer.routing",
      "com.drajer.cda",
      "com.drajer.bsa.utils",
      "com.drajer.bsa.kar.action",
      "com.drajer.bsa.kar.model",
      "com.drajer.bsa.service",
      "com.drajer.bsa.ehr.service",
      "com.drajer.bsa.ehr.customizations",
      "com.drajer.bsa.routing",
      "com.drajer.bsa.interfaces",
      "com.drajer.bsa"
    })
@Import(EvaluatorConfiguration.class)
@Configuration
@EnableAutoConfiguration(exclude = HibernateJpaAutoConfiguration.class)
public class SpringConfiguration {

  @Autowired RetryStatusCode retryStatusCode;
  public static final String ERSD_FHIR_BASE_SERVER = "https://ersd.aimsplatform.org/api/fhir";

  public static final String AUTHORIZATION_TOKEN =
      "d94874a5b6d848ae921e75b9bf202feb97905791ff890a6e189614053a8032c6f298662299dea42df6cef59fde";

  public static final FhirContext ctx = FhirContext.forR4();

  @Autowired FHIRRetryTemplateConfig fhirRetryTemplateConfig;

  public void setFhirRetryTemplateConfig(FHIRRetryTemplateConfig fhirRetryTemplateConfig) {
    this.fhirRetryTemplateConfig = fhirRetryTemplateConfig;
  }

  @Bean
  @Primary
  FederatedRepository getEcrRepository(
      InMemoryFhirRepository artifactRepository, RestRepository ehrRepository) {
    return new FederatedRepository(artifactRepository, ehrRepository);
  }

  @Bean
  RestRepository getEhrRepository(IGenericClient esrdGenericClient) {
    return new RestRepository(esrdGenericClient);
  }

  @Bean
  InMemoryFhirRepository getArtifactRepository(FhirContext fhirContext)
      throws FileNotFoundException {
    return new InMemoryFhirRepository(fhirContext, readErsdBundleFromFile());
  }

  @Bean
  MeasureEvaluationOptions getMeasureEvaluationOptions(EvaluationSettings evaluationSettings) {
    var options = MeasureEvaluationOptions.defaultOptions();
    options.setEvaluationSettings(evaluationSettings);
    return options;
  }

  @Bean
  EvaluationSettings getEvaluationSettings(LibrarySourceProvider librarySourceProvider) {
    EvaluationSettings evaluationSettings =
        MeasureEvaluationOptions.defaultOptions().getEvaluationSettings();
    evaluationSettings
        .getCqlOptions()
        .getCqlCompilerOptions()
        .setSignatureLevel(LibraryBuilder.SignatureLevel.Overloads);
    evaluationSettings
        .getTerminologySettings()
        .setValuesetPreExpansionMode(TerminologySettings.VALUESET_PRE_EXPANSION_MODE.USE_IF_PRESENT)
        .setValuesetExpansionMode(
            TerminologySettings.VALUESET_EXPANSION_MODE.PERFORM_NAIVE_EXPANSION)
        .setValuesetMembershipMode(TerminologySettings.VALUESET_MEMBERSHIP_MODE.USE_EXPANSION)
        .setCodeLookupMode(TerminologySettings.CODE_LOOKUP_MODE.USE_CODESYSTEM_URL);
    evaluationSettings
        .getRetrieveSettings()
        .setTerminologyParameterMode(RetrieveSettings.TERMINOLOGY_FILTER_MODE.FILTER_IN_MEMORY)
        .setSearchParameterMode(RetrieveSettings.SEARCH_FILTER_MODE.FILTER_IN_MEMORY)
        .setProfileMode(RetrieveSettings.PROFILE_MODE.DECLARED);
    evaluationSettings.withLibrarySourceProviders(Collections.singletonList(librarySourceProvider));

    return evaluationSettings;
  }

  @Bean
  R4RepositorySubjectProvider getR4RepositorySubjectProvider() {
    return new R4RepositorySubjectProvider();
  }

  @Bean
  R4MeasureService getMeasureService(
      FederatedRepository ecrRepository, MeasureEvaluationOptions measureEvaluationOptions) {
    return new R4MeasureService(
        ecrRepository, measureEvaluationOptions, new MeasurePeriodValidator());
  }

  @Bean
  R4CqlExecutionService getExecutionService(
      FederatedRepository ecrRepository, EvaluationSettings evaluationSettings) {
    return new R4CqlExecutionService(ecrRepository, evaluationSettings);
  }

  @Bean
  R4LibraryEvaluationService getLibraryEvaluationService(
      FederatedRepository ecrRepository, EvaluationSettings evaluationSettings) {
    return new R4LibraryEvaluationService(ecrRepository, evaluationSettings);
  }

  @Bean
  LibrarySourceProvider getLibrarySourceProvider(InMemoryFhirRepository artifactRepository) {
    return new RepositoryFhirLibrarySourceProvider(
        artifactRepository, new AdapterFactory(), new LibraryVersionSelector(new AdapterFactory()));
  }

  @Bean(name = "esrdGenericClient")
  public IGenericClient getEsrdFhirContext() {
    BearerTokenAuthInterceptor authInterceptor =
        new BearerTokenAuthInterceptor(AUTHORIZATION_TOKEN);
    IGenericClient genericClient = ctx.newRestfulGenericClient(ERSD_FHIR_BASE_SERVER);
    genericClient.registerInterceptor(authInterceptor);
    return genericClient;
  }

  @Bean(name = "jsonParser")
  public IParser getEsrdJsonParser() {
    return ctx.newJsonParser().setPrettyPrint(true);
  }

  @Bean(name = "ECRRetryTemplate")
  public RetryTemplate retryTemplate() {

    RetryTemplate template = retryStatusCode.configureRetryTemplate();

    return template;
  }

  @Value("${ersd.file.location:default.json}")
  String ersdFileLocation;

  private Bundle readErsdBundleFromFile() throws FileNotFoundException {

    Bundle bundle = null;
    InputStream in = new FileInputStream(new File(ersdFileLocation));

    bundle = getEsrdJsonParser().parseResource(Bundle.class, in);

    return bundle;
  }

  // Needed for the evaluator configuration
  @Bean
  public FhirContext fhirContext() {
    return ctx;
  }

  @Bean
  public OkHttpClient okHttpClient() {
    return new OkHttpClient.Builder()
        .connectTimeout(30, TimeUnit.SECONDS)
        .readTimeout(300, TimeUnit.SECONDS)
        .writeTimeout(300, TimeUnit.SECONDS)
        .build();
  }
}
