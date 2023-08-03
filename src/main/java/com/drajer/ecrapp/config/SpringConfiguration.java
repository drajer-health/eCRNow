package com.drajer.ecrapp.config;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.interceptor.BearerTokenAuthInterceptor;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplateConfig;
import com.drajer.ecrapp.fhir.utils.ecrretry.RetryStatusCode;
import java.util.Collections;
import java.util.function.Supplier;
import org.opencds.cqf.cql.engine.fhir.converter.FhirTypeConverter;
import org.opencds.cqf.cql.engine.fhir.converter.FhirTypeConverterFactory;
import org.opencds.cqf.cql.evaluator.builder.CqlEvaluatorBuilder;
import org.opencds.cqf.cql.evaluator.builder.DataProviderFactory;
import org.opencds.cqf.cql.evaluator.builder.EndpointConverter;
import org.opencds.cqf.cql.evaluator.builder.FhirDalFactory;
import org.opencds.cqf.cql.evaluator.builder.LibrarySourceProviderFactory;
import org.opencds.cqf.cql.evaluator.builder.ModelResolverFactory;
import org.opencds.cqf.cql.evaluator.builder.TerminologyProviderFactory;
import org.opencds.cqf.cql.evaluator.builder.dal.FhirRestFhirDalFactory;
import org.opencds.cqf.cql.evaluator.builder.dal.TypedFhirDalFactory;
import org.opencds.cqf.cql.evaluator.builder.data.FhirModelResolverFactory;
import org.opencds.cqf.cql.evaluator.builder.data.FhirRestRetrieveProviderFactory;
import org.opencds.cqf.cql.evaluator.builder.data.TypedRetrieveProviderFactory;
import org.opencds.cqf.cql.evaluator.builder.library.FhirRestLibrarySourceProviderFactory;
import org.opencds.cqf.cql.evaluator.builder.library.TypedLibrarySourceProviderFactory;
import org.opencds.cqf.cql.evaluator.builder.terminology.FhirRestTerminologyProviderFactory;
import org.opencds.cqf.cql.evaluator.builder.terminology.TypedTerminologyProviderFactory;
import org.opencds.cqf.cql.evaluator.cql2elm.util.LibraryVersionSelector;
import org.opencds.cqf.cql.evaluator.expression.ExpressionEvaluator;
import org.opencds.cqf.cql.evaluator.fhir.ClientFactory;
import org.opencds.cqf.cql.evaluator.fhir.adapter.r4.AdapterFactory;
import org.opencds.cqf.cql.evaluator.library.CqlFhirParametersConverter;
import org.opencds.cqf.cql.evaluator.library.LibraryProcessor;
import org.opencds.cqf.cql.evaluator.measure.r4.R4MeasureProcessor;
import org.opencds.cqf.cql.evaluator.spring.EvaluatorConfiguration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Scope;
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

  // Needed for the evaluator configuration
  @Bean
  public FhirContext fhirContext() {
    return ctx;
  }

  @Bean
  public AdapterFactory adapterFactory() {
    return new AdapterFactory();
  }

  @Bean
  public FhirTypeConverter fhirTypeConverter(FhirContext fhirContext) {
    return new FhirTypeConverterFactory().create(fhirContext.getVersion().getVersion());
  }

  @Bean
  public ClientFactory clientFactory(FhirContext fhirContext) {
    return new ClientFactory(fhirContext);
  }

  @Bean
  public ModelResolverFactory modelResolverFactory() {
    return new FhirModelResolverFactory();
  }

  @Bean
  public CqlFhirParametersConverter cqlFhirParametersConverter(
      FhirContext fhirContext, AdapterFactory adapterFactory, FhirTypeConverter fhirTypeConverter) {
    return new CqlFhirParametersConverter(fhirContext, adapterFactory, fhirTypeConverter);
  }

  @Bean
  public EndpointConverter endpointConverter(AdapterFactory adapterFactory) {
    return new EndpointConverter(adapterFactory);
  }

  @Bean
  @Scope("prototype")
  public TerminologyProviderFactory terminologyProviderFactory(
      FhirContext fhirContext, ClientFactory clientFactory) {
    TypedTerminologyProviderFactory terminologyProviderFactory =
        new FhirRestTerminologyProviderFactory(fhirContext, clientFactory);
    return new org.opencds.cqf.cql.evaluator.builder.terminology.TerminologyProviderFactory(
        fhirContext, Collections.singleton(terminologyProviderFactory));
  }

  @Bean
  @Scope("prototype")
  public DataProviderFactory dataProviderFactory(
      FhirContext fhirContext,
      ClientFactory clientFactory,
      ModelResolverFactory modelResolverFactory) {
    TypedRetrieveProviderFactory typedRetrieveProviderFactory =
        new FhirRestRetrieveProviderFactory(fhirContext, clientFactory);
    return new org.opencds.cqf.cql.evaluator.builder.data.DataProviderFactory(
        fhirContext,
        Collections.singleton(modelResolverFactory),
        Collections.singleton(typedRetrieveProviderFactory));
  }

  @Bean
  @Scope("prototype")
  public LibrarySourceProviderFactory librarySourceProviderFactory(
      FhirContext fhirContext, ClientFactory clientFactory, AdapterFactory adapterFactory) {
    LibraryVersionSelector libraryVersionSelector = new LibraryVersionSelector(adapterFactory);
    TypedLibrarySourceProviderFactory typedLibrarySourceProviderFactory =
        new FhirRestLibrarySourceProviderFactory(
            clientFactory, adapterFactory, libraryVersionSelector);
    return new org.opencds.cqf.cql.evaluator.builder.library.LibrarySourceProviderFactory(
        fhirContext,
        adapterFactory,
        Collections.singleton(typedLibrarySourceProviderFactory),
        libraryVersionSelector);
  }

  @Bean
  @Scope("prototype")
  public FhirDalFactory fhirDalFactory(FhirContext fhirContext, ClientFactory clientFactory) {
    TypedFhirDalFactory typedFhirDalFactory = new FhirRestFhirDalFactory(clientFactory);
    return new org.opencds.cqf.cql.evaluator.builder.dal.FhirDalFactory(
        fhirContext, Collections.singleton(typedFhirDalFactory));
  }

  @Bean
  @Scope("prototype")
  public ExpressionEvaluator expressionEvaluator(
      FhirContext fhirContext,
      CqlFhirParametersConverter cqlFhirParametersConverter,
      LibrarySourceProviderFactory librarySourceProviderFactory,
      DataProviderFactory dataProviderFactory,
      TerminologyProviderFactory terminologyProviderFactory,
      EndpointConverter endpointConverter,
      ModelResolverFactory modelResolverFactory,
      Supplier<CqlEvaluatorBuilder> cqlEvaluatorBuilderSupplier) {
    return new ExpressionEvaluator(
        fhirContext,
        cqlFhirParametersConverter,
        librarySourceProviderFactory,
        dataProviderFactory,
        terminologyProviderFactory,
        endpointConverter,
        modelResolverFactory,
        cqlEvaluatorBuilderSupplier);
  }

  @Bean
  @Scope("prototype")
  public LibraryProcessor libraryProcessor(
      FhirContext fhirContext,
      CqlFhirParametersConverter cqlFhirParametersConverter,
      LibrarySourceProviderFactory librarySourceProviderFactory,
      DataProviderFactory dataProviderFactory,
      TerminologyProviderFactory terminologyProviderFactory,
      EndpointConverter endpointConverter,
      ModelResolverFactory modelResolverFactory,
      Supplier<CqlEvaluatorBuilder> cqlEvaluatorBuilderSupplier) {
    return new LibraryProcessor(
        fhirContext,
        cqlFhirParametersConverter,
        librarySourceProviderFactory,
        dataProviderFactory,
        terminologyProviderFactory,
        endpointConverter,
        modelResolverFactory,
        cqlEvaluatorBuilderSupplier);
  }

  @Bean
  @Scope("prototype")
  public R4MeasureProcessor measureProcessor(
      TerminologyProviderFactory terminologyProviderFactory,
      DataProviderFactory dataProviderFactory,
      LibrarySourceProviderFactory librarySourceProviderFactory,
      FhirDalFactory fhirDalFactory,
      EndpointConverter endpointConverter) {
    return new R4MeasureProcessor(
        terminologyProviderFactory,
        dataProviderFactory,
        librarySourceProviderFactory,
        fhirDalFactory,
        endpointConverter);
  }
}
