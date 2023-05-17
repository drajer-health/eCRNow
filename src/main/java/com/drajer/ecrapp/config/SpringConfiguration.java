package com.drajer.ecrapp.config;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.interceptor.BearerTokenAuthInterceptor;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplate;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplateConfig;
import com.drajer.ecrapp.fhir.utils.ecrretry.RetryStatusCode;

import java.util.function.Supplier;

import com.drajer.sof.utils.FhirContextInitializer;
import org.opencds.cqf.cql.evaluator.builder.CqlEvaluatorBuilder;
import org.opencds.cqf.cql.evaluator.builder.DataProviderFactory;
import org.opencds.cqf.cql.evaluator.builder.EndpointConverter;
import org.opencds.cqf.cql.evaluator.builder.FhirDalFactory;
import org.opencds.cqf.cql.evaluator.builder.ModelResolverFactory;
import org.opencds.cqf.cql.evaluator.builder.TerminologyProviderFactory;
import org.opencds.cqf.cql.evaluator.builder.library.LibraryContentProviderFactory;
import org.opencds.cqf.cql.evaluator.expression.ExpressionEvaluator;
import org.opencds.cqf.cql.evaluator.library.CqlFhirParametersConverter;
import org.opencds.cqf.cql.evaluator.library.LibraryProcessor;
import org.opencds.cqf.cql.evaluator.measure.r4.R4MeasureProcessor;
import org.opencds.cqf.cql.evaluator.spring.EvaluatorConfiguration;
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
@Import({ EvaluatorConfiguration.class, FHIRRetryTemplateConfig.class })
@Configuration
@EnableAutoConfiguration(exclude = HibernateJpaAutoConfiguration.class)
public class SpringConfiguration {
  public static final String ERSD_FHIR_BASE_SERVER = "https://ersd.aimsplatform.org/api/fhir";

  public static final String AUTHORIZATION_TOKEN =
      "d94874a5b6d848ae921e75b9bf202feb97905791ff890a6e189614053a8032c6f298662299dea42df6cef59fde";

  // Needed for the evaluator configuration
  @Bean
  public FhirContext fhirContext() {
    return FhirContext.forCached(FhirVersionEnum.R4);
  }

  @Bean
  public FhirContextInitializer fhirContextInitializer(FHIRRetryTemplate fhirRetryTemplate) {
    return new FhirContextInitializer(fhirRetryTemplate);
  }

  @Bean(name = "esrdGenericClient")
  public IGenericClient getEsrdFhirContext(FhirContext ctx) {
    BearerTokenAuthInterceptor authInterceptor =
        new BearerTokenAuthInterceptor(AUTHORIZATION_TOKEN);
    IGenericClient genericClient = ctx.newRestfulGenericClient(ERSD_FHIR_BASE_SERVER);
    genericClient.registerInterceptor(authInterceptor);
    return genericClient;
  }

  @Bean(name = "jsonParser")
  public IParser getEsrdJsonParser(FhirContext ctx) {
    return ctx.newJsonParser().setPrettyPrint(true);
  }

  @Bean(name = "ECRRetryTemplate")
  public RetryTemplate retryTemplate(RetryStatusCode retryStatusCode) {
    return retryStatusCode.configureRetryTemplate();
  }

  @Bean
  @Scope("prototype")
  public ExpressionEvaluator expressionEvaluator(
          FhirContext fhirContext, CqlFhirParametersConverter cqlFhirParametersConverter,
          LibraryContentProviderFactory libraryContentProviderFactory, DataProviderFactory dataProviderFactory,
          TerminologyProviderFactory terminologyProviderFactory, EndpointConverter endpointConverter,
          ModelResolverFactory modelResolverFactory, Supplier<CqlEvaluatorBuilder> cqlEvaluatorBuilderSupplier) {
    return new ExpressionEvaluator(fhirContext, cqlFhirParametersConverter, libraryContentProviderFactory,
            dataProviderFactory, terminologyProviderFactory, endpointConverter, modelResolverFactory,
            cqlEvaluatorBuilderSupplier);
  }

  @Bean
  @Scope("prototype")
  public LibraryProcessor libraryProcessor(
          FhirContext fhirContext, CqlFhirParametersConverter cqlFhirParametersConverter,
          LibraryContentProviderFactory libraryContentProviderFactory, DataProviderFactory dataProviderFactory,
          TerminologyProviderFactory terminologyProviderFactory, EndpointConverter endpointConverter,
          ModelResolverFactory modelResolverFactory, Supplier<CqlEvaluatorBuilder> cqlEvaluatorBuilderSupplier
  ) {
    return new LibraryProcessor(fhirContext, cqlFhirParametersConverter, libraryContentProviderFactory,
            dataProviderFactory, terminologyProviderFactory, endpointConverter, modelResolverFactory,
            cqlEvaluatorBuilderSupplier);
  }

  @Bean
  @Scope("prototype")
  public R4MeasureProcessor measureProcessor(
          TerminologyProviderFactory terminologyProviderFactory, DataProviderFactory dataProviderFactory,
          LibraryContentProviderFactory libraryContentProviderFactory, FhirDalFactory fhirDalFactory,
          EndpointConverter endpointConverter) {
    return new R4MeasureProcessor(terminologyProviderFactory, dataProviderFactory,
            libraryContentProviderFactory, fhirDalFactory, endpointConverter);
  }
}
