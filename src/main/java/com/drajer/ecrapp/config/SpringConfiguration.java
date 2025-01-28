package com.drajer.ecrapp.config;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.interceptor.BearerTokenAuthInterceptor;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplateConfig;
import com.drajer.ecrapp.fhir.utils.ecrretry.RetryStatusCode;
import com.drajer.ecrapp.repository.EcrRepository;
import org.hl7.fhir.r4.model.Bundle;
import org.opencds.cqf.fhir.api.Repository;
import org.opencds.cqf.fhir.cql.EvaluationSettings;
import org.opencds.cqf.fhir.cr.cpg.r4.R4CqlExecutionService;
import org.opencds.cqf.fhir.cr.cpg.r4.R4LibraryEvaluationService;
import org.opencds.cqf.fhir.cr.measure.MeasureEvaluationOptions;
import org.opencds.cqf.fhir.cr.measure.common.MeasurePeriodValidator;
import org.opencds.cqf.fhir.cr.measure.r4.R4MeasureService;
import org.opencds.cqf.fhir.cr.measure.r4.R4RepositorySubjectProvider;
import org.opencds.cqf.fhir.cr.spring.EvaluatorConfiguration;
import org.opencds.cqf.fhir.utility.repository.InMemoryFhirRepository;
import org.opencds.cqf.fhir.utility.repository.RestRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.retry.support.RetryTemplate;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

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
  Repository getEhrRepository() throws FileNotFoundException {
    return new InMemoryFhirRepository(fhirContext(), readErsdBundleFromFile());
    //return new RestRepository(getEsrdFhirContext());
  }

  @Bean
  MeasureEvaluationOptions getMeasureEvaluationOptions() {
    return new MeasureEvaluationOptions();
  }

  @Bean
  R4RepositorySubjectProvider getR4RepositorySubjectProvider() {
    return new R4RepositorySubjectProvider();
  }

  @Bean
  R4MeasureService getMeasureService() throws FileNotFoundException {
    return new R4MeasureService(
        getEhrRepository(), new MeasureEvaluationOptions(), new MeasurePeriodValidator());
  }

  @Bean
  R4CqlExecutionService getExecutionService() throws FileNotFoundException {
    return new R4CqlExecutionService(getEhrRepository(), new EvaluationSettings());
  }

  @Bean
  R4LibraryEvaluationService getLibraryEvaluationService() throws FileNotFoundException {
    return new R4LibraryEvaluationService(getEhrRepository(), new EvaluationSettings());
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
}
