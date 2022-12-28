package com.drajer.ecrapp.fhir.utils.ecrretry;

import ca.uhn.fhir.rest.api.CacheControlDirective;
import ca.uhn.fhir.rest.api.EncodingEnum;
import ca.uhn.fhir.rest.api.SummaryEnum;
import ca.uhn.fhir.rest.gclient.IGetPage;
import ca.uhn.fhir.rest.gclient.IGetPageTyped;
import ca.uhn.fhir.rest.gclient.IGetPageUntyped;
import ca.uhn.fhir.rest.server.exceptions.NotImplementedOperationException;
import java.util.List;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpMethod;

public class EcrFhirRetryablePage implements IGetPage, IGetPageTyped<IBaseBundle> {

  private static final Logger logger = LoggerFactory.getLogger(EcrFhirRetryablePage.class);
  private IGetPage page;
  private IGetPageTyped<IBaseBundle> pageTyped;
  private EcrFhirRetryClient client;

  public EcrFhirRetryablePage(final IGetPage page, final EcrFhirRetryClient client) {
    this.page = page;
    this.client = client;
  }

  public EcrFhirRetryablePage(
      final IGetPage page,
      final IGetPageTyped<IBaseBundle> pageTyped,
      final EcrFhirRetryClient client) {
    this.page = page;
    this.pageTyped = pageTyped;
    this.client = client;
  }

  @Override
  public IGetPageTyped<IBaseBundle> andLogRequestAndResponse(boolean theLogRequestAndResponse) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageTyped<IBaseBundle> cacheControl(CacheControlDirective theCacheControlDirective) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageTyped<IBaseBundle> elementsSubset(String... theElements) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageTyped<IBaseBundle> encoded(EncodingEnum theEncoding) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageTyped<IBaseBundle> encodedJson() {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageTyped<IBaseBundle> encodedXml() {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageTyped<IBaseBundle> withAdditionalHeader(
      String theHeaderName, String theHeaderValue) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IBaseBundle execute() {
    return client
        .getRetryTemplate()
        .execute(
            retryContext -> {
              try {
                client.getHttpInterceptor().setRetryCount(retryContext.getRetryCount());
                logger.info("Retrying FHIR page. Count: {}", retryContext.getRetryCount());
                return pageTyped.execute();
              } catch (final Exception ex) {
                throw client.handleException(ex, HttpMethod.GET.name());
              }
            },
            null);
  }

  @Override
  public IGetPageTyped<IBaseBundle> preferResponseType(Class<? extends IBaseResource> theType) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageTyped<IBaseBundle> preferResponseTypes(
      List<Class<? extends IBaseResource>> theTypes) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageTyped<IBaseBundle> prettyPrint() {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageTyped<IBaseBundle> summaryMode(SummaryEnum theSummary) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageTyped<IBaseBundle> accept(String theHeaderValue) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public <T extends IBaseBundle> IGetPageTyped<T> next(T theBundle) {
    pageTyped = page.next(theBundle);
    return (IGetPageTyped<T>) new EcrFhirRetryablePage(page, pageTyped, client);
  }

  @Override
  public <T extends IBaseBundle> IGetPageTyped<T> previous(T theBundle) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IGetPageUntyped byUrl(String thePageUrl) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }
}
