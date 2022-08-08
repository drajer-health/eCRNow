package com.drajer.ecrapp.fhir.utils.ecrretry;

import ca.uhn.fhir.rest.api.CacheControlDirective;
import ca.uhn.fhir.rest.api.EncodingEnum;
import ca.uhn.fhir.rest.api.SummaryEnum;
import ca.uhn.fhir.rest.gclient.IGetPage;
import ca.uhn.fhir.rest.gclient.IGetPageTyped;
import ca.uhn.fhir.rest.gclient.IGetPageUntyped;
import java.util.List;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IBaseResource;

public class EcrRetryablePage implements IGetPage, IGetPageTyped<IBaseBundle> {

  private IGetPage page;
  private IGetPageTyped<IBaseBundle> pageTyped;
  private EcrFhirClient client;

  public EcrRetryablePage(final IGetPage page, final EcrFhirClient client) {
    this.page = page;
    this.client = client;
  }

  public EcrRetryablePage(
      final IGetPage page, final IGetPageTyped<IBaseBundle> pageTyped, final EcrFhirClient client) {
    this.page = page;
    this.pageTyped = pageTyped;
    this.client = client;
  }

  @Override
  public IGetPageTyped<IBaseBundle> andLogRequestAndResponse(boolean theLogRequestAndResponse) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageTyped<IBaseBundle> cacheControl(CacheControlDirective theCacheControlDirective) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageTyped<IBaseBundle> elementsSubset(String... theElements) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageTyped<IBaseBundle> encoded(EncodingEnum theEncoding) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageTyped<IBaseBundle> encodedJson() {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageTyped<IBaseBundle> encodedXml() {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageTyped<IBaseBundle> withAdditionalHeader(
      String theHeaderName, String theHeaderValue) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IBaseBundle execute() {
    return client
        .getRetryTemplate()
        .execute(
            retryContext -> {
              client.getHttpInterceptor().setRetryCount(retryContext.getRetryCount());

              try {
                return pageTyped.execute();

              } catch (final Exception e) {
                // TODO
              }
              return null;
            },
            null); // TODO recover
  }

  @Override
  public IGetPageTyped<IBaseBundle> preferResponseType(Class<? extends IBaseResource> theType) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageTyped<IBaseBundle> preferResponseTypes(
      List<Class<? extends IBaseResource>> theTypes) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageTyped<IBaseBundle> prettyPrint() {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageTyped<IBaseBundle> summaryMode(SummaryEnum theSummary) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageTyped<IBaseBundle> accept(String theHeaderValue) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends IBaseBundle> IGetPageTyped<T> next(T theBundle) {
    pageTyped = page.next(theBundle);
    return (IGetPageTyped<T>) new EcrRetryablePage(page, pageTyped, client);
  }

  @Override
  public <T extends IBaseBundle> IGetPageTyped<T> previous(T theBundle) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IGetPageUntyped byUrl(String thePageUrl) {
    // TODO Auto-generated method stub
    return null;
  }
}
