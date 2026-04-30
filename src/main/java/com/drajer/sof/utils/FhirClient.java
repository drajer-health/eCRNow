package com.drajer.sof.utils;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.interceptor.api.IInterceptorService;
import ca.uhn.fhir.model.primitive.IdDt;
import ca.uhn.fhir.model.primitive.UriDt;
import ca.uhn.fhir.rest.api.EncodingEnum;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.api.RequestFormatParamStyleEnum;
import ca.uhn.fhir.rest.api.SummaryEnum;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.api.IHttpClient;
import ca.uhn.fhir.rest.gclient.*;
import com.drajer.eca.model.EventTypes;
import javax.annotation.Nonnull;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IBaseResource;

/**
 * Wrapper class for HAPI FHIR IGenericClient that adds custom HTTP header interceptors and request
 * tracking capabilities.
 *
 * <p>This class implements deprecated methods from IGenericClient interface because we must
 * implement ALL interface methods. The warnings are documented with @Deprecated annotations to
 * guide callers toward modern alternatives.
 *
 * <p><b>Why we implement deprecated methods:</b> - This is a wrapper/proxy pattern - we must
 * implement the entire interface - We delegate to the underlying client which still supports these
 * methods - Callers can use either old or new API through this wrapper - Migration to new API can
 * happen incrementally at the call sites
 *
 * @see IGenericClient
 * @see FhirHttpHeaderInterceptor
 */
@SuppressWarnings("deprecation")
public class FhirClient implements IGenericClient {
  protected IGenericClient client;
  protected FhirHttpHeaderInterceptor interceptor;
  protected EventTypes.QueryType queryType;

  public FhirClient(@Nonnull IGenericClient client, String requestId, EventTypes.QueryType type) {
    this.interceptor = new FhirHttpHeaderInterceptor(requestId);
    this.client = client;
    this.client.registerInterceptor(this.interceptor);
    this.queryType = type;
  }

  public FhirHttpHeaderInterceptor getHttpInterceptor() {
    return this.interceptor;
  }

  @Override
  public IFetchConformanceUntyped capabilities() {
    this.interceptor.reset();
    return this.client.capabilities();
  }

  @Override
  public ICreate create() {
    this.interceptor.reset();
    return this.client.create();
  }

  @Override
  public IDelete delete() {
    this.interceptor.reset();
    return this.client.delete();
  }

  @Override
  public IFetchConformanceUntyped fetchConformance() {
    this.interceptor.reset();
    return this.client.capabilities();
  }

  @Override
  public void forceConformanceCheck() {
    this.interceptor.reset();
    this.client.forceConformanceCheck();
  }

  @Override
  public IHistory history() {
    this.interceptor.reset();
    return this.client.history();
  }

  @Override
  public IGetPage loadPage() {
    this.interceptor.incrementPageNum();
    return this.client.loadPage();
  }

  @Override
  public IMeta meta() {
    this.interceptor.reset();
    return this.client.meta();
  }

  @Override
  public IOperation operation() {
    this.interceptor.reset();
    return this.client.operation();
  }

  @Override
  public IPatch patch() {
    this.interceptor.reset();
    return this.client.patch();
  }

  @Override
  public IRead read() {
    this.interceptor.reset();
    return this.client.read();
  }

  @Override
  public <T extends IBaseResource> T read(Class<T> aClass, String s) {
    this.interceptor.reset();
    return this.client.read(aClass, s);
  }

  @Deprecated(since = "HAPI FHIR 8.0", forRemoval = true)
  @Override
  public <T extends IBaseResource> T read(Class<T> aClass, UriDt uriDt) {
    this.interceptor.reset();
    return this.client.read(aClass, uriDt);
  }

  @Override
  public IBaseResource read(UriDt uriDt) {
    this.interceptor.reset();
    return this.client.read(uriDt);
  }

  @Override
  public IInterceptorService getInterceptorService() {
    return this.client.getInterceptorService();
  }

  @Override
  public void setInterceptorService(@Nonnull IInterceptorService iInterceptorService) {
    this.client.setInterceptorService(iInterceptorService);
  }

  @Override
  public <T extends IBaseResource> T fetchResourceFromUrl(Class<T> aClass, String s) {
    this.interceptor.reset();
    return this.client.fetchResourceFromUrl(aClass, s);
  }

  @Override
  public EncodingEnum getEncoding() {
    return this.client.getEncoding();
  }

  @Override
  public void setEncoding(EncodingEnum encodingEnum) {
    this.client.setEncoding(encodingEnum);
  }

  @Override
  public FhirContext getFhirContext() {
    return this.client.getFhirContext();
  }

  @Override
  public IHttpClient getHttpClient() {
    return this.client.getHttpClient();
  }

  @Override
  public String getServerBase() {
    return this.client.getServerBase();
  }

  @Override
  public void registerInterceptor(Object o) {
    this.client.registerInterceptor(o);
  }

  @Override
  public void setPrettyPrint(Boolean aBoolean) {
    this.client.setPrettyPrint(aBoolean);
  }

  @Override
  public void setSummary(SummaryEnum summaryEnum) {
    this.client.setSummary(summaryEnum);
  }

  @Override
  public <T extends IBaseBundle> IUntypedQuery<T> search() {
    this.interceptor.reset();
    return this.client.search();
  }

  @Override
  public void setLogRequestAndResponse(boolean b) {
    this.client.setLogRequestAndResponse(b);
  }

  @Override
  public ITransaction transaction() {
    this.interceptor.reset();
    return this.client.transaction();
  }

  @Override
  public void unregisterInterceptor(Object o) {
    this.client.unregisterInterceptor(o);
  }

  @Override
  public void setFormatParamStyle(RequestFormatParamStyleEnum requestFormatParamStyleEnum) {
    this.client.setFormatParamStyle(requestFormatParamStyleEnum);
  }

  @Override
  public IUpdate update() {
    this.interceptor.reset();
    return this.client.update();
  }

  @Override
  public MethodOutcome update(IdDt idDt, IBaseResource iBaseResource) {
    this.interceptor.reset();
    return this.client.update(idDt, iBaseResource);
  }

  @Override
  public MethodOutcome update(String theId, IBaseResource iBaseResource) {
    this.interceptor.reset();
    return this.client.update(theId, iBaseResource);
  }

  @Override
  public IValidate validate() {
    this.interceptor.reset();
    return this.client.validate();
  }

  @Override
  public MethodOutcome validate(IBaseResource iBaseResource) {
    this.interceptor.reset();
    return this.client.validate(iBaseResource);
  }

  @Override
  public <T extends IBaseResource> T vread(Class<T> theType, IdDt idDt) {
    this.interceptor.reset();
    return this.client.vread(theType, idDt);
  }

  @Override
  public <T extends IBaseResource> T vread(Class<T> theType, String theId, String theVersionId) {
    this.interceptor.reset();
    return this.client.vread(theType, theId, theVersionId);
  }

  /**
   * Execute a raw HTTP request against the FHIR server. This method allows for custom HTTP
   * operations not covered by standard FHIR operations.
   *
   * @return IRawHttp builder for constructing raw HTTP requests
   */
  @Override
  public IRawHttp rawHttpRequest() {
    this.interceptor.reset();
    return this.client.rawHttpRequest();
  }
}
