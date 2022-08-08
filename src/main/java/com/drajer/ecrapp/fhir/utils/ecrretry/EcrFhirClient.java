package com.drajer.ecrapp.fhir.utils.ecrretry;

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
import ca.uhn.fhir.rest.client.exceptions.FhirClientConnectionException;
import ca.uhn.fhir.rest.gclient.ICreate;
import ca.uhn.fhir.rest.gclient.IDelete;
import ca.uhn.fhir.rest.gclient.IFetchConformanceUntyped;
import ca.uhn.fhir.rest.gclient.IGetPage;
import ca.uhn.fhir.rest.gclient.IHistory;
import ca.uhn.fhir.rest.gclient.IMeta;
import ca.uhn.fhir.rest.gclient.IOperation;
import ca.uhn.fhir.rest.gclient.IPatch;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.ITransaction;
import ca.uhn.fhir.rest.gclient.IUntypedQuery;
import ca.uhn.fhir.rest.gclient.IUpdate;
import ca.uhn.fhir.rest.gclient.IValidate;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplate;
import com.drajer.sof.utils.FhirClient;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IBaseResource;

public class EcrFhirClient extends FhirClient {

  private IGenericClient parent;
  private FHIRRetryTemplate fhirRetryTemplate;

  public EcrFhirClient(
      IGenericClient parent, FHIRRetryTemplate fhirRetryTemplate, String requestId) {
    super(parent, requestId);
    this.parent = parent;
    this.fhirRetryTemplate = fhirRetryTemplate;
  }

  public FHIRRetryTemplate getRetryTemplate() {
    return fhirRetryTemplate;
  }

  @Override
  public IInterceptorService getInterceptorService() {
    return parent.getInterceptorService();
  }

  @Override
  public void setInterceptorService(IInterceptorService theInterceptorService) {
    parent.setInterceptorService(theInterceptorService);
  }

  @Override
  public <T extends IBaseResource> T fetchResourceFromUrl(Class<T> theResourceType, String theUrl) {
    return parent.fetchResourceFromUrl(theResourceType, theUrl);
  }

  @Override
  public EncodingEnum getEncoding() {
    return parent.getEncoding();
  }

  @Override
  public void setEncoding(EncodingEnum theEncoding) {
    parent.setEncoding(theEncoding);
  }

  @Override
  public FhirContext getFhirContext() {
    return parent.getFhirContext();
  }

  @Override
  public IHttpClient getHttpClient() {
    return parent.getHttpClient();
  }

  @Override
  public String getServerBase() {
    return parent.getServerBase();
  }

  @Override
  public void setPrettyPrint(Boolean thePrettyPrint) {
    parent.setPrettyPrint(thePrettyPrint);
  }

  @Override
  public void setSummary(SummaryEnum theSummary) {
    parent.setSummary(theSummary);
  }

  @Override
  public void setFormatParamStyle(RequestFormatParamStyleEnum theRequestFormatParamStyle) {
    parent.setFormatParamStyle(theRequestFormatParamStyle);
  }

  @Override
  public IFetchConformanceUntyped capabilities() {
    return parent.capabilities();
  }

  @Override
  public ICreate create() {
    return parent.create();
  }

  @Override
  public IDelete delete() {
    return parent.delete();
  }

  @Override
  public IFetchConformanceUntyped fetchConformance() {
    return parent.fetchConformance();
  }

  @Override
  public void forceConformanceCheck() throws FhirClientConnectionException {
    parent.forceConformanceCheck();
  }

  @Override
  public IHistory history() {
    return parent.history();
  }

  @Override
  public IGetPage loadPage() {
    return new EcrRetryablePage(parent.loadPage(), this); // TODO
  }

  @Override
  public IMeta meta() {
    return parent.meta();
  }

  @Override
  public IOperation operation() {
    return parent.operation();
  }

  @Override
  public IPatch patch() {
    return parent.patch();
  }

  @Override
  public IRead read() {
    return new EcrFhirRetryableRead(parent.read(), this);
  }

  @Override
  public <T extends IBaseResource> T read(Class<T> theType, String theId) {
    return parent.read(theType, theId);
  }

  @Override
  public <T extends IBaseResource> T read(Class<T> theType, UriDt theUrl) {
    return parent.read(theType, theUrl);
  }

  @Override
  public IBaseResource read(UriDt theUrl) {
    return parent.read(theUrl);
  }

  @Override
  public void registerInterceptor(Object theInterceptor) {
    parent.registerInterceptor(theInterceptor);
  }

  @Override
  public <T extends IBaseBundle> IUntypedQuery<T> search() {
    return new EcrRetryableQuery(parent.search(), this); // TODO
  }

  @Override
  public void setLogRequestAndResponse(boolean theLogRequestAndResponse) {
    parent.setLogRequestAndResponse(theLogRequestAndResponse);
  }

  @Override
  public ITransaction transaction() {
    return parent.transaction();
  }

  @Override
  public void unregisterInterceptor(Object theInterceptor) {
    parent.unregisterInterceptor(theInterceptor);
  }

  @Override
  public IUpdate update() {
    return parent.update();
  }

  @Override
  public MethodOutcome update(IdDt theId, IBaseResource theResource) {
    return parent.update(theId, theResource);
  }

  @Override
  public MethodOutcome update(String theId, IBaseResource theResource) {
    return parent.update(theId, theResource);
  }

  @Override
  public IValidate validate() {
    return parent.validate();
  }

  @Override
  public MethodOutcome validate(IBaseResource theResource) {
    return parent.validate(theResource);
  }

  @Override
  public <T extends IBaseResource> T vread(Class<T> theType, IdDt theId) {
    return parent.vread(theType, theId);
  }

  @Override
  public <T extends IBaseResource> T vread(Class<T> theType, String theId, String theVersionId) {
    return parent.vread(theType, theId, theVersionId);
  }
}
