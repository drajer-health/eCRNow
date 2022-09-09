package com.drajer.ecrapp.fhir.utils.ecrretry;

import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.IGetPage;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.IUntypedQuery;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplate;
import com.drajer.sof.utils.FhirClient;
import org.hl7.fhir.instance.model.api.IBaseBundle;

public class EcrFhirRetryClient extends FhirClient {

  private IGenericClient parent;
  private FHIRRetryTemplate fhirRetryTemplate;

  public EcrFhirRetryClient(
      IGenericClient parent, FHIRRetryTemplate fhirRetryTemplate, String requestId) {
    super(parent, requestId);
    this.parent = parent;
    this.fhirRetryTemplate = fhirRetryTemplate;
  }

  public FHIRRetryTemplate getRetryTemplate() {
    return fhirRetryTemplate;
  }

  @Override
  public IRead read() {
    this.interceptor.reset();
    return new EcrFhirRetryableRead(client.read(), this);
  }

  @Override
  public IGetPage loadPage() {
    this.interceptor.incrementPageNum();
    return new EcrFhirRetryablePage(parent.loadPage(), this);
  }

  @Override
  public <T extends IBaseBundle> IUntypedQuery<T> search() {
    this.interceptor.reset();
    return new EcrFhirRetryableSearch(client.search(), this);
  }
}
