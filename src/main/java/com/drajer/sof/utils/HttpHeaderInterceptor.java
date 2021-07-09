package com.drajer.sof.utils;

import ca.uhn.fhir.rest.client.api.IClientInterceptor;
import ca.uhn.fhir.rest.client.api.IHttpRequest;
import ca.uhn.fhir.rest.client.api.IHttpResponse;
import java.io.IOException;

public class HttpHeaderInterceptor implements IClientInterceptor {
  private String header;
  private String value;

  public HttpHeaderInterceptor(final String header, final String value) {
    this.header = header;
    this.value = value;
  }

  public void setValue(final String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  @Override
  public void interceptRequest(final IHttpRequest req) {
    req.addHeader(header, value);
  }

  @Override
  public void interceptResponse(final IHttpResponse resp) throws IOException {
    // nothing to do
  }
}
