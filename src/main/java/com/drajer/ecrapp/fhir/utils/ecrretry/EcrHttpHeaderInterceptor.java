package com.drajer.ecrapp.fhir.utils.ecrretry;

import com.drajer.sof.utils.HttpHeaderInterceptor;

public class EcrHttpHeaderInterceptor extends HttpHeaderInterceptor {

  public EcrHttpHeaderInterceptor(String header, String value) {
    super(header, value);
    // TODO Auto-generated constructor stub
  }

  private int pageNum;
  private int retryCount;

  private void updateValue() {
    setValue(String.format("%s-%sp%dr%d", pageNum, retryCount));
  }

  public void setRetryCount(final int retryCount) {
    this.retryCount = retryCount;
    updateValue();
  }
}
