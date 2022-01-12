package com.drajer.sof.utils;

import java.util.Random;

public class FhirHttpHeaderInterceptor extends HttpHeaderInterceptor {
  private static final int RANDOM_NUM_WIDTH = 5;
  private static final String RANDOM_NUM_FORMAT = "%0" + RANDOM_NUM_WIDTH + "d";
  private static final int RANDOM_NUM_BASE = (int) Math.round(Math.pow(10, RANDOM_NUM_WIDTH) - 1);
  private String xReqId;
  private String randomNum;
  private int pageNum = 1;
  private int retryCount;

  private void updateValue() {
    setValue(String.format("%s-%sp%dr%d", xReqId, randomNum, pageNum, retryCount));
  }

  public void setXReqId(final String xReqId) {
    this.xReqId = xReqId;
    updateValue();
  }

  public String getXReqId() {
    return this.xReqId;
  }

  public String newRandomNum() {
    this.randomNum = String.format(RANDOM_NUM_FORMAT, new Random().nextInt(RANDOM_NUM_BASE));
    updateValue();
    return this.randomNum;
  }

  public void setPageNum(final int pageNum) {
    this.pageNum = pageNum;
    updateValue();
  }

  public int incrementPageNum() {
    this.pageNum++;
    updateValue();
    return this.pageNum;
  }

  public void setRetryCount(final int retryCount) {
    this.retryCount = retryCount;
    updateValue();
  }

  public void reset() {
    this.pageNum = 1;
    this.retryCount = 0;
    newRandomNum();
  }

  public FhirHttpHeaderInterceptor(String requestId) {
    super("X-Request-ID", requestId);
    if (requestId == null) {
      requestId = java.util.UUID.randomUUID().toString();
    }
    this.xReqId = requestId;
    this.randomNum = newRandomNum();
    updateValue();
  }
}
