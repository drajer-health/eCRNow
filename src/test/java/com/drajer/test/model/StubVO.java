package com.drajer.test.model;

public class StubVO {

  private Object params;
  private String responseFilePath;

  public StubVO(Object params, String responseFilePath) {
    super();
    this.params = params;
    this.responseFilePath = responseFilePath;
  }

  public Object getParams() {
    return params;
  }

  public void setParams(Object params) {
    this.params = params;
  }

  public String getResponseFilePath() {
    return responseFilePath;
  }

  public void setResponseFilePath(String responseFilePath) {
    this.responseFilePath = responseFilePath;
  }
}
