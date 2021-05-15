package com.drajer.bsa.model;

public final class BsaTypes {

  public enum AuthenticationType {
    SofSystem,
    SofProvider,
    UserNamePwd
  }

  public enum BsaActionStatusType {
    NotStarted,
    InProgress,
    Completed,
    Failed,
    Aborted
  }
}
