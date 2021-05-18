package com.drajer.bsa.model;

public final class BsaTypes {

  public enum AuthenticationType {
    SofSystem,
    SofProvider,
    UserNamePwd,
    Unknown
  }

  public static String getString(AuthenticationType t) {

    if (t == AuthenticationType.SofSystem) return "SofSystem";
    else if (t == AuthenticationType.SofProvider) return "SofProvider";
    else if (t == AuthenticationType.UserNamePwd) return "UserNamePwd";
    else return "Unknown";
  }

  public static AuthenticationType getAuthenticationType(String s) {

    if (s.contentEquals("SofSystem")) return AuthenticationType.SofSystem;
    else if (s.contentEquals("SofProvider")) return AuthenticationType.SofProvider;
    else if (s.contentEquals("UserNamePwd")) return AuthenticationType.UserNamePwd;
    else return AuthenticationType.Unknown;
  }

  public enum BsaActionStatusType {
    NotStarted,
    Scheduled,
    InProgress,
    Completed,
    Failed,
    Aborted
  }
}
