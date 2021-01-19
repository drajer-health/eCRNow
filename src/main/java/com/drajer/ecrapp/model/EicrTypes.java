package com.drajer.ecrapp.model;

public final class EicrTypes {

  public enum RrType {

    // These are the responses that need to be handled.
    FAILURE_MDN,
    REPORTABLE,
    MAYBE_REPORTABLE,
    NO_RULE_MET,
    NOT_REPORTABLE
  }
}
