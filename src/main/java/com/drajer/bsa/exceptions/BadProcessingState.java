package com.drajer.bsa.exceptions;

public class BadProcessingState extends Exception {

  public BadProcessingState(String message) {
    super(message);
  }

  public BadProcessingState(String message, Throwable throwable) {
    super(message, throwable);
  }
}
