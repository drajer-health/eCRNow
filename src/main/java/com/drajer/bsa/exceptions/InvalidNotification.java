package com.drajer.bsa.exceptions;

public class InvalidNotification extends Exception {

  public InvalidNotification(String message) {
    super(message);
  }

  public InvalidNotification(String message, Throwable throwable) {
    super(message, throwable);
  }
}
