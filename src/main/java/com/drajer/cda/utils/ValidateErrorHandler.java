package com.drajer.cda.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class ValidateErrorHandler implements ErrorHandler {

  public static final Logger logger = LoggerFactory.getLogger(ValidateErrorHandler.class);

  private static boolean isException = false;

  public void warning(SAXParseException exception) throws SAXException {
    logMessage(exception);
  }

  public void fatalError(SAXParseException exception) throws SAXException {
    isException = true;
    logMessage(exception);
  }

  public void error(SAXParseException exception) throws SAXException {
    isException = true;
    logMessage(exception);
  }

  public boolean getIsException() {
    return isException;
  }

  private static void logMessage(SAXParseException exception) {
    logger.error(
        "Message: Error validating XML Data at Line: "
            + exception.getLineNumber()
            + " Column: "
            + exception.getColumnNumber()
            + "; Message: "
            + exception.getMessage());
  }
}
