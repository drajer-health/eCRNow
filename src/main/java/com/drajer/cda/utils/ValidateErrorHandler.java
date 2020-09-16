package com.drajer.cda.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class ValidateErrorHandler implements ErrorHandler {

  public static final Logger logger = LoggerFactory.getLogger(ValidateErrorHandler.class);

  public static boolean isException = false;

  public void warning(SAXParseException exception) throws SAXException {
    logger.error(
        "Message: Error validating XML Data at Line: "
            + exception.getLineNumber()
            + " Column: "
            + exception.getColumnNumber()
            + "; Message: "
            + exception.getMessage());
  }

  public void fatalError(SAXParseException exception) throws SAXException {
    isException = true;
    logger.error(
        "Message: Error validating XML Data at Line: "
            + exception.getLineNumber()
            + " Column: "
            + exception.getColumnNumber()
            + "; Message: "
            + exception.getMessage());
  }

  public void error(SAXParseException exception) throws SAXException {
    isException = true;
    logger.error(
        "Message: Error validating XML Data at Line: "
            + exception.getLineNumber()
            + " Column: "
            + exception.getColumnNumber()
            + "; Message: "
            + exception.getMessage());
  }
}
