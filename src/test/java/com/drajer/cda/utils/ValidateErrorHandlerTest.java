package com.drajer.cda.utils;

import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.xml.sax.SAXParseException;

@RunWith(MockitoJUnitRunner.class)
public class ValidateErrorHandlerTest {

  private ValidateErrorHandler validateErrorHandler;

  private SAXParseException parseException;

  @Before
  public void setUp() {
    validateErrorHandler = spy(new ValidateErrorHandler());
    parseException = Mockito.mock(SAXParseException.class);
    Mockito.when(parseException.getLineNumber()).thenReturn(10);
    Mockito.when(parseException.getColumnNumber()).thenReturn(5);
    Mockito.when(parseException.getMessage()).thenReturn("Error validating XML Data");
  }

  @Test
  public void testWarning() throws Exception {
    validateErrorHandler.warning(parseException);
    verify(validateErrorHandler, times(1)).warning(parseException);
  }

  @Test
  public void testFatalError() throws Exception {
    validateErrorHandler.fatalError(parseException);
    verify(validateErrorHandler, times(1)).fatalError(parseException);
  }

  @Test
  public void testError() throws Exception {
    validateErrorHandler.error(parseException);
    verify(validateErrorHandler, times(1)).error(parseException);
  }

  @Test
  public void testGetIsException() throws SAXParseException {
    Boolean isException = validateErrorHandler.getIsException();
    assertFalse(isException);
  }
}
