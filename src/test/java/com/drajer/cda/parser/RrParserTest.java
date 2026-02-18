package com.drajer.cda.parser;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.powermock.api.mockito.PowerMockito.doThrow;
import static org.powermock.api.mockito.PowerMockito.spy;

import com.drajer.test.util.TestUtils;
import java.io.IOException;
import javax.xml.parsers.ParserConfigurationException;
import org.junit.Test;
import org.xml.sax.SAXException;

public class RrParserTest {
  @Test
  public void testParse() throws IOException {
    RrParser parser = new RrParser();
    String rrCdaFile = TestUtils.getFileContentAsString("R4/Misc/RrResponse.xml");
    CdaRrModel result = parser.parse(rrCdaFile);
    assertNotNull(result);

    var rrDocId = result.getRrDocId();
    assertNotNull(rrDocId);
    assertEquals("172b5e81-bd1e-4d14-8b59-28eb8c8131a4", rrDocId.getRootValue());
    assertFalse(rrDocId.getRootValue().isEmpty());

    var reportableStatus = result.getReportableStatus();
    assertNotNull(reportableStatus);
    assertEquals(result.getReportableType(), reportableStatus.getCode());
  }

  @Test
  public void testParse_WithPatientId() throws IOException {
    RrParser parser = new RrParser();
    String rrCdaFile = TestUtils.getFileContentAsString("R4/Misc/RrResponse_v1.xml");
    CdaRrModel result = parser.parse(rrCdaFile);
    assertNotNull(result);

    var patientId = result.getPatientId();
    assertNotNull(patientId);
    assertEquals("123453", patientId.getExtValue());
  }

  @Test
  public void testParse_SaxException_exactAssertion() throws Exception {

    RrParser parser = spy(new RrParser());

    doThrow(new SAXException("boom")).when(parser).initDoc(anyString());

    CdaRrModel result = parser.parse("<xml></xml>");

    assertNull(result);
  }

  @Test
  public void testParse_IOException_exactAssertion() throws Exception {

    RrParser parser = spy(new RrParser());

    doThrow(new IOException("boom")).when(parser).initDoc(anyString());

    CdaRrModel result = parser.parse("<xml></xml>");

    assertNull(result);
  }

  @Test
  public void testParse_ParserConfigurationException_exactAssertion() throws Exception {

    RrParser parser = spy(new RrParser());

    doThrow(new ParserConfigurationException("boom")).when(parser).initDoc(anyString());

    CdaRrModel result = parser.parse("<xml></xml>");

    assertNull(result);
  }

  @Test
  public void testParse_emptyInput_exactAssertion() {

    RrParser parser = new RrParser();

    CdaRrModel result = parser.parse("");

    assertNull(result);
  }

  @Test
  public void testParse_SaxExceptionPath() throws Exception {
    RrParser parser = spy(new RrParser());
    doThrow(new SAXException("boom")).when(parser).initDoc(anyString());
    CdaRrModel result = parser.parse("<xml></xml>");
    assertNull(result);
  }
}
