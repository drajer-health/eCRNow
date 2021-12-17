package com.drajer.test.util;

import static org.junit.Assert.*;

import com.drajer.cda.utils.CdaValidatorUtil;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public abstract class EicrValidation {

  private static final Logger logger = LoggerFactory.getLogger(EicrValidation.class);

  public static void validateEicrCDA(
      String eICRXml, Map<String, String> testData, List<Map<String, String>> fieldsToValidate)
      throws ParserConfigurationException, SAXException, IOException {
    validateEicrDocument(eICRXml, testData, fieldsToValidate);
    assertTrue(
        "Schema Validation Failed, check the logs", CdaValidatorUtil.validateEicrXMLData(eICRXml));
    assertTrue(
        "Schematron Validation Failed, check the logs",
        CdaValidatorUtil.validateEicrToSchematron(eICRXml));
  }

  public static void validateEicrDocument(
      String eICRXml, Map<String, String> testData, List<Map<String, String>> fieldsToValidate)
      throws ParserConfigurationException, SAXException, IOException {
    assertNotNull(eICRXml);
    assertFalse(eICRXml.isEmpty());
    Document eicrXmlDoc = TestUtils.getXmlDocument(eICRXml);
    validateXml(eicrXmlDoc, testData, fieldsToValidate);
  }

  private static void validateXml(
      Document eicrXml, Map<String, String> testData, List<Map<String, String>> fieldsToValidate) {

    final XPath xPath = XPathFactory.newInstance().newXPath();
    final String baseXPath = testData.get("BaseXPath");

    if (fieldsToValidate != null) {
      for (Map<String, String> field : fieldsToValidate) {
        try {
          String xPathExp = baseXPath + field.get("xPath");
          if (field.containsKey("count")) {
            try {
              NodeList nodeList =
                  (NodeList) xPath.compile(xPathExp).evaluate(eicrXml, XPathConstants.NODESET);
              assertEquals(xPathExp, Integer.parseInt(field.get("count")), nodeList.getLength());
            } catch (XPathExpressionException e) {
              logger.error("Exception validating field:", e);
              fail(e.getMessage() + ": Failed to evaluate field " + xPathExp);
            }
          } else {
            for (Entry<String, String> set : field.entrySet()) {
              if (!set.getKey().equalsIgnoreCase("xPath")) {
                String xPathFullExp = xPathExp + set.getKey();
                try {
                  String fieldValue =
                      (String) xPath.compile(xPathFullExp).evaluate(eicrXml, XPathConstants.STRING);
                  assertEquals(xPathFullExp, set.getValue(), fieldValue);
                } catch (XPathExpressionException e) {
                  logger.error("Exception validating field:", e);
                  fail(e.getMessage() + ": Failed to evaluate field " + xPathExp);
                }
              }
            }
          }

        } catch (Exception e) {
          logger.error("Exception validating field:", e);
          fail(e.getMessage() + ": This exception is not expected fix the test");
        }
      }

    } else {
      fail("validate field is not configured in the test");
    }
  }
}
