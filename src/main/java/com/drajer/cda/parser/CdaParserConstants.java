package com.drajer.cda.parser;

import java.util.Iterator;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaParserConstants {

  private static final Logger logger = LoggerFactory.getLogger(CdaParserConstants.class);
  private static final CdaParserConstants constants = new CdaParserConstants();

  private XPath CdaXPath = XPathFactory.newInstance().newXPath();

  public static XPathExpression DOC_ID_EXP;
  public static XPathExpression EICR_DOC_ID_EXP;
  public static XPathExpression REL_ID_EXP;
  public static XPathExpression RR_STATUS_OBS_EXP;
  public static XPathExpression REL_VAL_EXP;

  public static final String DEFAULT_XPATH = "/ClinicalDocument";

  // RR data
  public static final String RR_DOC_CODE = "88085-6";
  public static final String RR_DOC_CODE_SYSTEM = "http://loinc.org";
  public static final String RR_DOC_DISPLAY_NAME =
      "Reportability response report Document Public health";
  public static final String RR_DOC_CONTENT_TYPE = "application/xml;charset=utf-8";

  private CdaParserConstants() {
    initialize();
  }

  public CdaParserConstants getInstance() {
    return constants;
  }

  private void initialize() {

    try {

      DOC_ID_EXP = CdaXPath.compile("/ClinicalDocument/id[not(@nullFlavor)]");
      EICR_DOC_ID_EXP =
          CdaXPath.compile(
              "//externalDocument[not(@nullFlavor) and ./templateId[@root='2.16.840.1.113883.10.20.15.2.3.10']]");
      REL_ID_EXP = CdaXPath.compile("./id[not(@nullFlavor)]");
      RR_STATUS_OBS_EXP =
          CdaXPath.compile(
              "//observation[not(@nullFlavor) and ./templateId[@root='2.16.840.1.113883.10.20.15.2.3.19']]");
      REL_VAL_EXP = CdaXPath.compile("./value[not(@nullFlavor)]");

    } catch (XPathExpressionException e) {
      logger.error("Failed to resolve CDA xPath", e);
    }
  }

  NamespaceContext ctx =
      new NamespaceContext() {
        public String getNamespaceURI(String prefix) {
          if (prefix.contentEquals("hl7")) {
            return "urn:hl7-org:v3";
          } else if (prefix.contentEquals("hl7:sdtc")) {
            return "urn:hl7-org:v3:sdtc";
          } else return null;
        }

        public Iterator getPrefixes(String val) {
          return null;
        }

        public String getPrefix(String uri) {
          return null;
        }
      };
}
