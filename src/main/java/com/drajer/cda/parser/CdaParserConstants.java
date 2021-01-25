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
  // private static final CdaParserConstants constants = new CdaParserConstants();

  private static XPath CCDAXPATH;
  private static XPathExpression DOC_ID_EXP;

  public static final String DEFAULT_XPATH = "/ClinicalDocument";

  // RR data
  public static final String RR_DOC_CODE = "88085-6";
  public static final String RR_DOC_CODE_SYSTEM = "http://loinc.org";
  public static final String RR_DOC_DISPLAY_NAME =
      "Reportability response report Document Public health";
  public static final String RR_DOC_CONTENT_TYPE = "application/xml;charset=utf-8";

  public static XPath getXPath() {
    CCDAXPATH = XPathFactory.newInstance().newXPath();
    return CCDAXPATH;
  }

  public static XPathExpression getDocIdExp() {
    try {
      if (DOC_ID_EXP == null)
        DOC_ID_EXP = CCDAXPATH.compile("/ClinicalDocument/id[not(@nullFlavor)]");

    } catch (XPathExpressionException e) {
      logger.error("Failed to resolve CDA xPath", e);
    }
    return DOC_ID_EXP;
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
