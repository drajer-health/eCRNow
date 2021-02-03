package com.drajer.cda.parser;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.input.BOMInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class RrParser {

  private final Logger logger = LoggerFactory.getLogger(RrParser.class);

  private DocumentBuilderFactory factory;
  private DocumentBuilder builder;
  private Document doc;

  public void initDoc(String cdaFile)
      throws ParserConfigurationException, SAXException, IOException {
    logger.debug("Initializing Document ");
    factory = DocumentBuilderFactory.newInstance();
    builder = factory.newDocumentBuilder();
    doc =
        builder.parse(
            new BOMInputStream(IOUtils.toInputStream(cdaFile, StandardCharsets.UTF_8.name())));
  }

  public CdaRrModel parse(String cdaFile) {
    try {

      initDoc(cdaFile);
      // CdaParserConstants.getInstance();

      logger.debug("Creating Model");
      CdaRrModel model = new CdaRrModel();

      // Parse the id element.
      try {

        logger.info(" Setting the clinical document ids ");
        model.setRrDocId(
            CdaParserUtilities.readTemplateIdList(
                (NodeList) CdaParserConstants.DOC_ID_EXP.evaluate(doc, XPathConstants.NODESET)));

        logger.info(
            " RrDocId root = {} , extension = {} ",
            ((model.getRrDocId() != null) ? model.getRrDocId().getRootValue() : null),
            ((model.getRrDocId() != null) ? model.getRrDocId().getExtValue() : null));

        // Extract the Eicr Doc Id
        Element nd =
            (Element) CdaParserConstants.EICR_DOC_ID_EXP.evaluate(doc, XPathConstants.NODE);

        if (nd != null) {
          logger.info(" Eicr Document Reference Found ");

          model.setEicrDocId(
              CdaParserUtilities.readTemplateIdList(
                  (NodeList) CdaParserConstants.REL_ID_EXP.evaluate(nd, XPathConstants.NODESET)));

          logger.info(
              " EicrDocId root = {} , extension = {} ",
              ((model.getEicrDocId() != null) ? model.getEicrDocId().getRootValue() : null),
              ((model.getEicrDocId() != null) ? model.getEicrDocId().getExtValue() : null));

          // Determine status
          Element rrstatusElem =
              (Element) CdaParserConstants.RR_STATUS_OBS_EXP.evaluate(doc, XPathConstants.NODE);

          if (rrstatusElem != null) {

            logger.debug(" Found the Reportability Status Node ");
            Element resultValue =
                (Element)
                    CdaParserConstants.REL_VAL_EXP.evaluate(rrstatusElem, XPathConstants.NODE);

            if (resultValue != null) {

              logger.debug(" Found the Reportability Status Value Node ");
              CdaCode val = CdaParserUtilities.readCode(resultValue);

              if (val != null) {

                logger.debug(" Setting the Reportability Status ");
                model.setReportableType(val.getCode());
                model.setReportableStatus(val);
              }
            }
          }
        }

      } catch (XPathExpressionException e) {
        logger.error("Failed to resolve xPath", e);
      }

      logger.info("Returning Parsed Model");

      return model;
    } catch (ParserConfigurationException e1) {

      logger.error("Caught Parser config Exception", e1);

    } catch (SAXException e1) {

      logger.error("Caught SAX Exception", e1);

    } catch (IOException e1) {

      logger.error("Caught IO  Exception", e1);
    }

    return null;
  }
}
