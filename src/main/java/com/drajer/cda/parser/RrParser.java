package com.drajer.cda.parser;

import com.drajer.ecrapp.service.impl.EicrServiceImpl;
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
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class RrParser {

  private final Logger logger = LoggerFactory.getLogger(EicrServiceImpl.class);

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
      CdaParserConstants.getInstance();

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
            model.getRrDocId().getRootValue(),
            model.getRrDocId().getExtValue());
      } catch (XPathExpressionException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }

      logger.info("Returning Parsed Model");

      return model;
    } catch (ParserConfigurationException e1) {

      System.out.println("Caught Parser config Excep");
      // TODO Auto-generated catch block
      e1.printStackTrace();
    } catch (SAXException e1) {

      System.out.println("Caught SAX Excep");
      // TODO Auto-generated catch block
      e1.printStackTrace();
    } catch (IOException e1) {

      System.out.println("Caught IO  Excep");
      // TODO Auto-generated catch block
      e1.printStackTrace();
    }

    return null;
  }
}
