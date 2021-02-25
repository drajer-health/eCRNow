package com.drajer.cda.utils;

import com.drajer.eca.model.ActionRepo;
import com.helger.schematron.ISchematronResource;
import com.helger.schematron.svrl.jaxb.FailedAssert;
import com.helger.schematron.svrl.jaxb.SchematronOutputType;
import com.helger.schematron.xslt.SchematronResourceSCH;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.List;
import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;

/** Util class for validating Cda data. */
public class CdaValidatorUtil {

  private CdaValidatorUtil() {
    throw new IllegalStateException("Utility class");
  }

  public static final Logger logger = LoggerFactory.getLogger(CdaValidatorUtil.class);

  private static final Schema schema = getSchema();

  private static Schema getSchema() {
    Schema schema;
    try {
      logger.info("*** Inside getSchema Method ***");
      String xsd = new File(ActionRepo.getInstance().getXsdSchemasLocation()).getAbsolutePath();
      StreamSource xsdStreamSource = new StreamSource(xsd);
      SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
      System.setProperty("javax.xml.accessExternalSchema", "file");
      // schemaFactory.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "all");
      // schemaFactory.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "");
      schema = schemaFactory.newSchema(xsdStreamSource);
      System.clearProperty("javax.xml.accessExternalSchema");
    } catch (SAXException e) {
      schema = null;
      logger.error(
          " **** Error loading XSD file from the location : {}",
          ActionRepo.getInstance().getXsdSchemasLocation(),
          e);
    }
    return schema;
  }

  /**
   * Method validates XML data against xsdFile
   *
   * @param xmlData
   * @return boolean value
   */
  public static boolean validateEicrXMLData(String xmlData) {

    try {
      logger.info(" **** Starting XML validation from xsd **** ");

      if (schema == null) {
        logger.error("Message: Error validating XML Data ");
        return false;
      }

      Validator validator = schema.newValidator();
      // Add a custom ErrorHandler
      ValidateErrorHandler errorHandler = new ValidateErrorHandler();
      validator.setErrorHandler(errorHandler);
      validator.validate(new StreamSource(new ByteArrayInputStream(xmlData.getBytes())));

      logger.info(" **** End of XML validation from xsd **** ");

      if (errorHandler.getIsException()) return false;

    } catch (SAXException | IOException e) {
      logger.error("Message: Error validating XML Data ", e);
      return false;
    }
    return true;
  }

  /**
   * Method used for validating XML data against valid Schematron returns true if XML data matched
   * Schematton
   *
   * @param ecrData
   * @return boolean value
   */
  public static boolean validateEicrToSchematron(String ecrData) {

    boolean validationResult = false;
    final ISchematronResource aResSCH =
        SchematronResourceSCH.fromFile(ActionRepo.getInstance().getSchematronFileLocation());

    if (!aResSCH.isValidSchematron()) {
      logger.info(" *** Cannot Validate since Schematron is not valid *** ");
    } else {
      SchematronOutputType output = null;
      try {
        logger.info("Found Valid Schematron which can be applied EICR ");
        output =
            aResSCH.applySchematronValidationToSVRL(new StreamSource(new StringReader(ecrData)));
      } catch (Exception e) {
        logger.error("Unable to read/write execution state: ", e);
      }

      if (output != null) {
        List<Object> objs = output.getActivePatternAndFiredRuleAndFailedAssert();
        boolean foundFailures = false;
        logger.info(" Number of Failed Assertions {}", objs.size());

        for (Object obj : objs) {
          if (obj instanceof FailedAssert) {
            FailedAssert fa = (FailedAssert) obj;
            if (fa.getFlag() != null && (fa.getFlag().contentEquals("error"))) {
              foundFailures = true;
              logger.info(
                  " Failed Asertion : Id = "
                      + fa.getId()
                      + " , Location = "
                      + fa.getLocation()
                      + " , Text = "
                      + fa.getText()
                      + ", Flag = "
                      + fa.getFlag());
            } else {

              // It is a warning, so need to print to log for analysis
              // logger.info("Failed Asertion : Id = " + fa.getId() + ", Flag = " + fa.getFlag());
            }
          }
        }

        if (foundFailures) validationResult = false;
        else validationResult = true;
      } else {
        logger.info("Schematron Validation Ouput is null, so validation was not performed ");
        validationResult = false;
      }
    }
    return validationResult;
  }
}
