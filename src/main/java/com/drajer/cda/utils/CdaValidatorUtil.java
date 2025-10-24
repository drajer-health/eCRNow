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
      logger.trace("*** Inside getSchema Method ***");
      String xsd = new File(ActionRepo.getInstance().getXsdSchemasLocation()).getAbsolutePath();
      StreamSource xsdStreamSource = new StreamSource(xsd);
      SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
      schemaFactory.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "file");
      schemaFactory.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "file");
      schema = schemaFactory.newSchema(xsdStreamSource);
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
      logger.trace("**** Starting CDA Schema Validation from XSD ****");

      if (schema == null) {
        logger.error("No schema present to validate the EICR XML");
        return false;
      }

      Validator validator = schema.newValidator();
      // Add a custom ErrorHandler
      ValidateErrorHandler errorHandler = new ValidateErrorHandler();
      validator.setErrorHandler(errorHandler);
      validator.validate(new StreamSource(new ByteArrayInputStream(xmlData.getBytes())));

      logger.trace("**** End CDA Schema Validation from XSD ****");

      if (errorHandler.getIsException()) {
        logger.info("CDA Schema Validation Failed");
        return false;
      }

    } catch (SAXException | IOException e) {
      logger.error("Error in CDA Schema Validation", e);
      return false;
    }
    logger.info("CDA Schema Validation Succeed");
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

    return validateEicrToSchematron(ecrData, ActionRepo.getInstance().getSchematronFileLocation());
  }

  /**
   * Method used for validating XML data against valid Schematron returns true if XML data matched
   * Schematton
   *
   * @param ecrData
   * @param schematronFilePath Indicates which schematron to use
   * @return boolean value
   */
  public static boolean validateEicrToSchematron(String ecrData, String schematronFilePath) {

    boolean validationResult = false;
    final ISchematronResource aResSCH = SchematronResourceSCH.fromFile(schematronFilePath);

    if (!aResSCH.isValidSchematron()) {
      logger.warn(
          "*** Cannot Validate EICR since Schematron {} is not valid ***", schematronFilePath);
    } else {
      SchematronOutputType output = null;
      try {
        logger.debug("Found Valid Schematron {} which can be applied to EICR", schematronFilePath);
        output =
            aResSCH.applySchematronValidationToSVRL(new StreamSource(new StringReader(ecrData)));
      } catch (Exception e) {
        logger.error("Unable to read/write execution state: ", e);
      }

      if (output != null) {
        List<Object> objs = output.getActivePatternAndFiredRuleAndFailedAssert();
        boolean foundFailures = false;
        logger.info("Number of Failed Assertions {}", objs.size());

        for (Object obj : objs) {
          if (obj instanceof FailedAssert) {
            FailedAssert fa = (FailedAssert) obj;
            if (fa.getFlag() != null && (fa.getFlag().contentEquals("error"))) {
              foundFailures = true;
              logger.error(
                  "Failed Assertion: \n"
                      + "Id = {}\n"
                      + "Location = {}\n"
                      + "Text = {}\n"
                      + "Flag = {}",
                  fa.getId(),
                  fa.getLocation(),
                  fa.getText(),
                  fa.getFlag());
            } else {

              // It is a warning, so need to print to log for analysis
              logger.debug(
                  "Failed Assertion: \n"
                      + "Id = {}\n"
                      + "Location = {}\n"
                      + "Text = {}\n"
                      + "Flag = {}",
                  fa.getId(),
                  fa.getLocation(),
                  fa.getText(),
                  fa.getFlag());
            }
          }
        }

        if (foundFailures) validationResult = false;
        else validationResult = true;
      } else {
        logger.warn("Schematron Validation Output is null, so validation was not performed");
        validationResult = false;
      }
    }
    logger.info("Schematron Validation Result: {}", validationResult);
    return validationResult;
  }
}
