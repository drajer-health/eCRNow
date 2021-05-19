package com.drajer.bsa.utils;

import ca.uhn.fhir.parser.IParser;

import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ValueSet;
import org.hl7.fhir.r4.model.ValueSet.ConceptReferenceComponent;
import org.hl7.fhir.r4.model.ValueSet.ConceptSetComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetComposeComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>BsaServiceUtils</h1>
 *
 * This class implements a few utilities to be used by many of the different services.
 *
 * @author nbashyam
 */
@Service
@Transactional
public class BsaServiceUtils {

  private static final Logger logger = LoggerFactory.getLogger(BsaServiceUtils.class);

  @Autowired
  @Qualifier("jsonParser")
  IParser jsonParser;
  
  @Value("${bsa.output.directory}")
  String debugDirectory;

  public Bundle readKarFromFile(String filePath) {

    logger.info("About to read KAR File {}", filePath);
    Bundle bundle = null;
    try (InputStream in = new FileInputStream(new File(filePath))) {
      logger.info("Start Reading KAR File ");

      bundle = jsonParser.parseResource(Bundle.class, in);
      logger.info("Completed Reading KAR File");
    } catch (Exception e) {
      logger.error("Exception Reading KAR File", e);
    }
    return bundle;
  }

  public static Boolean isCodeableConceptPresentInValueSet(ValueSet vs, CodeableConcept cd) {

    Boolean retVal = false;

    if (cd != null && cd.getCoding().size() > 0) {

      for (Coding c : cd.getCoding()) {

        if (isCodingPresentInValueSet(vs, c)) retVal = true;
      }
    }

    return retVal;
  }

  public static Boolean isCodingPresentInValueSet(ValueSet vs, Coding coding) {

    Boolean retVal = false;

    if (coding != null && isCodePresentInValueSet(vs, coding.getSystem(), coding.getCode()))
      return true;

    return retVal;
  }

  public static Boolean isCodePresentInValueSet(ValueSet vs, String system, String code) {

    Boolean retVal = false;

    if (vs.hasCompose()) {

      ValueSetComposeComponent vsc = vs.getCompose();

      List<ConceptSetComponent> cscs = vsc.getInclude();

      if (cscs != null) {

        for (ConceptSetComponent csc : cscs) {

          if (csc.getSystem() != null && csc.getSystem().contentEquals(system)) {

            logger.info(" Found Code System {} in value set ", system);

            List<ConceptReferenceComponent> crcs = csc.getConcept();

            if (crcs != null) {

              for (ConceptReferenceComponent crc : crcs) {

                if (crc.getCode().contentEquals(code)) {
                  logger.info(" Found code system {} and code {} in value set ", system, code);
                  retVal = true;
                }
              }
            }
          }
        }
      }
    }

    return retVal;
  }
  
  public void saveResorceToFile(Resource res) {
	  
	  String fileName = debugDirectory + res.getResourceType().toString() + "_" + res.getId() + ".json";
		        
	  String data = jsonParser.encodeResourceToString(res); 
	  
	  try (DataOutputStream outStream =
		        new DataOutputStream(new BufferedOutputStream(new FileOutputStream(fileName)))) {

		      logger.info(" Writing data to file: {}", fileName);
		      outStream.writeBytes(data);
		    } catch (IOException e) {
		      logger.debug(" Unable to write data to file: {}", fileName, e);
		    }
  }
}
