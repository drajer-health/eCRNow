package com.drajer.cda.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Properties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FHIRURLResourceLoader {

  private static final Logger logger = LoggerFactory.getLogger(FHIRURLResourceLoader.class);

  private FHIRURLResourceLoader() {
    // not called
  }

  // OID to URI Mapping
  private static HashMap<String, String> fhirURLMap = new HashMap<>();

  // Static block to load OID to URI mapping from property file
  static {
    try (InputStream input =
        FHIRURLResourceLoader.class
            .getClassLoader()
            .getResourceAsStream("patientresource.properties")) {

      Properties prop = new Properties();
      prop.load(input);
      prop.forEach(
          (key, value) -> {
            fhirURLMap.put((String) key, (String) value);
            logger.info("Key[" + (String) key + "] Value[" + (String) value + "]");
          });
    } catch (IOException ex) {
      logger.error("Error while loading FHIR URL resource properties files", ex);
    }
  }

  /**
   * @param oid
   * @return URI|Name
   */
  public static String getfhirURL(String theKey) {
    if (fhirURLMap.containsKey(theKey)) {
      logger.info("theKey[" + theKey + "] Value[" + fhirURLMap.get(theKey) + "]");
      return fhirURLMap.get(theKey);
    } else {
      return "";
    }
  }
}
