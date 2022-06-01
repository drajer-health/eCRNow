package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Properties;
import java.util.Set;
import org.hl7.fhir.r4.model.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This is the interface for creating a Report for a Public Health use case.
 *
 * @author nbashyam
 */
public abstract class ReportCreator {

  private final Logger logger = LoggerFactory.getLogger(ReportCreator.class);
  private static final Logger logger2 = LoggerFactory.getLogger(ReportCreator.class);

  private static HashMap<String, ReportCreator> reportingClasses = new HashMap<>();

  public static ReportCreator getReportCreator(String profileName) {

    if (reportingClasses.size() == 0) initReportingClasses();

    if (reportingClasses.containsKey(profileName)) {
      return reportingClasses.get(profileName);
    } else return null;
  }

  // Load the Topic to Named Event Map.
  public static void initReportingClasses() {

    try (InputStream input =
        ReportCreator.class.getClassLoader().getResourceAsStream("create-report.properties")) {

      Properties prop = new Properties();
      prop.load(input);

      Set<String> entries = prop.stringPropertyNames();

      for (String propName : entries) {
        ReportCreator creator;
        try {
          creator = (ReportCreator) (Class.forName(prop.getProperty(propName))).newInstance();
          reportingClasses.put(propName, creator);
        } catch (InstantiationException e) {
          logger2.error(" Instantiation Exception in creating reporting class {}", propName, e);
        } catch (IllegalAccessException e) {
          logger2.error(" IllegalAccess Exception in creating reporting class {}", propName, e);
        } catch (ClassNotFoundException e) {
          logger2.error(" ClassNotFound Exception in creating reporting class {}", propName, e);
        }
      }
    } catch (IOException ex) {
      logger2.error("Error while loading Action Classes from Properties File ");
    }
  }

  public abstract Resource createReport(
      KarProcessingData kd, EhrQueryService ehrService, String id, String profile, BsaAction act);
}
