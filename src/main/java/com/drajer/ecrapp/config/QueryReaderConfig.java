package com.drajer.ecrapp.config;

import com.drajer.cda.utils.CdaValidatorUtil;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class QueryReaderConfig {
  public static final Logger logger = LoggerFactory.getLogger(CdaValidatorUtil.class);

  private final Properties properties = new Properties();

  public QueryReaderConfig() {
    try (InputStream input =
        getClass().getClassLoader().getResourceAsStream("dbqueries.properties")) {
      if (input == null) {
        logger.info("Sorry, unable to find queries.properties");
        return;
      }
      properties.load(input);
    } catch (IOException ex) {
      logger.error("error while loading config" + ex.getMessage());
    }
  }

  public String getQuery(String queryKey) {
    return properties.getProperty(queryKey);
  }
}
