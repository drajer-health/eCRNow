package com.drajer.ecrapp.config;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import org.springframework.stereotype.Component;

@Component
public class QueryReaderConfig {

  private final Properties properties = new Properties();

  public QueryReaderConfig() {
    try (InputStream input =
        getClass().getClassLoader().getResourceAsStream("dbqueries.properties")) {
      if (input == null) {
        System.out.println("Sorry, unable to find queries.properties");
        return;
      }
      properties.load(input);
    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  public String getQuery(String queryKey) {
    return properties.getProperty(queryKey);
  }
}
