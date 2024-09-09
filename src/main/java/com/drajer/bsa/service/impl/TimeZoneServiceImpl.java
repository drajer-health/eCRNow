package com.drajer.bsa.service.impl;

import com.drajer.bsa.dao.TimeZoneDao;
import com.drajer.bsa.service.TimeZoneService;
import com.drajer.ecrapp.config.QueryReaderConfig;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

@Service
public class TimeZoneServiceImpl implements TimeZoneService {

  private static final Logger logger = LoggerFactory.getLogger(TimeZoneServiceImpl.class);

  private final TimeZoneDao timeZoneDao;
  private final QueryReaderConfig queryReaderConfig;

  @Value("${db.timezone}")
  private String dbTimezone;

  public TimeZoneServiceImpl(TimeZoneDao timeZoneDao, QueryReaderConfig queryReaderConfig) {
    this.timeZoneDao = timeZoneDao;
    this.queryReaderConfig = queryReaderConfig;
  }

  @Override
  public String getDatabaseTimezone() {
    String query =
        Optional.ofNullable(queryReaderConfig.getQuery("query.getTimezone"))
            .orElseThrow(
                () ->
                    new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        "Query for 'query.getTimezone' not found in queries properties."));

    try {
      return timeZoneDao.getDatabaseTimezone(query);
    } catch (Exception ex) {
      logErrorAndThrow("retrieving database timezone", ex);
    }
    return null; // This will never be reached due to the exception being thrown
  }

  @Override
  public void setDatabaseTimezone(String timeZone) {
    String query =
        Optional.ofNullable(queryReaderConfig.getQuery("query.setTimezone"))
            .orElseThrow(
                () ->
                    new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        "Query for 'query.setTimezone' not found in queries properties."));

    String effectiveTimeZone = Optional.ofNullable(timeZone).orElse(dbTimezone);

    if (effectiveTimeZone.isEmpty()) {
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid time zone provided");
    }

    try {
      timeZoneDao.setDatabaseTimezone(query, effectiveTimeZone);
    } catch (Exception ex) {
      logErrorAndThrow("setting database timezone to '" + effectiveTimeZone + "'", ex);
    }
  }

  private void logErrorAndThrow(String action, Exception ex) {
    logger.error("Error while {}.", action, ex);
    throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Unable to " + action);
  }
}
