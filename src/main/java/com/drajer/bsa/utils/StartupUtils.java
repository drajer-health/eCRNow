package com.drajer.bsa.utils;

import java.time.Instant;
import java.util.Calendar;
import java.util.Date;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 *
 *
 * <h1>StartupUtils</h1>
 *
 * This class implements startup utilities and static attributes to be used by many of the different
 * services.
 *
 * @author nbashyam
 */
@Service
public class StartupUtils {

  private static final Logger logger = LoggerFactory.getLogger(StartupUtils.class);

  private static Date startTime;

  private static Integer startupTimeDelay;

  public static void setStartTime(Date d) {
    startTime = d;
    logger.info(" Start Time of the App {}", startTime);
  }

  public static Date getStartTime() {
    return startTime;
  }

  public static Integer getStartupTimeDelay() {
    return startupTimeDelay;
  }

  public static Boolean hasAppStarted() {

    Calendar c = Calendar.getInstance();
    c.setTime(startTime);
    ;
    c.add(Calendar.SECOND, startupTimeDelay);
    Date delayedTime = c.getTime();

    if (Date.from(Instant.now()).after(delayedTime)) {
      return true;
    } else return false;
  }

  public static Date getPatientLaunchInstanceTime() {

    Calendar c = Calendar.getInstance();
    c.add(Calendar.SECOND, startupTimeDelay);
    Date delayedTime = c.getTime();

    return delayedTime;
  }

  @Value("${startup.timedelay}")
  private void setStartupTimeDelay(Integer val) {
    startupTimeDelay = val;
  };
}
