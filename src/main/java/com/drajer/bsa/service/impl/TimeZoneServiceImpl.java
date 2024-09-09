package com.drajer.bsa.service.impl;

import com.drajer.bsa.dao.TimeZoneDao;
import com.drajer.bsa.service.TimeZoneService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class TimeZoneServiceImpl implements TimeZoneService {

  @Autowired private TimeZoneDao timeZoneDao;

  @Override
  public String getDatabaseTimezone() {
    return timeZoneDao.getDatabaseTimezone();
  }
}
