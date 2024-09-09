package com.drajer.bsa.service;

public interface TimeZoneService {
  String getDatabaseTimezone();

  void setDatabaseTimezone(String timeZone);
}
