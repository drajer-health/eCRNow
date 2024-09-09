package com.drajer.bsa.dao;

public interface TimeZoneDao {

  String getDatabaseTimezone();

  void setDatabaseTimezone(String tz);
}
