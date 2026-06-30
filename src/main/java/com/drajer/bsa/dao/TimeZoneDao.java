package com.drajer.bsa.dao;

public interface TimeZoneDao {

  String getDatabaseTimezone(String query);

  void setDatabaseTimezone(String timeZone);
}
