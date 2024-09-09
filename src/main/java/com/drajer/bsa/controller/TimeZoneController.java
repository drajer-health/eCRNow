package com.drajer.bsa.controller;

import com.drajer.bsa.service.TimeZoneService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class TimeZoneController {

  @Autowired private TimeZoneService timeZoneService;

  @GetMapping("/api/timezone")
  public String getDatabaseTimezone() {
    return timeZoneService.getDatabaseTimezone();
  }
}
