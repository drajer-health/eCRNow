package com.drajer.bsa.controller;

import com.drajer.bsa.service.TimeZoneService;
import java.time.ZoneId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class TimeZoneController {

  @Autowired private TimeZoneService timeZoneService;

  @GetMapping("/api/timezone")
  public String getDatabaseTimezone() {
    return timeZoneService.getDatabaseTimezone();
  }

  @PostMapping("/api/timezone")
  public ResponseEntity<String> setTimeZone(@RequestParam String timeZone) {

    try {
      // Validate the time zone
      ZoneId.of(timeZone);

      timeZoneService.setDatabaseTimezone(timeZone);

      return ResponseEntity.ok("Time zone updated successfully to " + timeZone);

    } catch (java.time.DateTimeException e) {
      return ResponseEntity.badRequest().body("Invalid time zone: " + timeZone);

    } catch (Exception e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body("Error updating time zone: " + e.getMessage());
    }
  }
}
