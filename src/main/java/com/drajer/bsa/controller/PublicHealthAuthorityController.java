package com.drajer.bsa.controller;

import com.drajer.bsa.model.PublicHealthAuthority;
import com.drajer.bsa.service.PublicHealthAuthorityService;
import java.util.List;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
public class PublicHealthAuthorityController {
  private final Logger logger = LoggerFactory.getLogger(PublicHealthAuthorityController.class);

  @Autowired PublicHealthAuthorityService publicHealthAuthorityService;

  @CrossOrigin
  @GetMapping("/api/publicHealthAuthority/{phaId}")
  public PublicHealthAuthority getPublicHealthAuthorityById(@PathVariable("phaId") Integer phaId) {
    return publicHealthAuthorityService.getPublicHealthAuthorityById(phaId);
  }

  @CrossOrigin
  @PostMapping(value = "/api/publicHealthAuthority")
  public ResponseEntity<Object> createPublicHealthAuthority(
      @RequestBody PublicHealthAuthority pha) {
    PublicHealthAuthority phaCurrent =
        publicHealthAuthorityService.getPublicHealthAuthorityByUrl(pha.getFhirServerBaseURL());

    if (phaCurrent == null) {

      logger.info("Public Health Authority does not exist, Saving the Public Health Authority");

      publicHealthAuthorityService.saveOrUpdate(pha);

      return new ResponseEntity<>(pha, HttpStatus.OK);

    } else {

      logger.error(
          "PHA FHIR Server URL is already registered, suggest modifying the existing record.");

      JSONObject responseObject = new JSONObject();
      responseObject.put("status", "error");
      responseObject.put(
          "message",
          "PHA FHIR Server URL is already registered, suggest modifying the existing record. is already registered");
      return new ResponseEntity<>(responseObject, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  @CrossOrigin
  @PutMapping(value = "/api/publicHealthAuthority")
  public ResponseEntity<Object> updatePublicHealthAuthority(
      @RequestBody PublicHealthAuthority pha) {
    PublicHealthAuthority existingPha =
        publicHealthAuthorityService.getPublicHealthAuthorityByUrl(pha.getFhirServerBaseURL());

    if (existingPha == null || (existingPha.getId().equals(pha.getId()))) {
      logger.info("Saving the PHA details");
      publicHealthAuthorityService.saveOrUpdate(pha);
      return new ResponseEntity<>(pha, HttpStatus.OK);
    } else {
      logger.error(
          "Public Health Authority is already registered with a different Id which is {}, contact developer. ",
          existingPha.getId());
      JSONObject responseObject = new JSONObject();
      responseObject.put("status", "error");
      responseObject.put(
          "message",
          "Public Health Authority is already registered with a different Id, contact developer. ");
      return new ResponseEntity<>(responseObject, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  @CrossOrigin
  @GetMapping("/api/publicHealthAuthority")
  public PublicHealthAuthority getPublicHealthAuthorityByUrl(
      @RequestParam(value = "url") String url) {
    return publicHealthAuthorityService.getPublicHealthAuthorityByUrl(url);
  }

  @CrossOrigin
  @GetMapping("/api/publicHealthAuthority/")
  public List<PublicHealthAuthority> getAllPublicHealthAuthority() {
    return publicHealthAuthorityService.getAllPublicHealthAuthority();
  }
}
