package com.drajer.bsa.controller;

import com.drajer.bsa.model.KnowledgeArtifiactRepository;
import com.drajer.bsa.service.KarService;
import java.util.List;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class KnowledgeArtifactRepositoryController {

  private final Logger logger = LoggerFactory.getLogger(HealthcareSettingsController.class);

  @Autowired KarService karService;

  @CrossOrigin
  @RequestMapping("/api/kar/{karId}")
  public KnowledgeArtifiactRepository getHealthcareSettingById(
      @PathVariable("karId") Integer karId) {
    return karService.getKARById(karId);
  }

  @CrossOrigin
  @RequestMapping(value = "/api/kar", method = RequestMethod.POST)
  public ResponseEntity<?> createKARs(@RequestBody KnowledgeArtifiactRepository kar) {
    KnowledgeArtifiactRepository existingKar = karService.getKARByUrl(kar.getFhirServerURL());
    if (existingKar == null || (existingKar.getId().equals(existingKar.getId()))) {
      logger.info("Saving the KAR Details");
      karService.saveOrUpdate(kar);
      return new ResponseEntity<>(kar, HttpStatus.OK);
    } else {
      logger.error(
          "KAR is already exists with a different Id which is {}, contact developer. ",
          existingKar.getId());
      JSONObject responseObject = new JSONObject();
      responseObject.put("status", "error");
      responseObject.put(
          "message",
          "Healthcare Setting is already registered with a different Id, contact developer. ");
      return new ResponseEntity<>(responseObject, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  @CrossOrigin
  @RequestMapping("/api/kars")
  public KnowledgeArtifiactRepository getKARByUrl(@RequestParam(value = "url") String url) {
    return karService.getKARByUrl(url);
  }

  @CrossOrigin
  @RequestMapping("/api/kars/")
  public List<KnowledgeArtifiactRepository> getAllKARs() {
    return karService.getAllKARs();
  }
}
