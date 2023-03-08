package com.drajer.bsa.controller;

import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import com.drajer.bsa.service.KarService;
import java.util.List;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 *
 *
 * <h1>KnowledgeArtifactRepositoryController</h1>
 *
 * The KnowledgeArtifactRepositoryController is used to manage the different Knowledge Artifact
 * Repositories that will be used by the Backend Service App.(BSA). A Knowledge Artifact Repository
 * (KAR) is used to publish Knowledge Artifacts for various reporting programs.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
@RestController
public class KnowledgeArtifactRepositoryController {

  private final Logger logger =
      LoggerFactory.getLogger(KnowledgeArtifactRepositoryController.class);

  /** The Service class to manage the Knowledge Artifact Repositories */
  @Autowired KarService karService;

  /**
   * Method to retrieve the Knowledge Artifact Repository by Id
   *
   * @param karId - Id of the Knowledge Artifact Repository
   * @return - The Knowledge Artifact Repository object for the id provided
   */
  @CrossOrigin
  @GetMapping("/api/kar/{karId}")
  public KnowledgeArtifactRepository getKnowledgeArtifactById(
      @PathVariable("karId") Integer karId) {
    return karService.getKARById(karId);
  }

  /**
   * Method to create a Knowledge Artifact Repository with the summary information.
   *
   * @param kar - The KnowledgeArtifactRepository object that needs to be persisted.
   * @return - Entity that is created.
   */
  @CrossOrigin
  @PostMapping(value = "/api/kar")
  public ResponseEntity<Object> createKARs(@RequestBody KnowledgeArtifactRepository kar) {
    KnowledgeArtifactRepository existingKar = karService.getKARByUrl(kar.getFhirServerURL());
    if (existingKar == null || (existingKar.getId().equals(kar.getId()))) {
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
          "Knowledge Artifact Repository is already registered with a different Id, contact developer. ");
      return new ResponseEntity<>(responseObject, HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Method to retrieve the KAR by its URL.
   *
   * @param url - The unique URL for the KAR
   * @return - The KAR object by its URL
   */
  @CrossOrigin
  @GetMapping("/api/kars")
  public KnowledgeArtifactRepository getKARByUrl(@RequestParam(value = "url") String url) {
    return karService.getKARByUrl(url);
  }

  /**
   * Method to retrieve the list of all KARs
   *
   * @return - List of KARs
   */
  @CrossOrigin
  @GetMapping("/api/kars/")
  public List<KnowledgeArtifactRepository> getAllKARs() {
    return karService.getAllKARs();
  }

  /**
   * Method to add KARStatus for a specific Healthcare setting. The KARStatus object contains how
   * the KAR is activated, de-activated for a Healthcare setting. The KARStatus also contains
   * information on whether subscriptions are enabled, disabled and the type of output that should
   * be generated for the specific KAR for the specific Healthcare setting.
   *
   * @param - The KAR status object for the healthcare setting.
   * @return - The KARStatus Object that is created.
   */
  @CrossOrigin
  @PostMapping(value = "/api/addKARStatus/")
  public ResponseEntity<List<KnowledgeArtifactStatus>> addKARStatus(
      @RequestBody List<KnowledgeArtifactStatus> karStatuses) {
    for (KnowledgeArtifactStatus karStatus : karStatuses) {
      KnowledgeArtifactStatus existingKarStatus =
          karService.getKarStatusByKarIdAndKarVersion(
              karStatus.getKarId(), karStatus.getKarVersion(), karStatus.getHsId());
      if (existingKarStatus == null) {
        karService.saveOrUpdateKARStatus(karStatus);
      } else {
        karStatus.setId(existingKarStatus.getId());

        if (Boolean.TRUE.equals(karStatus.getIsActive())) {
          karStatus.setLastActivationDate(existingKarStatus.getLastActivationDate());
        } else {
          karStatus.setLastInActivationDate(existingKarStatus.getLastInActivationDate());
        }
        karService.saveOrUpdateKARStatus(karStatus);
      }
    }
    return new ResponseEntity<>(karStatuses, HttpStatus.OK);
  }

  /**
   * Method to retrieve the KARStatus by Healthcare Setting Id.
   *
   * @param hsId - The Healthcare setting Id for which the KARStatus is being accessed.
   * @return - The List of KnowledgeArtifact Status for the HealthcareSetting.
   */
  @CrossOrigin
  @GetMapping("/api/karStatusByHsId")
  public List<KnowledgeArtifactStatus> getKARStatusByHsId(
      @RequestParam(value = "hsId") Integer hsId) {
    return karService.getKARStatusByHsId(hsId);
  }
}
