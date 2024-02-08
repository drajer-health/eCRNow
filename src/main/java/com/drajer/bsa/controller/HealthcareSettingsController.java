package com.drajer.bsa.controller;

import ca.uhn.fhir.context.FhirVersionEnum;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.service.HealthcareSettingsService;
import com.drajer.sof.utils.Authorization;
import com.microsoft.sqlserver.jdbc.StringUtils;

import java.util.List;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 *
 *
 * <h1>HealthcareSettingsController</h1>
 *
 * The HealthcareSettingsController is used to manage the different healthcare settings that will be
 * using the Backend Service App.(BSA). For each Healthcare Settings a set of configuration items
 * are initially provided by the administrator configuring the BSA. These configuration items
 * include FHIR Server URL, Name, Knowledge Artifacts to be processed etc.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
@RestController
public class HealthcareSettingsController {

  private static final String FHIR_VERSION = "fhirVersion";
  private static final String VALUE_URI = "valueUri";
  private static final String EXTENSION = "extension";
  public static final String ERROR_IN_PROCESSING_THE_REQUEST = "Error in Processing the Request";

  @Autowired Authorization authorization;

  @Autowired HealthcareSettingsService healthcareSettingsService;
  
  @Value("${direct.tls.version}")
  String directSmtpTlsVersion;

  private final Logger logger = LoggerFactory.getLogger(HealthcareSettingsController.class);

  /**
   * This method is used to retrieve the HealthcareSettings details by primary key of the table. The
   * user interface for the BSA is expected to use this method when it already knows the id of the
   * HealthcareSetting.
   *
   * @param hdsId The id to be used to retrieve the HealthcareSetting
   * @return The HealthcareSetting object for the id provided
   */
  @CrossOrigin
  @GetMapping("/api/healthcareSettings/{hsId}")
  public HealthcareSetting getHealthcareSettingById(@PathVariable("hsId") Integer hsId) {
    return healthcareSettingsService.getHealthcareSettingById(hsId);
  }

  /**
   * This method is used to create a HealthcareSettings object by providing the necessary details.
   * The BSA administrator is expected to use this method when provisioning each of the
   * HealthcareSettings.
   *
   * @param hsDetails The HealthcareSettings details passed as part of the Request Body.
   * @return This returns the HTTP Response Entity containing the JSON representation of the
   *     HealthcareSetting when successful, else returns appropriate error.
   */
  @CrossOrigin
  @PostMapping(value = "/api/healthcareSettings")
  public ResponseEntity<Object> createHealthcareSettings(@RequestBody HealthcareSetting hsDetails) {
    HealthcareSetting hsd =
        healthcareSettingsService.getHealthcareSettingByUrl(hsDetails.getFhirServerBaseURL());

    if (hsd == null) {

      logger.info("Healthcare Setting does not exist, Saving the Healthcare Settings");

      if (hsDetails.getTokenUrl() == null) {
        JSONObject object =
            authorization.getMetadata(hsDetails.getFhirServerBaseURL() + "/metadata");
        if (object != null) {
          logger.info("Reading Metadata information");
          JSONObject security = (JSONObject) object.getJSONArray("rest").get(0);
          JSONObject sec = security.getJSONObject("security");
          JSONObject extension = (JSONObject) sec.getJSONArray(EXTENSION).get(0);
          JSONArray innerExtension = extension.getJSONArray(EXTENSION);
          if (object.getString(FHIR_VERSION).startsWith("1.")) {
            hsDetails.setFhirVersion(FhirVersionEnum.DSTU2.toString());
          }
          if (object.getString(FHIR_VERSION).startsWith("4.")) {
            hsDetails.setFhirVersion(FhirVersionEnum.R4.toString());
          }
          
          if(StringUtils.isEmpty(hsDetails.getDirectTlsVersion())) {
        	  hsDetails.setDirectTlsVersion(directSmtpTlsVersion);
          }

          for (int i = 0; i < innerExtension.length(); i++) {
            JSONObject urlExtension = innerExtension.getJSONObject(i);
            if (urlExtension.getString("url").equals("token")) {
              logger.info("Token URL::::: {}", urlExtension.getString(VALUE_URI));
              hsDetails.setTokenUrl(urlExtension.getString(VALUE_URI));
            }
          }
        }
      }
      healthcareSettingsService.saveOrUpdate(hsDetails);

      return new ResponseEntity<>(hsDetails, HttpStatus.OK);

    } else {

      logger.error("FHIR Server URL is already registered, suggest modifying the existing record.");

      JSONObject responseObject = new JSONObject();
      responseObject.put("status", "error");
      responseObject.put(
          "message",
          "FHIR Server URL is already registered, suggest modifying the existing record. is already registered");
      return new ResponseEntity<>(responseObject, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  /**
   * This method is used to update a HealthcareSettings object by providing the necessary details.
   * The BSA administrator is expected to use this method when updating already provisioned
   * HealthcareSetting.
   *
   * @param hsDetails The updated HealthcareSettings details passed as part of the Request Body.
   * @return This returns the HTTP Response Entity containing the JSON representation of the
   *     HealthcareSetting when successful, else returns appropriate error.
   */
  @CrossOrigin
  @PutMapping(value = "/api/healthcareSettings")
  public ResponseEntity<Object> updateHealthcareSettings(@RequestBody HealthcareSetting hsDetails) {
    HealthcareSetting existingHsd =
        healthcareSettingsService.getHealthcareSettingByUrl(hsDetails.getFhirServerBaseURL());

    if (existingHsd == null || (existingHsd.getId().equals(hsDetails.getId()))) {
      logger.info("Saving the Client Details");
      
      if(StringUtils.isEmpty(hsDetails.getDirectTlsVersion())) {
    	  hsDetails.setDirectTlsVersion(directSmtpTlsVersion);
      }
      
      healthcareSettingsService.saveOrUpdate(hsDetails);
      return new ResponseEntity<>(hsDetails, HttpStatus.OK);
    } else {
      logger.error(
          "Healthcare Setting is already registered with a different Id which is {}, contact developer. ",
          existingHsd.getId());
      JSONObject responseObject = new JSONObject();
      responseObject.put("status", "error");
      responseObject.put(
          "message",
          "Healthcare Setting is already registered with a different Id, contact developer. ");
      return new ResponseEntity<>(responseObject, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  /**
   * This method is used to retrieve the HealthcareSettings details by the FHIR Server URL for the
   * setting. The user interface for the BSA is expected to use this method during configuration of
   * the HealthcareSetting.
   *
   * @param url The url to be used to retrieve the HealthcareSetting
   * @return The HealthcareSetting for the url provided
   */
  @CrossOrigin
  @GetMapping("/api/healthcareSettings")
  public HealthcareSetting getHealthcareSettingsByUrl(@RequestParam(value = "url") String url) {
    return healthcareSettingsService.getHealthcareSettingByUrl(url);
  }

  /**
   * This method is used to retrieve all existing HealthcareSettings details. The user interface for
   * the BSA is expected to use this method during configuration of the HealthcareSetting.
   *
   * @param none
   * @return The existing list of HealthcareSettings.
   */
  @CrossOrigin
  @GetMapping("/api/healthcareSettings/")
  public List<HealthcareSetting> getAllHealthcareSettings() {
    return healthcareSettingsService.getAllHealthcareSettings();
  }

  /**
   * Deletes a HealthcareSetting based on the provided FHIR Server URL.
   *
   * @param url The URL used to retrieve the HealthcareSetting.
   * @return A ResponseEntity with a status message indicating the result of the delete operation.
   */
  @CrossOrigin
  @DeleteMapping("/api/healthcareSettings")
  public ResponseEntity<String> deleteHealthcareSettingsByUrl(@RequestParam("url") String url) {
    try {
      logger.info("Received URL: {} for deleting healthcareSettings", url);

      if (url == null || url.isEmpty()) {
        return ResponseEntity.badRequest().body("Requested FHIR URL is missing or empty");
      }

      HealthcareSetting healthcareSetting =
          healthcareSettingsService.getHealthcareSettingByUrl(url);

      if (healthcareSetting != null) {
        healthcareSettingsService.delete(healthcareSetting);
        return ResponseEntity.ok("HealthcareSetting deleted successfully");
      }

      return ResponseEntity.status(HttpStatus.NOT_FOUND).body("HealthcareSetting not found");

    } catch (Exception e) {
      logger.error("Error in processing the request", e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body("Error in processing the request");
    }
  }
}
