package com.drajer.bsa.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.service.HealthcareSettingsService;
import com.drajer.sof.utils.Authorization;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Arrays;
import java.util.List;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@RunWith(MockitoJUnitRunner.class)
public class HealthcareSettingsControllerTest {

  @Mock private HealthcareSettingsService healthcareSettingsService;

  @Mock private Authorization authorization;

  @InjectMocks private HealthcareSettingsController healthcareSettingsController;

  public static final String HEALTHCARE_SETTING_JSON = "Bsa/HealthCareSettings.json";

  @Test
  public void getHealthcareSettingById_shouldReturnSetting() {

    HealthcareSetting setting =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    setting.setId(123);

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingById(123))
        .thenReturn(setting);

    HealthcareSetting response = healthcareSettingsController.getHealthcareSettingById(123);

    assertEquals((Integer) 123, response.getId());
    verify(healthcareSettingsService).getHealthcareSettingById(123);
  }

  // =====================================================
  // GET ALL SETTINGS
  // =====================================================
  @Test
  public void getAllHealthcareSettings_shouldReturnList() {

    HealthcareSetting s1 =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    HealthcareSetting s2 =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    s2.setFhirVersion("4.0.2");

    Mockito.lenient()
        .when(healthcareSettingsService.getAllHealthcareSettings())
        .thenReturn(Arrays.asList(s1, s2));

    List<HealthcareSetting> response = healthcareSettingsController.getAllHealthcareSettings();

    assertEquals(2, response.size());
    verify(healthcareSettingsService).getAllHealthcareSettings();
  }

  // =====================================================
  // DELETE SETTINGS
  // =====================================================
  @Test
  public void deleteHealthcareSettings_shouldReturnBadRequest_whenNullUrl() {

    ResponseEntity<String> response =
        healthcareSettingsController.deleteHealthcareSettingsByUrl(null);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
  }

  @Test
  public void deleteHealthcareSettings_shouldReturnNotFound_whenMissing() {

    String url = "http://localhost:9011/fhir";

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(url))
        .thenReturn(null);

    ResponseEntity<String> response =
        healthcareSettingsController.deleteHealthcareSettingsByUrl(url);

    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
  }

  @Test
  public void deleteHealthcareSettings_shouldDelete_whenExists() {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(hs);

    ResponseEntity<String> response =
        healthcareSettingsController.deleteHealthcareSettingsByUrl(hs.getFhirServerBaseURL());

    assertEquals(HttpStatus.OK, response.getStatusCode());

    verify(healthcareSettingsService).delete(hs);
  }

  // =====================================================
  // UPDATE SETTINGS
  // =====================================================
  @Test
  public void updateHealthcareSettings_shouldReturnOk() {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    hs.setId(100);

    Mockito.lenient().when(healthcareSettingsService.saveOrUpdate(hs)).thenReturn(hs);

    ResponseEntity<Object> response = healthcareSettingsController.updateHealthcareSettings(hs);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    verify(healthcareSettingsService).saveOrUpdate(hs);
  }

  // =====================================================
  // CREATE SETTINGS
  // =====================================================
  @Test
  public void createHealthcareSettings_shouldCreate_usingJson() throws Exception {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    Mockito.lenient().when(authorization.getMetadata(anyString())).thenReturn(mockMetadata());

    ResponseEntity<Object> response = healthcareSettingsController.createHealthcareSettings(hs);

    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  // =====================================================
  // MOCK METADATA BUILDER
  // =====================================================
  private JSONObject mockMetadata() throws Exception {

    JSONObject metadata = new JSONObject();

    JSONArray restArray = new JSONArray();
    JSONObject restObj = new JSONObject();

    JSONObject securityObj = new JSONObject();
    JSONArray extensionArray = new JSONArray();

    JSONObject extensionObj = new JSONObject();
    extensionObj.put("url", "token");
    extensionObj.put("valueUri", "http://example-token-url.com");

    JSONArray innerExtension = new JSONArray();
    JSONObject tokenExt = new JSONObject();
    tokenExt.put("url", "token");
    tokenExt.put("valueUri", "http://example-token-url.com");

    innerExtension.put(tokenExt);
    extensionObj.put("extension", innerExtension);

    extensionArray.put(extensionObj);
    securityObj.put("extension", extensionArray);

    restObj.put("security", securityObj);
    restArray.put(restObj);

    metadata.put("rest", restArray);
    metadata.put("fhirVersion", "4.0");

    return metadata;
  }
}
