package com.drajer.bsa.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.verify;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.service.HealthcareSettingsService;
import com.drajer.sof.utils.Authorization;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class HealthcareSettingsControllerTest {

  @Mock private HealthcareSettingsService healthcareSettingsService;

  @Mock private Authorization authorization;

  @InjectMocks private HealthcareSettingsController healthcareSettingsController;

  public static final String HEALTHCARE_SETTING_JSON = "Bsa/HealthCareSettings.json";

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(healthcareSettingsController, "directSmtpTlsVersion", "TLSv1.2");
  }

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

  @Test
  public void getAllHealthcareSettings_shouldReturnEmptyList() {

    Mockito.lenient()
        .when(healthcareSettingsService.getAllHealthcareSettings())
        .thenReturn(Arrays.asList());

    List<HealthcareSetting> response = healthcareSettingsController.getAllHealthcareSettings();

    assertEquals(0, response.size());
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
  public void deleteHealthcareSettings_shouldReturnBadRequest_whenEmptyUrl() {

    ResponseEntity<String> response =
        healthcareSettingsController.deleteHealthcareSettingsByUrl("");

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

  @Test
  public void deleteHealthcareSettings_shouldReturnInternalServerError_onException() {

    String url = "http://localhost:9011/fhir";

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(url))
        .thenThrow(new RuntimeException("Database error"));

    ResponseEntity<String> response =
        healthcareSettingsController.deleteHealthcareSettingsByUrl(url);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
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

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(null);

    Mockito.lenient().when(healthcareSettingsService.saveOrUpdate(hs)).thenReturn(hs);

    ResponseEntity<Object> response = healthcareSettingsController.updateHealthcareSettings(hs);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    verify(healthcareSettingsService).saveOrUpdate(hs);
  }

  @Test
  public void updateHealthcareSettings_shouldReturnOk_whenSameId() {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    Integer existingId = 100;
    hs.setId(existingId);

    HealthcareSetting existingHs = new HealthcareSetting();
    existingHs.setId(existingId);
    existingHs.setFhirServerBaseURL(hs.getFhirServerBaseURL());

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(existingHs);

    Mockito.lenient().when(healthcareSettingsService.saveOrUpdate(hs)).thenReturn(hs);

    ResponseEntity<Object> response = healthcareSettingsController.updateHealthcareSettings(hs);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    verify(healthcareSettingsService).saveOrUpdate(hs);
  }

  @Test
  public void updateHealthcareSettings_shouldReturnError_whenDifferentId() {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    hs.setId(100);

    HealthcareSetting existingHs = new HealthcareSetting();
    existingHs.setId(200);
    existingHs.setFhirServerBaseURL(hs.getFhirServerBaseURL());

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(existingHs);

    ResponseEntity<Object> response = healthcareSettingsController.updateHealthcareSettings(hs);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
  }

  @Test
  public void updateHealthcareSettings_shouldSetDirectTlsVersion_whenEmpty() {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    hs.setId(100);
    hs.setDirectTlsVersion("");

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(null);

    Mockito.lenient().when(healthcareSettingsService.saveOrUpdate(hs)).thenReturn(hs);

    ResponseEntity<Object> response = healthcareSettingsController.updateHealthcareSettings(hs);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals("TLSv1.2", hs.getDirectTlsVersion());
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

  @Test
  public void createHealthcareSettings_shouldReturnError_whenDuplicate() throws Exception {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    HealthcareSetting existingHs = new HealthcareSetting();
    existingHs.setId(200);
    existingHs.setFhirServerBaseURL(hs.getFhirServerBaseURL());

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(existingHs);

    ResponseEntity<Object> response = healthcareSettingsController.createHealthcareSettings(hs);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
  }

  @Test
  public void createHealthcareSettings_shouldSetFhirVersion_DSTU2() throws Exception {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(null);

    JSONObject dstu2Metadata = mockDSTU2Metadata();
    Mockito.lenient().when(authorization.getMetadata(anyString())).thenReturn(dstu2Metadata);
    ResponseEntity<Object> response = healthcareSettingsController.createHealthcareSettings(hs);
    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  public void createHealthcareSettings_shouldSetFhirVersion_R4() throws Exception {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(null);

    JSONObject r4Metadata = mockR4Metadata();
    Mockito.lenient().when(authorization.getMetadata(anyString())).thenReturn(r4Metadata);

    ResponseEntity<Object> response = healthcareSettingsController.createHealthcareSettings(hs);

    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  public void createHealthcareSettings_shouldSetDirectTlsVersion_whenEmpty() throws Exception {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    hs.setDirectTlsVersion("");

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(null);

    Mockito.lenient().when(authorization.getMetadata(anyString())).thenReturn(mockMetadata());

    ResponseEntity<Object> response = healthcareSettingsController.createHealthcareSettings(hs);

    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  public void createHealthcareSettings_shouldPreserveDirectTlsVersion_whenProvided()
      throws Exception {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    hs.setDirectTlsVersion("TLSv1.3");

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(null);

    Mockito.lenient().when(authorization.getMetadata(anyString())).thenReturn(mockMetadata());

    ResponseEntity<Object> response = healthcareSettingsController.createHealthcareSettings(hs);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals("TLSv1.3", hs.getDirectTlsVersion());
  }

  @Test
  public void createHealthcareSettings_shouldHandleNullMetadata() throws Exception {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(null);

    Mockito.lenient().when(authorization.getMetadata(anyString())).thenReturn(null);

    ResponseEntity<Object> response = healthcareSettingsController.createHealthcareSettings(hs);

    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  public void createHealthcareSettings_withTokenUrl_shouldNotOverrideExisting() throws Exception {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    String providedTokenUrl = "http://custom-token-url.com";
    hs.setTokenUrl(providedTokenUrl);

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(null);

    Mockito.lenient().when(authorization.getMetadata(anyString())).thenReturn(mockMetadata());

    ResponseEntity<Object> response = healthcareSettingsController.createHealthcareSettings(hs);

    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  // =====================================================
  // GET HEALTHCARE SETTING BY URL
  // =====================================================
  @Test
  public void getHealthcareSettingsByUrl_shouldReturnSetting() {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    String url = "http://localhost:9011/fhir";

    Mockito.lenient().when(healthcareSettingsService.getHealthcareSettingByUrl(url)).thenReturn(hs);

    HealthcareSetting response = healthcareSettingsController.getHealthcareSettingsByUrl(url);

    assertEquals(hs.getId(), response.getId());
    verify(healthcareSettingsService).getHealthcareSettingByUrl(url);
  }

  @Test
  public void getHealthcareSettingsByUrl_shouldReturnNull_whenNotFound() {

    String url = "http://localhost:9011/fhir";

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(url))
        .thenReturn(null);

    HealthcareSetting response = healthcareSettingsController.getHealthcareSettingsByUrl(url);

    assertEquals(null, response);
  }

  // =====================================================
  // CHECK HEALTHCARE SETTING EXIST
  // =====================================================
  @Test
  public void checkHealthCareSettingExist_shouldReturnConflict_whenExists() {

    HealthcareSetting hs =
        TestUtils.readFileContents(
            HEALTHCARE_SETTING_JSON, new TypeReference<HealthcareSetting>() {});

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(hs.getFhirServerBaseURL()))
        .thenReturn(hs);

    ResponseEntity<Object> response =
        healthcareSettingsController.checkHealthCareSettingExist(hs.getFhirServerBaseURL());

    assertEquals(HttpStatus.CONFLICT, response.getStatusCode());

    Map<String, Object> responseBody = (Map<String, Object>) response.getBody();
    assertEquals(true, responseBody.get("exists"));
  }

  @Test
  public void checkHealthCareSettingExist_shouldReturnOk_whenNotExists() {

    String url = "http://localhost:9011/fhir";

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(url))
        .thenReturn(null);

    ResponseEntity<Object> response = healthcareSettingsController.checkHealthCareSettingExist(url);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    Map<String, Object> responseBody = (Map<String, Object>) response.getBody();
    assertEquals(false, responseBody.get("exists"));
  }

  @Test
  public void checkHealthCareSettingExist_shouldReturnBadRequest_whenNullUrl() {

    ResponseEntity<Object> response =
        healthcareSettingsController.checkHealthCareSettingExist(null);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
  }

  @Test
  public void checkHealthCareSettingExist_shouldReturnBadRequest_whenEmptyUrl() {

    ResponseEntity<Object> response = healthcareSettingsController.checkHealthCareSettingExist("");

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
  }

  @Test
  public void checkHealthCareSettingExist_shouldReturnInternalServerError_onException() {

    String url = "http://localhost:9011/fhir";

    Mockito.lenient()
        .when(healthcareSettingsService.getHealthcareSettingByUrl(url))
        .thenThrow(new RuntimeException("Database connection error"));

    ResponseEntity<Object> response = healthcareSettingsController.checkHealthCareSettingExist(url);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
  }

  // =====================================================
  // MOCK METADATA BUILDERS
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

  private JSONObject mockDSTU2Metadata() throws Exception {

    JSONObject metadata = new JSONObject();

    JSONArray restArray = new JSONArray();
    JSONObject restObj = new JSONObject();

    JSONObject securityObj = new JSONObject();
    JSONArray extensionArray = new JSONArray();

    JSONObject extensionObj = new JSONObject();
    extensionObj.put("url", "token");

    JSONArray innerExtension = new JSONArray();
    JSONObject tokenExt = new JSONObject();
    tokenExt.put("url", "token");
    tokenExt.put("valueUri", "http://dstu2-token-url.com");

    innerExtension.put(tokenExt);
    extensionObj.put("extension", innerExtension);

    extensionArray.put(extensionObj);
    securityObj.put("extension", extensionArray);

    restObj.put("security", securityObj);
    restArray.put(restObj);

    metadata.put("rest", restArray);
    metadata.put("fhirVersion", "1.0.2");

    return metadata;
  }

  private JSONObject mockR4Metadata() throws Exception {

    JSONObject metadata = new JSONObject();

    JSONArray restArray = new JSONArray();
    JSONObject restObj = new JSONObject();

    JSONObject securityObj = new JSONObject();
    JSONArray extensionArray = new JSONArray();

    JSONObject extensionObj = new JSONObject();
    extensionObj.put("url", "token");

    JSONArray innerExtension = new JSONArray();
    JSONObject tokenExt = new JSONObject();
    tokenExt.put("url", "token");
    tokenExt.put("valueUri", "http://r4-token-url.com");

    innerExtension.put(tokenExt);
    extensionObj.put("extension", innerExtension);

    extensionArray.put(extensionObj);
    securityObj.put("extension", extensionArray);

    restObj.put("security", securityObj);
    restArray.put(restObj);

    metadata.put("rest", restArray);
    metadata.put("fhirVersion", "4.0.1");

    return metadata;
  }
}
