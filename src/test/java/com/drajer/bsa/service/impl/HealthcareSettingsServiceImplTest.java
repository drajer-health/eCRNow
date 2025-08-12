package com.drajer.bsa.service.impl;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.HealthcareSetting;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class HealthcareSettingsServiceImplTest {
  @Mock private HealthcareSettingsDao hsDao;

  @InjectMocks private HealthcareSettingsServiceImpl healthcareSettingsService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testSaveOrUpdate() {
    HealthcareSetting hsd = new HealthcareSetting();
    hsd.setId(3424);
    hsd.setFhirVersion("v-123");
    when(hsDao.saveOrUpdate(any())).thenReturn(hsd);
    HealthcareSetting result = healthcareSettingsService.saveOrUpdate(hsd);
    assertEquals(hsd, result);
    assertEquals(hsd.getId().longValue(), result.getId().longValue());
    assertEquals("v-123", result.getFhirVersion());
    assertNotNull(hsd);
    verify(hsDao, times(1)).saveOrUpdate(hsd);
  }

  @Test
  public void testGetHealthcareSettingById() {
    HealthcareSetting hsd = new HealthcareSetting();
    hsd.setId(3424);
    when(hsDao.getHealthcareSettingById(any())).thenReturn(hsd);
    HealthcareSetting result = healthcareSettingsService.getHealthcareSettingById(any());
    assertEquals(hsd.getId().longValue(), result.getId().longValue());
    assertNotNull(hsd);
    verify(hsDao, times(1)).getHealthcareSettingById(any());
  }

  @Test
  public void testGetHealthcareSettingByUrl() {
    HealthcareSetting hsd = new HealthcareSetting();
    hsd.setTokenUrl("https://www.google.com");
    when(hsDao.getHealthcareSettingByUrl(any())).thenReturn(hsd);
    HealthcareSetting result = healthcareSettingsService.getHealthcareSettingByUrl(any());
    assertEquals(hsd.getTokenUrl(), result.getTokenUrl());
    assertNotNull(hsd);
    verify(hsDao, times(1)).getHealthcareSettingByUrl(any());
  }

  @Test
  public void testGetAllHealthcareSettings() {
    HealthcareSetting hsd = new HealthcareSetting();
    hsd.setTokenUrl("https://www.google.com");
    hsd.setId(1234);
    hsd.setFhirServerBaseURL("https://FHIRTest.com");
    List<HealthcareSetting> hsdList = Arrays.asList(hsd);
    when(hsDao.getAllHealthcareSettings()).thenReturn(hsdList);
    List<HealthcareSetting> result = healthcareSettingsService.getAllHealthcareSettings();
    assertEquals(1, result.size());
    assertEquals(hsd.getId().longValue(), result.get(0).getId().longValue());
    assertEquals(hsd.getTokenUrl(), result.get(0).getTokenUrl());
    assertEquals(hsd.getFhirServerBaseURL(), result.get(0).getFhirServerBaseURL());
    verify(hsDao, times(1)).getAllHealthcareSettings();
  }

  @Test
  public void testDelete() {
    HealthcareSetting hsd = new HealthcareSetting();
    doNothing().when(hsDao).delete(any(HealthcareSetting.class));
    healthcareSettingsService.delete(hsd);
    verify(hsDao, times(1)).delete(hsd);
  }
}
