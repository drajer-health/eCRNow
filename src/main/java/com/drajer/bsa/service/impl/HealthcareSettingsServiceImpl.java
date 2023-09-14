package com.drajer.bsa.service.impl;

import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.service.HealthcareSettingsService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>HealthcareSettingsServiceImpl</h1>
 *
 * The HealthcareSettingsServiceImpl class implements the Create, Read, Update service methods for
 * HealthcareSettings.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
@Service
@Transactional
public class HealthcareSettingsServiceImpl implements HealthcareSettingsService {

  @Autowired HealthcareSettingsDao hsDao;

  /**
   * Method to create or update a HealthcareSetting.
   *
   * @param hsd The HealthcareSettings details to be used for creation or updation.
   * @return Returns the HealthcareSettings created or updated.
   */
  @Override
  public HealthcareSetting saveOrUpdate(HealthcareSetting hsd) {
    hsDao.saveOrUpdate(hsd);
    return hsd;
  }

  /**
   * Method to retrieve a HealthcareSetting.
   *
   * @param id The HealthcareSettings details to be retrieved based on the id.
   * @return Returns the HealthcareSettings for the provided id.
   */
  @Override
  public HealthcareSetting getHealthcareSettingById(Integer id) {
    return hsDao.getHealthcareSettingById(id);
  }

  /**
   * Method to retrieve a HealthcareSetting.
   *
   * @param url The HealthcareSettings details to be retrieved based on the url.
   * @return Returns the HealthcareSettings for the provided url.
   */
  @Override
  public HealthcareSetting getHealthcareSettingByUrl(String url) {
    return hsDao.getHealthcareSettingByUrl(url);
  }

  /**
   * Method to retrieve all existing HealthcareSettings.
   *
   * @param none
   * @return Returns the list of existing HealthcareSettings.
   */
  @Override
  public List<HealthcareSetting> getAllHealthcareSettings() {
    return hsDao.getAllHealthcareSettings();
  }

  /**
   * Deletes the specified HealthcareSetting from the system.
   *
   * @param healthcareSetting The HealthcareSetting to be deleted.
   */
  public void delete(HealthcareSetting healthcareSetting) {
    hsDao.delete(healthcareSetting);
  }
}
