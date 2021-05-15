package com.drajer.bsa.dao;

import com.drajer.bsa.model.HealthcareSetting;
import java.util.List;

/**
 *
 *
 * <h1>HealthcareSettingsDao Interface</h1>
 *
 * The HealthcareSettingsDao Interface class defines the typical Create, Read, Update service
 * methods for HealthcareSettings.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
public interface HealthcareSettingsDao {

  /**
   * Method to create or update a HealthcareSetting.
   *
   * @param hsd The HealthcareSettings details to be used for creation or updation.
   * @return Returns the HealthcareSettings created or updated.
   */
  public HealthcareSetting saveOrUpdate(HealthcareSetting hsd);

  /**
   * Method to retrieve a HealthcareSetting.
   *
   * @param id The HealthcareSettings details to be retrieved based on the id.
   * @return Returns the HealthcareSettings for the provided id.
   */
  public HealthcareSetting getHealthcareSettingById(Integer id);

  /**
   * Method to retrieve a HealthcareSetting by Url.
   *
   * @param url The HealthcareSettings details to be retrieved based on the url.
   * @return Returns the HealthcareSettings for the provided url.
   */
  public HealthcareSetting getHealthcareSettingByUrl(String url);

  /**
   * Method to retrieve all existing HealthcareSettings.
   *
   * @param none
   * @return Returns the list of existing HealthcareSettings.
   */
  public List<HealthcareSetting> getAllHealthcareSettings();
}
