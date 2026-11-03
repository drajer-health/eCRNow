package com.drajer.bsa.service;

import com.drajer.bsa.model.HealthcareSetting;
import java.util.List;

/**
 *
 *
 * <h1>HealthcareSettingsService Interface</h1>
 *
 * The HealthcareSettingsService Interface class defines the typical Create, Read, Update service
 * methods for HealthcareSettings.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
public interface HealthcareSettingsService {

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
   * Method to retrieve a HealthcareSetting.
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

  /**
   * Deletes the specified HealthcareSetting from the system.
   *
   * @param healthcareSetting The HealthcareSetting to be deleted.
   */
  void delete(HealthcareSetting healthcareSetting);

  /**
   * Marks the HealthcareSetting read-only in the current Hibernate session, so that in-memory
   * changes (such as a refreshed EHR access token) are never written as part of the current
   * transaction.
   */
  void markReadOnly(HealthcareSetting hs);

  /**
   * Saves the EHR access token of the HealthcareSetting in its own new transaction, independent of
   * the caller's transaction. Failures are thrown to the caller, which decides how to handle them.
   */
  void saveEhrAccessToken(HealthcareSetting hs);
}
