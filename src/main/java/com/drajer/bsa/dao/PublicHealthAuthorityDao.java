package com.drajer.bsa.dao;

import com.drajer.bsa.model.PublicHealthAuthority;
import java.util.List;

public interface PublicHealthAuthorityDao {

  /**
   * Method to create or update a PublicHealthAuthority.
   *
   * @param pha The PublicHealthAuthority details to be used for creation or updating.
   * @return Returns the PublicHealthAuthority created or updated.
   */
  PublicHealthAuthority saveOrUpdate(PublicHealthAuthority pha);

  /**
   * Method to retrieve a PublicHealthAuthority.
   *
   * @param id The PublicHealthAuthority details to be retrieved based on the id.
   * @return Returns the PublicHealthAuthority for the provided id.
   */
  PublicHealthAuthority getPublicHealthAuthorityById(Integer id);

  /**
   * Method to retrieve a HealthcareSetting by Url.
   *
   * @param url The PublicHealthAuthority details to be retrieved based on the url.
   * @return Returns the PublicHealthAuthority for the provided url.
   */
  PublicHealthAuthority getPublicHealthAuthorityByUrl(String url);

  /**
   * Method to retrieve all existing HealthcareSettings.
   *
   * @return Returns the list of existing HealthcareSettings.
   */
  List<PublicHealthAuthority> getAllPublicHealthAuthority();
}
