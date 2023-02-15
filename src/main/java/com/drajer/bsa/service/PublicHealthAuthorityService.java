package com.drajer.bsa.service;

import com.drajer.bsa.model.PublicHealthAuthority;
import java.util.List;

public interface PublicHealthAuthorityService {

  /**
   * Method to create or update a PublicHealthAuthority.
   *
   * @param pha The PublicHealthAuthority's details to be used for creation or updation.
   * @return Returns the PublicHealthAuthority created or updated.
   */
  PublicHealthAuthority saveOrUpdate(PublicHealthAuthority pha);

  /**
   * Method to retrieve a PublicHealthAuthority.
   *
   * @param id The PublicHealthAuthority's details to be retrieved based on the id.
   * @return Returns the PublicHealthAuthority for the provided id.
   */
  PublicHealthAuthority getPublicHealthAuthorityById(Integer id);

  /**
   * Method to retrieve a PublicHealthAuthority.
   *
   * @param url The PublicHealthAuthority's details to be retrieved based on the url.
   * @return Returns the HealthcareSetting for the provided url.
   */
  PublicHealthAuthority getPublicHealthAuthorityByUrl(String url);

  /**
   * Method to retrieve all existing PublicHealthAuthority.
   *
   * @return Returns the list of existing PublicHealthAuthority.
   */
  List<PublicHealthAuthority> getAllPublicHealthAuthority();
}
