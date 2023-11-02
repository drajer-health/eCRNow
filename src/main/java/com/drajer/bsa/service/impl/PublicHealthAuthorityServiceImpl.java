package com.drajer.bsa.service.impl;

import com.drajer.bsa.dao.PublicHealthAuthorityDao;
import com.drajer.bsa.model.PublicHealthAuthority;
import com.drajer.bsa.service.PublicHealthAuthorityService;
import java.util.List;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class PublicHealthAuthorityServiceImpl implements PublicHealthAuthorityService {

  @Autowired PublicHealthAuthorityDao phaDao;
  /**
   * Method to create or update a PublicHealthAuthority.
   *
   * @param pha The PublicHealthAuthority details to be used for creation or updating.
   * @return Returns the PublicHealthAuthority created or updated.
   */
  @Override
  public PublicHealthAuthority saveOrUpdate(PublicHealthAuthority pha) {
    phaDao.saveOrUpdate(pha);
    return pha;
  }

  /**
   * Method to retrieve a PublicHealthAuthority.
   *
   * @param id The PublicHealthAuthority details to be retrieved based on the id.
   * @return Returns the PublicHealthAuthority for the provided id.
   */
  @Override
  public PublicHealthAuthority getPublicHealthAuthorityById(Integer id) {
    return phaDao.getPublicHealthAuthorityById(id);
  }

  /**
   * Method to retrieve a PublicHealthAuthority.
   *
   * @param url The PublicHealthAuthority details to be retrieved based on the url.
   * @return Returns the PublicHealthAuthority for the provided url.
   */
  @Override
  public PublicHealthAuthority getPublicHealthAuthorityByUrl(String url) {
    return phaDao.getPublicHealthAuthorityByUrl(url);
  }

  /**
   * Method to retrieve all existing HealthcareSettings.
   *
   * @return Returns the list of existing HealthcareSettings.
   */
  @Override
  public List<PublicHealthAuthority> getAllPublicHealthAuthority() {
    return phaDao.getAllPublicHealthAuthority();
  }
}
