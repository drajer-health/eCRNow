package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.ecrapp.dao.AbstractDao;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.sqlserver.jdbc.StringUtils;

import java.util.List;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>HealthcareSettingsDaoImpl</h1>
 *
 * The HealthcareSettingsDaoImpl class implements the Create, Read, Update service methods for
 * HealthcareSettings.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
@Repository
@Transactional
public class HealthcareSettingsDaoImpl extends AbstractDao implements HealthcareSettingsDao {

  @Autowired KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;

  private final Logger logger = LoggerFactory.getLogger(HealthcareSettingsDaoImpl.class);

  /**
   * Method to create or update a HealthcareSetting.
   *
   * @param hsd The HealthcareSettings details to be used for creation or updation.
   * @return Returns the HealthcareSettings created or updated.
   */
  @Override
  public HealthcareSetting saveOrUpdate(HealthcareSetting hsd) {

    if (hsd.getKars() != null) {
      ObjectMapper mapper = new ObjectMapper();

      try {
        String kars = mapper.writeValueAsString(hsd.getKars());
        hsd.setKarsActive(kars);
        logger.info(" Successfully set the KarsActive list ");
      } catch (JsonProcessingException e) {
        logger.error("Exception Occurred: ", e);
      }
    }

    getSession().saveOrUpdate(hsd);
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
    HealthcareSetting hs = getSession().get(HealthcareSetting.class, id);

    if (hs != null) setKars(hs);

    return hs;
  }

  /**
   * Method to retrieve a HealthcareSetting.
   *
   * @param url The HealthcareSettings details to be retrieved based on the url.
   * @return Returns the HealthcareSettings for the provided url.
   */
  @Override
  public HealthcareSetting getHealthcareSettingByUrl(String url) {
    Criteria criteria = getSession().createCriteria(HealthcareSetting.class);
    criteria.add(Restrictions.eq("fhirServerBaseURL", url));
    HealthcareSetting hs = (HealthcareSetting) criteria.uniqueResult();

    if (hs != null) setKars(hs);

    return hs;
  }

  /**
   * Method to retrieve all existing HealthcareSettings.
   *
   * @param none
   * @return Returns the list of existing HealthcareSettings.
   */
  @Override
  public List<HealthcareSetting> getAllHealthcareSettings() {
    Criteria criteria = getSession().createCriteria(HealthcareSetting.class);
    return criteria.addOrder(Order.desc("id")).list();
  }

  private void setKars(HealthcareSetting hs) {

    List<KnowledgeArtifactStatus> activeKars = getKarsActiveByHsId(hs.getId());

    if (activeKars != null) {

      HealthcareSettingOperationalKnowledgeArtifacts opkars =
          new HealthcareSettingOperationalKnowledgeArtifacts();
      opkars.setId(hs.getId());

      for (KnowledgeArtifactStatus stat : activeKars) {
        logger.info(" Adding Kar Id {} to active Kars", stat.getVersionUniqueKarId());
        opkars.addArtifactStatus(stat);
      }

      hs.setKars(opkars);
    }
  }

  @Override
  public List<KnowledgeArtifactStatus> getKarsActiveByHsId(Integer id) {

    Criteria criteria = getSession().createCriteria(KnowledgeArtifactStatus.class);
    criteria.add(Restrictions.eq("hsId", id));

    return criteria.list();
  }

  @Override
  public void delete(HealthcareSetting healthcareSetting) {
    getSession().delete(healthcareSetting);
  }
}
