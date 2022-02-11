package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes.OutputContentType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.ecrapp.dao.AbstractDao;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
        // TODO Auto-generated catch block
        e.printStackTrace();
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
   * @return Returns the list of existing HealthcareSettings.
   */
  @Override
  public List<HealthcareSetting> getAllHealthcareSettings() {
    Criteria criteria = getSession().createCriteria(HealthcareSetting.class);
    return criteria.addOrder(Order.desc("id")).list();
  }

  private void setKars(HealthcareSetting hs) {

    ObjectMapper mapper = new ObjectMapper();
    HealthcareSettingOperationalKnowledgeArtifacts artifacts = null;

    try {

      // To be discussed at the connectathon, no operation defined to make this work currently.
      if (hs.getKarsActive() != null) {
        artifacts =
            mapper.readValue(
                hs.getKarsActive(), HealthcareSettingOperationalKnowledgeArtifacts.class);
        hs.setKars(artifacts);
      }

      // Setup using Knowledge Artifact Repository temporarily as a work around.
      logger.info(" TODO : Remove after finalizing approach at the Connectathon");

      HashMap<String, KnowledgeArtifact> arts =
          KnowledgeArtifactRepositorySystem.getInstance().getArtifacts();

      HealthcareSettingOperationalKnowledgeArtifacts opkars =
          new HealthcareSettingOperationalKnowledgeArtifacts();
      opkars.setId(hs.getId());
      for (HashMap.Entry<String, KnowledgeArtifact> entry : arts.entrySet()) {

        KnowledgeArtifactStatus stat = new KnowledgeArtifactStatus();
        stat.setKarId(entry.getValue().getKarId());
        stat.setKarVersion(entry.getValue().getKarVersion());
        stat.setIsActive(true);
        stat.setLastActivationDate(Date.from(Instant.now()));
        stat.setVersionUniqueKarId(entry.getValue().getVersionUniqueId());
        stat.setOutputFormat(OutputContentType.CDA_R11);

        opkars.addArtifactStatus(stat);
      }

      hs.setKars(opkars);

      logger.info(" Successfully set the KAR status ");
    } catch (JsonMappingException e) {
      logger.info(" Error reading Kars Active status from the database. {}", e);
    } catch (JsonProcessingException e) {
      logger.info(" Error reading Kars Active status from the database. {}", e);
    }
  }
}
