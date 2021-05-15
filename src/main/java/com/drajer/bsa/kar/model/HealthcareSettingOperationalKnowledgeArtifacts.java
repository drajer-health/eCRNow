package com.drajer.bsa.kar.model;

import java.util.HashSet;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This entity is used to marshall/unmarshall the Knowledge Artifact status for each
 * HealthcareSetting. These are not permanent tables because they can be changed dynamically and
 * have to be applied based on the named events and context.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
public class HealthcareSettingOperationalKnowledgeArtifacts {

  private final Logger logger =
      LoggerFactory.getLogger(HealthcareSettingOperationalKnowledgeArtifacts.class);

  /** The Id of the HealthcareSetting */
  private Integer hsId;

  /** The list of artifacts and their status. */
  Set<KnowledgeArtifactStatus> artifactStatus;

  public HealthcareSettingOperationalKnowledgeArtifacts() {

    hsId = -1;
    artifactStatus = new HashSet<>();
  }

  public Integer getId() {
    return hsId;
  }

  public void setId(Integer id) {
    this.hsId = id;
  }

  public Set<KnowledgeArtifactStatus> getArtifactStatus() {
    return artifactStatus;
  }

  public void setArtifactStatus(Set<KnowledgeArtifactStatus> artifactStatus) {
    this.artifactStatus = artifactStatus;
  }

  public void log() {

    logger.info(
        " **** START Printing KnowledgeArtifactOperationalStatus For a Healthcare Setting **** ");

    logger.info(" Healthcare Setting Id : {}", hsId);
    artifactStatus.forEach(status -> status.log());

    logger.info(
        " **** END Printing KnowledgeArtifactOperationalStatus For a Healthcare Setting **** ");
  }
}
