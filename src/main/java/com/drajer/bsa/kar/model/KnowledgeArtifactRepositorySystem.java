package com.drajer.bsa.kar.model;

import java.util.HashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This entity is used to store convenient information for operational evaluation of knowledge
 * artifacts. This is initialized by the KnowledgeArtifactProcessor during startup and or at a later
 * point of time. The class implements a Singleton pattern so that it can be accessed
 * anytime/anywhere from the rest of the modules.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
public class KnowledgeArtifactRepositorySystem {

  /** Singleton Instance for the Repository. */
  private static KnowledgeArtifactRepositorySystem instance = null;

  private final Logger logger = LoggerFactory.getLogger(KnowledgeArtifactRepositorySystem.class);

  /** The list of Knowledge Artifacts accessible by its identifiers. */
  HashMap<String, KnowledgeArtifact> artifacts;

  private KnowledgeArtifactRepositorySystem() {
    artifacts = new HashMap<>();
  }

  public HashMap<String, KnowledgeArtifact> getArtifacts() {
    return artifacts;
  }

  public void setArtifacts(HashMap<String, KnowledgeArtifact> artifacts) {
    this.artifacts = artifacts;
  }

  public void remove(String karId) {

    if (artifacts.containsKey(karId)) artifacts.remove(karId);
  }

  public void add(KnowledgeArtifact kar) {

    if (!artifacts.containsKey(kar.getVersionUniqueId()))
      artifacts.put(kar.getVersionUniqueId(), kar);
  }

  public KnowledgeArtifact getById(String id) {

    if (artifacts != null && artifacts.containsKey(id)) return artifacts.get(id);
    else return null;
  }

  public static KnowledgeArtifactRepositorySystem getInstance() {

    if (instance == null) instance = new KnowledgeArtifactRepositorySystem();

    return instance;
  }

  public void log() {

    logger.info(" **** START Printing Knowledge Artifact Repository **** ");

    if (artifacts != null) artifacts.forEach((key, value) -> value.log());

    logger.info(" **** END Printing Knowledge Artifact Repository **** ");
  }
}
