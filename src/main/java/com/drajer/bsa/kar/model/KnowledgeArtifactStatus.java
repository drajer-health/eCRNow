package com.drajer.bsa.kar.model;

import com.drajer.bsa.model.BsaTypes.OutputContentType;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This entity is used to manage the Knowledge Artifact status for each HealthcareSetting. When the
 * isActive flag is true, the Knowledge Artifact [having an id populated in the instance] will be
 * evaluated based on the notification.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
public class KnowledgeArtifactStatus {

  private final Logger logger = LoggerFactory.getLogger(KnowledgeArtifactStatus.class);

  /** The Id of the HealthcareSetting */
  private Integer hsId;

  /**
   * The Id of the Knowledge Artifact as defined by the Public Health Agency or Research
   * Organization.
   */
  String karId;

  /**
   * The Version of the Knowledge Artifact as defined by the Public Health Agency or Research
   * Organization.
   */
  String karVersion;

  /**
   * The version unique id of the Knowledge Artifact as defined by the Public Health Agency or
   * Research Organization. This is a concatenation of Id and version.
   */
  String versionUniqueKarId;

  /**
   * The status of the Knowledge Artifact provisioned during the HealthcareSetting configuration.
   * True - Indicates that the Knowledge Artifact has to be processed. False - Indicates that the
   * Knowledge Artifact should not be processed.
   */
  Boolean isActive;

  /** The last time the Knowledge Artifact became active. */
  Date lastActivationDate;

  /** The last time the Knowledge Artifact became inactive. */
  Date lastInActivationDate;

  /**
   * The flag indicates if subscriptions have been enabled for the artifact. This will typically be
   * set to true when a KnowledeArtifact gets processed and becomes active. When the
   * KnowledgeArtifact becomes inactive, the subscriptions should be removed.
   */
  Boolean subscriptionsEnabled;

  /**
   * The list of subscriptions that have been enabled by the KnowledgeArtifact for the specific
   * HealthcareSetting.
   */
  Set<String> subscriptions;

  /**
   * The flag indicates the type of output to be produced. If the output format is FHIR, then a FHIR
   * bundle would be produced for the KAR. If the output format is CDA, then a CDA document would be
   * produced for the KAR.
   */
  OutputContentType outputFormat;

  public KnowledgeArtifactStatus() {

    karId = "";
    isActive = true;
    subscriptionsEnabled = false;
    outputFormat = OutputContentType.Both;
    lastActivationDate = new Date();
    lastInActivationDate = new Date();
    subscriptions = new HashSet<String>();
  }

  public void log() {

    logger.info(" **** START Printing Knowledge Artifact Status **** ");

    logger.info(" Kar Id {} ", karId);
    logger.info(" Kar Version : {}", karVersion);
    logger.info(" Kar Version Unique Id : {}", versionUniqueKarId);
    logger.info(" Kar IsActive : {} ", isActive);
    logger.info(" Kar Subscriptions Enabled : {} ", subscriptionsEnabled);
    logger.info(" Kar Last activation date : {} ", lastActivationDate.toString());
    logger.info(" Kar Last in-activation date : {} ", lastInActivationDate.toString());

    subscriptions.forEach(subscription -> logger.info(" Subscription id : {} ", subscription));

    logger.info(" **** START Printing Knowledge Artifact Status **** ");
  }

  public String getKarId() {
    return karId;
  }

  public void setKarId(String id) {
    this.karId = id;
  }

  public Boolean getIsActive() {
    return isActive;
  }

  public void setIsActive(Boolean isActive) {
    this.isActive = isActive;
  }

  public Date getLastActivationDate() {
    return lastActivationDate;
  }

  public void setLastActivationDate(Date lastActivationDate) {
    this.lastActivationDate = lastActivationDate;
  }

  public Date getLastInActivationDate() {
    return lastInActivationDate;
  }

  public void setLastInActivationDate(Date lastInActivationDate) {
    this.lastInActivationDate = lastInActivationDate;
  }

  public String getKarVersion() {
    return karVersion;
  }

  public void setKarVersion(String karVersion) {
    this.karVersion = karVersion;
  }

  public Boolean getSubscriptionsEnabled() {
    return subscriptionsEnabled;
  }

  public void setSubscriptionsEnabled(Boolean subscriptionsEnabled) {
    this.subscriptionsEnabled = subscriptionsEnabled;
  }

  public Set<String> getSubscriptions() {
    return subscriptions;
  }

  public void setSubscriptions(Set<String> subscriptions) {
    this.subscriptions = subscriptions;
  }

  public boolean equals(Object o) {
    return (o instanceof KnowledgeArtifactStatus)
        && (((KnowledgeArtifactStatus) o).getKarId()).equals(this.getKarId());
  }

  public int hashCode() {
    return karId.hashCode();
  }

  public String getVersionUniqueKarId() {
    return versionUniqueKarId;
  }

  public void setVersionUniqueKarId(String versionUniqueKarId) {
    this.versionUniqueKarId = versionUniqueKarId;
  }
}
