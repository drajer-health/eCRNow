package com.drajer.bsa.kar.model;

import com.drajer.bsa.model.BsaTypes.OutputContentType;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.TypeDefs;
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
@Entity
@Table(name = "hs_kar_status")
@DynamicUpdate
@TypeDefs({@TypeDef(name = "SetOfStringsUserType", typeClass = SetOfStringsUserType.class)})
public class KnowledgeArtifactStatus {

  @Transient private final Logger logger = LoggerFactory.getLogger(KnowledgeArtifactStatus.class);

  /** The attribute represents the primary key for the table and is auto incremented. */
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  /** The Id of the HealthcareSetting */
  @Column(name = "hs_id", nullable = false)
  private Integer hsId;

  /**
   * The Id of the Knowledge Artifact as defined by the Public Health Agency or Research
   * Organization.
   */
  @Column(name = "kar_id", nullable = false)
  String karId;

  /**
   * The Version of the Knowledge Artifact as defined by the Public Health Agency or Research
   * Organization.
   */
  @Column(name = "kar_version", columnDefinition = "TEXT", nullable = false)
  String karVersion;

  /**
   * The version unique id of the Knowledge Artifact as defined by the Public Health Agency or
   * Research Organization. This is a concatenation of Id and version.
   */
  @Column(name = "map_versionid_karid", columnDefinition = "TEXT", unique = true, nullable = false)
  String versionUniqueKarId;

  /**
   * The status of the Knowledge Artifact provisioned during the HealthcareSetting configuration.
   * True - Indicates that the Knowledge Artifact has to be processed. False - Indicates that the
   * Knowledge Artifact should not be processed.
   */
  @Column(name = "is_activated", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  Boolean isActive;

  /** The last time the Knowledge Artifact became active. */
  @Column(name = "last_activation_date")
  Date lastActivationDate;

  /** The last time the Knowledge Artifact became inactive. */
  @Column(name = "last_inactivation_date")
  Date lastInActivationDate;

  /**
   * The flag indicates if subscriptions have been enabled for the artifact. This will typically be
   * set to true when a KnowledeArtifact gets processed and becomes active. When the
   * KnowledgeArtifact becomes inactive, the subscriptions should be removed.
   */
  @Column(name = "is_subscriptions_enabled", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  Boolean subscriptionsEnabled;

  /**
   * The list of subscriptions that have been enabled by the KnowledgeArtifact for the specific
   * HealthcareSetting.
   */
  @Column(name = "subscriptions", columnDefinition = "TEXT")
  @Type(type = "SetOfStringsUserType")
  Set<String> subscriptions;

  @Column(name = "is_only_covid", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  Boolean covidOnly;

  /**
   * The flag indicates the type of output to be produced. If the output format is FHIR, then a FHIR
   * bundle would be produced for the KAR. If the output format is CDA, then a CDA document would be
   * produced for the KAR.
   */
  @Enumerated(EnumType.STRING)
  @Column(name = "output_format", columnDefinition = "TEXT")
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

    // subscriptions.forEach(subscription -> logger.info(" Subscription id : {} ", subscription));

    logger.info(" **** START Printing Knowledge Artifact Status **** ");
  }

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public Integer getHsId() {
    return hsId;
  }

  public void setHsId(Integer hsId) {
    this.hsId = hsId;
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

  public Boolean getCovidOnly() {
    return covidOnly;
  }

  public void setCovidOnly(Boolean covidOnly) {
    this.covidOnly = covidOnly;
  }

  public OutputContentType getOutputFormat() {
    return outputFormat;
  }

  public void setOutputFormat(OutputContentType outputFormat) {
    this.outputFormat = outputFormat;
  }
}
