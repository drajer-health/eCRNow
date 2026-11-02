package com.drajer.bsa.dto;

import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes.OutputContentType;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * Request body for creating or updating a {@link KnowledgeArtifactStatus}. It accepts the same JSON
 * properties as the entity, so the API contract is unchanged, and is converted to the entity with
 * {@link #toEntity()} so the persistence class is never bound directly from the request.
 */
public class KnowledgeArtifactStatusRequest {

  private Integer id;
  private Integer hsId;
  private String karId;
  private Boolean isActive;
  private Date lastActivationDate;
  private Date lastInActivationDate;
  private String karVersion;
  private Boolean subscriptionsEnabled;
  private Set<String> subscriptions;
  private String versionUniqueKarId;
  private Boolean covidOnly;
  private OutputContentType outputFormat;

  public KnowledgeArtifactStatusRequest() {
    karId = "";
    isActive = true;
    subscriptionsEnabled = false;
    outputFormat = OutputContentType.BOTH;
    lastActivationDate = new Date();
    lastInActivationDate = new Date();
    subscriptions = new HashSet<>();
  }

  /** Creates a new KnowledgeArtifactStatus carrying every value of this request. */
  public KnowledgeArtifactStatus toEntity() {
    KnowledgeArtifactStatus entity = new KnowledgeArtifactStatus();
    entity.setId(id);
    entity.setHsId(hsId);
    entity.setKarId(karId);
    entity.setIsActive(isActive);
    entity.setLastActivationDate(lastActivationDate);
    entity.setLastInActivationDate(lastInActivationDate);
    entity.setKarVersion(karVersion);
    entity.setSubscriptionsEnabled(subscriptionsEnabled);
    entity.setSubscriptions(subscriptions);
    entity.setVersionUniqueKarId(versionUniqueKarId);
    entity.setCovidOnly(covidOnly);
    entity.setOutputFormat(outputFormat);
    return entity;
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

  public void setKarId(String karId) {
    this.karId = karId;
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
