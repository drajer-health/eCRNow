package com.drajer.bsa.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import org.hibernate.annotations.DynamicUpdate;

/**
 *
 *
 * <h1>KnowledgeArtifactSummaryInfo</h1>
 *
 * This entity is used to store convenient information to display Knowledge Artifacts available from
 * a Knowledge Artifact Repository. Health Care settings may activate, deactivate and setup other
 * information related to teh processing of the KnowledgeArtifact.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
@Entity
@Table(name = "kar_info")
@DynamicUpdate
public class KnowledgeArtifactSummaryInfo {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  @Column(name = "kar_id", nullable = false, columnDefinition = "TEXT")
  private String karId;

  @Column(name = "kar_name", nullable = false, columnDefinition = "TEXT")
  private String karName;

  @Column(name = "kar_publisher", nullable = false, columnDefinition = "TEXT")
  private String karPublisher;

  @Column(name = "kar_version", nullable = false, columnDefinition = "TEXT")
  private String karVersion;

  public String getVersionUniqueId() {
    return this.karId + "|" + this.getKarVersion();
  }

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public String getKarId() {
    return karId;
  }

  public void setKarId(String karId) {
    this.karId = karId;
  }

  public String getKarName() {
    return karName;
  }

  public void setKarName(String karName) {
    this.karName = karName;
  }

  public String getKarPublisher() {
    return karPublisher;
  }

  public void setKarPublisher(String karPublisher) {
    this.karPublisher = karPublisher;
  }

  public String getKarVersion() {
    return karVersion;
  }

  public void setKarVersion(String karVersion) {
    this.karVersion = karVersion;
  }
}
