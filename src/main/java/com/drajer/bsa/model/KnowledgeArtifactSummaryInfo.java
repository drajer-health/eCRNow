package com.drajer.bsa.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import org.hibernate.annotations.DynamicUpdate;

@Entity
@Table(name = "kar_info")
@DynamicUpdate
public class KnowledgeArtifactSummaryInfo {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  @Column(name = "plan_definition_id", nullable = false, columnDefinition = "TEXT")
  private String planDefinitionId;

  @Column(name = "plan_definition_name", nullable = false, columnDefinition = "TEXT")
  private String planDefinitionName;

  @Column(name = "plan_definition_publisher", nullable = false, columnDefinition = "TEXT")
  private String planDefinitionPublisher;

  @Column(name = "plan_definition_version", nullable = false, columnDefinition = "TEXT")
  private String planDefinitionVersion;

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public String getPlanDefinitionId() {
    return planDefinitionId;
  }

  public void setPlanDefinitionId(String planDefinitionId) {
    this.planDefinitionId = planDefinitionId;
  }

  public String getPlanDefinitionName() {
    return planDefinitionName;
  }

  public void setPlanDefinitionName(String planDefinitionName) {
    this.planDefinitionName = planDefinitionName;
  }

  public String getPlanDefinitionPublisher() {
    return planDefinitionPublisher;
  }

  public void setPlanDefinitionPublisher(String planDefinitionPublisher) {
    this.planDefinitionPublisher = planDefinitionPublisher;
  }

  public String getPlanDefinitionVersion() {
    return planDefinitionVersion;
  }

  public void setPlanDefinitionVersion(String planDefinitionVersion) {
    this.planDefinitionVersion = planDefinitionVersion;
  }
}
