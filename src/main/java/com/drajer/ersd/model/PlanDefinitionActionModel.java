package com.drajer.ersd.model;

import jakarta.persistence.Column;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import org.hibernate.annotations.DynamicUpdate;

// @Entity - No need to persist.
@Table(name = "plandefinitionactions")
@DynamicUpdate
public class PlanDefinitionActionModel {

  @Id
  @Column(name = "id")
  private String id;

  @Column(name = "actionid")
  private String actionId;

  @Column(name = "actiontriggertype")
  private String actionTriggerType;

  @Column(name = "actionelementtype")
  private String actionElementType;

  @Column(name = "actionpath")
  private String actionPath;

  @Column(name = "actionvaluesetgrouper")
  private String actionValueSetGrouper;

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getActionId() {
    return actionId;
  }

  public void setActionId(String actionId) {
    this.actionId = actionId;
  }

  public String getActionTriggerType() {
    return actionTriggerType;
  }

  public void setActionTriggerType(String actionTriggerType) {
    this.actionTriggerType = actionTriggerType;
  }

  public String getActionElementType() {
    return actionElementType;
  }

  public void setActionElementType(String actionElementType) {
    this.actionElementType = actionElementType;
  }

  public String getActionPath() {
    return actionPath;
  }

  public void setActionPath(String actionPath) {
    this.actionPath = actionPath;
  }

  public String getActionValueSetGrouper() {
    return actionValueSetGrouper;
  }

  public void setActionValueSetGrouper(String actionValueSetGrouper) {
    this.actionValueSetGrouper = actionValueSetGrouper;
  }
}
