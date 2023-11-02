package com.drajer.ersd.model;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;

// @Entity - No need to persist.
@Table(name = "valuesetgrouper")
public class ValueSetGrouperModel {

  @Id
  @Column(name = "id")
  private String id;

  @Column(name = "valuesetgrouper")
  private String valueSetGrouper;

  @Column(name = "valuesetid")
  private String valueSetId;

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getValueSetGrouper() {
    return valueSetGrouper;
  }

  public void setValueSetGrouper(String valueSetGrouper) {
    this.valueSetGrouper = valueSetGrouper;
  }

  public String getValueSetId() {
    return valueSetId;
  }

  public void setValueSetId(String valueSetId) {
    this.valueSetId = valueSetId;
  }
}
