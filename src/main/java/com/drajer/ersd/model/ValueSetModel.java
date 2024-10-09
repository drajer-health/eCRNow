package com.drajer.ersd.model;

import jakarta.persistence.Column;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import java.util.Date;
import org.hibernate.annotations.UpdateTimestamp;

// @Entity - No need to persist.
@Table(name = "valueset")
public class ValueSetModel {

  @Id
  @Column(name = "id")
  private String valueSetId;

  @Column(name = "data", columnDefinition = "TEXT")
  private String data;

  @Column(name = "last_updated_ts")
  @Temporal(TemporalType.TIMESTAMP)
  @UpdateTimestamp
  private Date timestamp;

  public String getValueSetId() {
    return valueSetId;
  }

  public void setValueSetId(String valueSetId) {
    this.valueSetId = valueSetId;
  }

  public String getData() {
    return data;
  }

  public void setData(String data) {
    this.data = data;
  }

  public Date getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(Date timestamp) {
    this.timestamp = timestamp;
  }
}
