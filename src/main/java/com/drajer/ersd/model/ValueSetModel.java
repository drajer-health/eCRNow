package com.drajer.ersd.model;

import com.drajer.ecrapp.config.JSONObjectUserType;
import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.UpdateTimestamp;

// @Entity - No need to persist.
@Table(name = "valueset")
@TypeDef(name = "StringJsonObject", typeClass = JSONObjectUserType.class)
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
