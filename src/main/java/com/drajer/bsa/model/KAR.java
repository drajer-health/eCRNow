package com.drajer.bsa.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.util.Set;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;

@Entity
@Table(name = "kar_mng")
@DynamicUpdate
@JsonInclude(Include.NON_NULL)
public class KAR {

  /** The attribute represents the primary key for the table and is auto incremented. */
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  /** The attribute represents the client id that is to be used for SMART on FHIR Authorization. */
  @Column(name = "fhirServerURL", nullable = false, columnDefinition = "TEXT")
  private String fhirServerURL;

  @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
  @Fetch(FetchMode.SELECT)
  @JoinColumn(name = "kar_id")
  private Set<KARInfo> kars_info;

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public String getFhirServerURL() {
    return fhirServerURL;
  }

  public void setFhirServerURL(String fhirServerURL) {
    this.fhirServerURL = fhirServerURL;
  }

  public Set<KARInfo> getKars_info() {
    return kars_info;
  }

  public void setKars_info(Set<KARInfo> kars_info) {
    this.kars_info = kars_info;
  }
}
