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

/*
 * <h1>KnowledgeArtifactRepository</h1>
 *
 * An instance of this class is created for each FHIR Server that is hosting Knowledge Artifacts.
 *
 * @author nbashyam
 */
@Entity
@Table(name = "kar_repos")
@DynamicUpdate
@JsonInclude(Include.NON_NULL)
public class KnowledgeArtifiactRepository {

  /** The attribute represents the primary key for the table and is auto incremented. */
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  /** The attribute represents the FHIR Server URL which hosts the Knowledge Artifact. */
  @Column(name = "repo_fhir_url", nullable = false, columnDefinition = "TEXT")
  private String fhirServerURL;

  /**
   * The attribute represents the FHIR Server URL for the HealthcareSetting. This is unique for the
   * entire table.
   */
  @Column(name = "repo_name", nullable = false, unique = true)
  private String repoName;

  @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
  @Fetch(FetchMode.SELECT)
  @JoinColumn(name = "repo_id")
  private Set<KnowledgeArtifactSummaryInfo> karsInfo;

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

  public Set<KnowledgeArtifactSummaryInfo> getKars_info() {
    return karsInfo;
  }

  public void setKars_info(Set<KnowledgeArtifactSummaryInfo> karsinfo) {
    this.karsInfo = karsinfo;
  }

  public String getRepoName() {
    return repoName;
  }

  public void setRepoName(String repoName) {
    this.repoName = repoName;
  }

  public Set<KnowledgeArtifactSummaryInfo> getKarsInfo() {
    return karsInfo;
  }

  public void setKarsInfo(Set<KnowledgeArtifactSummaryInfo> karsInfo) {
    this.karsInfo = karsInfo;
  }
}
