package com.drajer.bsa.dto;

import com.drajer.bsa.model.KnowledgeArtifactRepository;
import com.drajer.bsa.model.KnowledgeArtifactSummaryInfo;
import java.util.HashSet;
import java.util.Set;

/**
 * Request body for creating or updating a {@link KnowledgeArtifactRepository}. It accepts the same
 * JSON properties as the entity, so the API contract is unchanged, and is converted to the entity
 * with {@link #toEntity()} so the persistence class is never bound directly from the request.
 */
public class KnowledgeArtifactRepositoryRequest {

  private Integer id;
  private String fhirServerURL;
  private String repoName;
  private Set<KnowledgeArtifactSummaryInfo> karsInfo;
  private Boolean repoStatus;

  public KnowledgeArtifactRepositoryRequest() {
    karsInfo = new HashSet<>();
  }

  /** Creates a new KnowledgeArtifactRepository carrying every value of this request. */
  public KnowledgeArtifactRepository toEntity() {
    KnowledgeArtifactRepository entity = new KnowledgeArtifactRepository();
    entity.setId(id);
    entity.setFhirServerURL(fhirServerURL);
    entity.setRepoName(repoName);
    entity.setKarsInfo(karsInfo);
    entity.setRepoStatus(repoStatus);
    return entity;
  }

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

  public Boolean getRepoStatus() {
    return repoStatus;
  }

  public void setRepoStatus(Boolean repoStatus) {
    this.repoStatus = repoStatus;
  }
}
