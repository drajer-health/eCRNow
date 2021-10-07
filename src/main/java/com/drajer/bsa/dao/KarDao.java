package com.drajer.bsa.dao;

import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifiactRepository;
import java.util.List;

/**
 *
 *
 * <h1>KarDao</h1>
 *
 * This interface declares methods to perform CRUD operations on the Knowledge Artifact.
 *
 * @author nbashyam
 */
public interface KarDao {

  public KnowledgeArtifiactRepository saveOrUpdate(KnowledgeArtifiactRepository kar);

  public KnowledgeArtifiactRepository getKARById(Integer id);

  public KnowledgeArtifiactRepository getKARByUrl(String url);

  public List<KnowledgeArtifiactRepository> getAllKARs();

  public KnowledgeArtifactStatus saveOrUpdateKARStatus(KnowledgeArtifactStatus karStatus);

  public List<KnowledgeArtifactStatus> getKARStatusByHsId(Integer hsId);

  public KnowledgeArtifactStatus getKarStausByKarIdAndKarVersion(String karId, String karVersion);
}
