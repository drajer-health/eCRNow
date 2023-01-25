package com.drajer.bsa.service;

import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import java.util.List;

public interface KarService {

  public KnowledgeArtifactRepository saveOrUpdate(KnowledgeArtifactRepository kar);

  public KnowledgeArtifactRepository getKARById(Integer id);

  public KnowledgeArtifactRepository getKARByUrl(String url);

  public List<KnowledgeArtifactRepository> getAllKARs();

  public KnowledgeArtifactStatus saveOrUpdateKARStatus(KnowledgeArtifactStatus karStatus);

  public List<KnowledgeArtifactStatus> getKARStatusByHsId(Integer hsId);

  public KnowledgeArtifactStatus getKarStatusByKarIdAndKarVersion(
      String karId, String karVersion, Integer hsId);
}
