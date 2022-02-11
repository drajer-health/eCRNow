package com.drajer.bsa.service;

import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifiactRepository;
import java.util.List;

public interface KarService {

  public KnowledgeArtifiactRepository saveOrUpdate(KnowledgeArtifiactRepository kar);

  public KnowledgeArtifiactRepository getKARById(Integer id);

  public KnowledgeArtifiactRepository getKARByUrl(String url);

  public List<KnowledgeArtifiactRepository> getAllKARs();

  public KnowledgeArtifactStatus saveOrUpdateKARStatus(KnowledgeArtifactStatus karStatus);

  public List<KnowledgeArtifactStatus> getKARStatusByHsId(Integer hsId);

  public KnowledgeArtifactStatus getKarStatusByKarIdAndKarVersion(String karId, String karVersion);
}
