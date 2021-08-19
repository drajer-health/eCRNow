package com.drajer.bsa.service;

import com.drajer.bsa.model.KnowledgeArtifiactRepository;
import java.util.List;

public interface KarService {

  public KnowledgeArtifiactRepository saveOrUpdate(KnowledgeArtifiactRepository kar);

  public KnowledgeArtifiactRepository getKARById(Integer id);

  public KnowledgeArtifiactRepository getKARByUrl(String url);

  public List<KnowledgeArtifiactRepository> getAllKARs();
}
