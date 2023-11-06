package com.drajer.bsa.service.impl;

import com.drajer.bsa.dao.KarDao;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import com.drajer.bsa.service.KarService;
import java.util.Date;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class KarServiceImpl implements KarService {

  @Autowired KarDao karDao;

  @Override
  public KnowledgeArtifactRepository saveOrUpdate(KnowledgeArtifactRepository kar) {
    karDao.saveOrUpdate(kar);
    return kar;
  }

  @Override
  public KnowledgeArtifactRepository getKARById(Integer id) {
    return karDao.getKARById(id);
  }

  @Override
  public KnowledgeArtifactRepository getKARByUrl(String url) {
    return karDao.getKARByUrl(url);
  }

  @Override
  public List<KnowledgeArtifactRepository> getAllKARs() {
    return karDao.getAllKARs();
  }

  @Override
  public KnowledgeArtifactStatus saveOrUpdateKARStatus(KnowledgeArtifactStatus karStatus) {
    if (karStatus.getIsActive().booleanValue()) {
      karStatus.setLastActivationDate(new Date());
    } else {
      karStatus.setLastInActivationDate(new Date());
    }
    karDao.saveOrUpdateKARStatus(karStatus);
    return karStatus;
  }

  @Override
  public List<KnowledgeArtifactStatus> getKARStatusByHsId(Integer hsId) {
    return karDao.getKARStatusByHsId(hsId);
  }

  @Override
  public KnowledgeArtifactStatus getKarStatusByKarIdAndKarVersion(
      String karId, String karVersion, Integer hsId) {
    return karDao.getKarStausByKarIdAndKarVersion(karId, karVersion, hsId);
  }
}
