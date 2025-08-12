package com.drajer.bsa.service.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.drajer.bsa.dao.KarDao;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class KarServiceImplTest {
  @Mock KarDao karDao;

  @InjectMocks KarServiceImpl karServiceImpl;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testsaveOrUpdate() {
    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setId(1234);
    kar.setRepoName("ArtifactRepo");
    kar.setRepoStatus(true);
    when(karDao.saveOrUpdate(any())).thenReturn(kar);
    KnowledgeArtifactRepository result = karServiceImpl.saveOrUpdate(kar);
    assertNotNull(kar);
    assertEquals(kar.getId(), result.getId());
    assertEquals(kar.getRepoName(), result.getRepoName());
    assertTrue(kar.getRepoStatus());
    verify(karDao, times(1)).saveOrUpdate(kar);
  }

  @Test
  public void testgetKARById() {
    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setId(1234);
    when(karDao.getKARById(any())).thenReturn(kar);
    KnowledgeArtifactRepository result = karServiceImpl.getKARById(any());
    assertNotNull(result);
    assertEquals(kar.getId().longValue(), result.getId().longValue());
    verify(karDao, times(1)).getKARById(any());
  }

  @Test
  public void testgetKARByUrl() {
    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setFhirServerURL("http://fhirserver.com");
    when(karDao.getKARByUrl(any())).thenReturn(kar);
    KnowledgeArtifactRepository result = karServiceImpl.getKARByUrl(any());
    assertNotNull(result);
    assertEquals(kar.getFhirServerURL(), result.getFhirServerURL());
    verify(karDao, times(1)).getKARByUrl(any());
  }

  @Test
  public void testgetAllKARs() {
    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setId(1234);
    kar.setRepoName("ArtifactRepo");
    kar.setRepoStatus(true);
    List<KnowledgeArtifactRepository> knowledgeArtifactRepositoryList = new ArrayList<>();
    knowledgeArtifactRepositoryList.add(kar);
    when(karDao.getAllKARs()).thenReturn(knowledgeArtifactRepositoryList);
    List<KnowledgeArtifactRepository> result = karServiceImpl.getAllKARs();
    assertNotNull(result);
    assertEquals(kar.getId(), result.get(0).getId());
    assertEquals(kar.getRepoName(), result.get(0).getRepoName());
    assertTrue(kar.getRepoStatus());
    verify(karDao, times(1)).getAllKARs();
  }

  @Test
  public void testsaveOrUpdateKARStatus_Active() {
    KnowledgeArtifactStatus karStatus = new KnowledgeArtifactStatus();
    karStatus.setIsActive(true);
    when(karDao.saveOrUpdateKARStatus(any())).thenReturn(karStatus);
    KnowledgeArtifactStatus result = karServiceImpl.saveOrUpdateKARStatus(karStatus);
    assertNotNull(result);
    assertTrue(result.getIsActive());
    assertNotNull(result.getLastActivationDate());
    verify(karDao, times(1)).saveOrUpdateKARStatus(karStatus);
  }

  @Test
  public void testsaveOrUpdateKARStatus_Inactive() {
    KnowledgeArtifactStatus karStatus = new KnowledgeArtifactStatus();
    karStatus.setIsActive(false);
    when(karDao.saveOrUpdateKARStatus(any())).thenReturn(karStatus);

    KnowledgeArtifactStatus result = karServiceImpl.saveOrUpdateKARStatus(karStatus);

    assertNotNull(result);
    assertFalse(result.getIsActive());
    assertNotNull(result.getLastInActivationDate());
    verify(karDao, times(1)).saveOrUpdateKARStatus(karStatus);
  }

  @Test
  public void testgetKARStatusByHsId() {
    KnowledgeArtifactStatus karStatus = new KnowledgeArtifactStatus();
    karStatus.setHsId(1234);
    List<KnowledgeArtifactStatus> knowledgeArtifactStatusList = new ArrayList<>();
    knowledgeArtifactStatusList.add(karStatus);
    when(karDao.getKARStatusByHsId(any())).thenReturn(knowledgeArtifactStatusList);
    List<KnowledgeArtifactStatus> result = karServiceImpl.getKARStatusByHsId(any());
    assertNotNull(result);
    assertEquals(karStatus.getHsId(), result.get(0).getHsId());
    verify(karDao, times(1)).getKARStatusByHsId(any());
  }

  @Test
  public void testgetKarStatusByKarIdAndKarVersion() {
    KnowledgeArtifactStatus karStatus = new KnowledgeArtifactStatus();
    karStatus.setKarId("1234");
    karStatus.setKarVersion("1.0");
    when(karDao.getKarStausByKarIdAndKarVersion(any(), any(), any())).thenReturn(karStatus);
    KnowledgeArtifactStatus result =
        karServiceImpl.getKarStatusByKarIdAndKarVersion(any(), any(), any());
    assertNotNull(result);
    assertEquals(karStatus.getKarId(), result.getKarId());
    assertEquals(karStatus.getKarVersion(), result.getKarVersion());
    verify(karDao, times(1)).getKarStausByKarIdAndKarVersion(any(), any(), any());
  }
}
