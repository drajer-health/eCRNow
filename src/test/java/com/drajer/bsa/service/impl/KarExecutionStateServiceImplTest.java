package com.drajer.bsa.service.impl;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.drajer.bsa.dao.KarExecutionStateDao;
import com.drajer.bsa.model.KarExecutionState;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class KarExecutionStateServiceImplTest {

  @Mock KarExecutionStateDao KeDao;

  @InjectMocks KarExecutionStateServiceImpl karExecutionStateService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testSaveOrUpdate() {
    KarExecutionState kar = mock(KarExecutionState.class);
    kar.setId(UUID.fromString("123e4567-e89b-12d3-a456-426614174000"));
    kar.setKarUniqueId("27PB");
    kar.setHsFhirServerUrl("http://hsfhirserver.com");
    when(KeDao.saveOrUpdate(any())).thenReturn(kar);
    KarExecutionState result = karExecutionStateService.saveOrUpdate(kar);
    assertNotNull(kar);
    assertEquals(kar.getKarUniqueId(), result.getKarUniqueId());
    assertEquals(kar.getId(), result.getId());
    verify(KeDao, times(1)).saveOrUpdate(kar);
  }

  @Test
  public void testGetKarExecutionStateById() {
    KarExecutionState kar = mock(KarExecutionState.class);
    kar.setId(UUID.fromString("123e4567-e89b-12d3-a456-426614174000"));
    when(KeDao.getKarExecutionStateById(any(UUID.class))).thenReturn(kar);
    KarExecutionState result =
        karExecutionStateService.getKarExecutionStateById(
            UUID.fromString("123e4567-e89b-12d3-a456-426614174000"));
    assertNotNull(kar);
    assertEquals(kar.getId(), result.getId());
    verify(KeDao, times(1)).getKarExecutionStateById((any(UUID.class)));
  }

  @Test
  public void testGetAllKarExecutionStates() {
    KarExecutionState kar = mock(KarExecutionState.class);
    kar.setId(UUID.fromString("123e4567-e89b-12d3-a456-426614174000"));
    kar.setKarUniqueId("3527ec");
    kar.setHsFhirServerUrl("http://hsfhirserver.com");
    List<KarExecutionState> karExecutionStateList = new ArrayList<>();
    karExecutionStateList.add(kar);
    when(KeDao.getAllKarExecutionStates()).thenReturn(karExecutionStateList);
    List<KarExecutionState> result = karExecutionStateService.getAllKarExecutionStates();
    assertNotNull(result);
    assertEquals(kar.getId(), result.get(0).getId());
    assertEquals(kar.getHsFhirServerUrl(), result.get(0).getHsFhirServerUrl());
    assertEquals(kar.getKarUniqueId(), result.get(0).getKarUniqueId());
    verify(KeDao, times(1)).getAllKarExecutionStates();
  }

  @Test
  public void testDelete() {
    KarExecutionState kar = mock(KarExecutionState.class);
    kar.setId(UUID.fromString("123e4567-e89b-12d3-a456-426614174000"));
    doNothing().when(KeDao).delete(any(KarExecutionState.class));
    karExecutionStateService.delete(kar);
    verify(KeDao, times(1)).delete(kar);
  }
}
