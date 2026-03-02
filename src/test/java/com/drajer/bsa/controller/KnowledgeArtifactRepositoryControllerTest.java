package com.drajer.bsa.controller;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import com.drajer.bsa.service.KarParser;
import com.drajer.bsa.service.KarService;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Arrays;
import java.util.List;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

public class KnowledgeArtifactRepositoryControllerTest {

  @Mock private KarService karService;

  @Mock private KarParser karParser;

  @InjectMocks private KnowledgeArtifactRepositoryController karController;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  public static final String KAR_STATUS_JSON = "Bsa/kar-status.json";

  // =====================================================
  // GET KAR BY ID
  // =====================================================
  @Test
  public void testShouldReturnKARWhenGetKnowledgeArtifactById() {

    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setId(1);
    kar.setFhirServerURL("http://hosp.com");

    Mockito.lenient().when(karService.getKARById(1)).thenReturn(kar);

    KnowledgeArtifactRepository result = karController.getKnowledgeArtifactById(1);

    assertNotNull(result);
    assertEquals(Integer.valueOf(1), result.getId());

    verify(karService).getKARById(1);
  }

  // =====================================================
  // CREATE KAR
  // =====================================================
  @Test
  public void testShouldCreateNewKARWhenNoExistingKAR() {

    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setFhirServerURL("http://hosp.com");

    Mockito.lenient().when(karService.getKARByUrl(anyString())).thenReturn(null);

    ResponseEntity<Object> response = karController.createKARs(kar);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    verify(karService).saveOrUpdate(kar);
  }

  @Test
  public void testShouldReturnErrorWhenKARAlreadyExists() throws Exception {

    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setFhirServerURL("http://hosp.com");

    KnowledgeArtifactRepository existingKar = new KnowledgeArtifactRepository();
    existingKar.setId(99);

    Mockito.lenient().when(karService.getKARByUrl(anyString())).thenReturn(existingKar);

    ResponseEntity<Object> response = karController.createKARs(kar);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());

    JSONObject body = (JSONObject) response.getBody();
    assertEquals("error", body.get("status"));

    verify(karService, never()).saveOrUpdate(any());
  }

  // =====================================================
  // GET ALL KARs
  // =====================================================
  @Test
  public void testShouldReturnAllKARsWhenExist() {

    KnowledgeArtifactRepository kar1 = new KnowledgeArtifactRepository();
    kar1.setId(1);

    KnowledgeArtifactRepository kar2 = new KnowledgeArtifactRepository();
    kar2.setId(2);

    Mockito.lenient().when(karService.getAllKARs()).thenReturn(Arrays.asList(kar1, kar2));

    List<KnowledgeArtifactRepository> result = karController.getAllKARs();

    assertEquals(2, result.size());

    verify(karService).getAllKARs();
  }

  // =====================================================
  // LOAD KARs
  // =====================================================
  @Test
  public void testLoadKarsSuccess() {

    doNothing().when(karParser).loadKars();

    ResponseEntity<Object> response = karController.loadKars();

    assertEquals(HttpStatus.OK, response.getStatusCode());

    verify(karParser).loadKars();
  }

  @Test
  public void testShouldHandleLoadKARsFailure() {

    doThrow(new RuntimeException("Test")).when(karParser).loadKars();

    ResponseEntity<Object> response = karController.loadKars();

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, response.getStatusCode());
  }

  // =====================================================
  // ADD KAR STATUS (🔥 USING JSON ARRAY FILE)
  // =====================================================
  @Test
  public void testShouldAddKARStatus_usingJsonArray() {

    List<KnowledgeArtifactStatus> karStatuses =
        TestUtils.readFileContents(
            KAR_STATUS_JSON, new TypeReference<List<KnowledgeArtifactStatus>>() {});

    Mockito.lenient()
        .when(karService.getKarStatusByKarIdAndKarVersion(anyString(), anyString(), anyInt()))
        .thenReturn(null);

    ResponseEntity<List<KnowledgeArtifactStatus>> response =
        karController.addKARStatus(karStatuses);

    verify(karService, times(1)).saveOrUpdateKARStatus(any(KnowledgeArtifactStatus.class));

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertNotNull(response.getBody());
    assertEquals(1, response.getBody().size());
  }
}
