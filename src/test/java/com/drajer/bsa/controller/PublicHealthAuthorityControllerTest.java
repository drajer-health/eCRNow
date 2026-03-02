package com.drajer.bsa.controller;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.PublicHealthAuthority;
import com.drajer.bsa.service.PublicHealthAuthorityService;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@RunWith(MockitoJUnitRunner.class)
public class PublicHealthAuthorityControllerTest {

  @InjectMocks private PublicHealthAuthorityController controller;

  @Mock private PublicHealthAuthorityService service;

  private PublicHealthAuthority pha;

  @Before
  public void setup() {
    pha = new PublicHealthAuthority();
    pha.setId(1);
    pha.setFhirServerBaseURL("http://pha.test");
  }

  @Test
  public void testCreatePublicHealthAuthority_New() {
    when(service.getPublicHealthAuthorityByUrl(anyString())).thenReturn(null);
    ResponseEntity<Object> response = controller.createPublicHealthAuthority(pha);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(service, times(1)).saveOrUpdate(pha);
    assertEquals(pha, response.getBody());
  }

  @Test
  public void testUpdatePublicHealthAuthority_NoExisting() {
    when(service.getPublicHealthAuthorityByUrl(anyString())).thenReturn(null);
    ResponseEntity<Object> response = controller.updatePublicHealthAuthority(pha);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(service, times(1)).saveOrUpdate(pha);
  }

  @Test
  public void testUpdatePublicHealthAuthority_SameId() {
    PublicHealthAuthority existing = new PublicHealthAuthority();
    existing.setId(1);
    when(service.getPublicHealthAuthorityByUrl(anyString())).thenReturn(existing);
    ResponseEntity<Object> response = controller.updatePublicHealthAuthority(pha);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(service, times(1)).saveOrUpdate(pha);
  }

  @Test
  public void testGetPublicHealthAuthorityById() {
    when(service.getPublicHealthAuthorityById(1)).thenReturn(pha);
    PublicHealthAuthority result = controller.getPublicHealthAuthorityById(1);
    assertNotNull(result);
    assertEquals("http://pha.test", result.getFhirServerBaseURL());
  }

  @Test
  public void testGetPublicHealthAuthorityByUrl() {
    when(service.getPublicHealthAuthorityByUrl("http://pha.test")).thenReturn(pha);
    PublicHealthAuthority result = controller.getPublicHealthAuthorityByUrl("http://pha.test");
    assertNotNull(result);
    assertEquals(1, result.getId().intValue());
  }

  @Test
  public void testGetAllPublicHealthAuthority() {
    List<PublicHealthAuthority> list = Arrays.asList(pha);
    when(service.getAllPublicHealthAuthority()).thenReturn(list);
    List<PublicHealthAuthority> result = controller.getAllPublicHealthAuthority();
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void testUpdatePublicHealthAuthority_DifferentId_ErrorPathCovered() {
    PublicHealthAuthority incoming = new PublicHealthAuthority();
    incoming.setId(1);
    incoming.setFhirServerBaseURL("http://pha.test");
    PublicHealthAuthority existing = new PublicHealthAuthority();
    existing.setId(99);
    existing.setFhirServerBaseURL("http://pha.test");
    when(service.getPublicHealthAuthorityByUrl("http://pha.test")).thenReturn(existing);
    ResponseEntity<Object> response = controller.updatePublicHealthAuthority(incoming);
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertNotNull(response.getBody());
    String body = response.getBody().toString();
    assertTrue(body.contains("\"status\":\"error\""));
    assertTrue(body.contains("already registered with a different Id"));
    verify(service, never()).saveOrUpdate(any());
  }

  @Test
  public void testCreatePublicHealthAuthority_AlreadyExists_COVERAGE() {

    PublicHealthAuthority existing = new PublicHealthAuthority();
    existing.setId(99);
    existing.setFhirServerBaseURL("http://pha.test");
    when(service.getPublicHealthAuthorityByUrl("http://pha.test")).thenReturn(existing);
    ResponseEntity<Object> response = controller.createPublicHealthAuthority(pha);
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertNotNull(response.getBody());
    String body = response.getBody().toString();
    assertTrue(body.contains("\"status\":\"error\""));
    assertTrue(body.contains("already registered"));
    verify(service, never()).saveOrUpdate(any());
  }
}
