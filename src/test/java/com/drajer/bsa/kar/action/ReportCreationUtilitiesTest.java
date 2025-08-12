package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.powermock.api.mockito.PowerMockito.when;

import com.drajer.bsa.model.KarProcessingData;
import java.util.*;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;

@RunWith(PowerMockRunner.class)
@PrepareForTest({ReportCreationUtilities.class})
public class ReportCreationUtilitiesTest {

  private Logger mockLogger;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    PowerMockito.mockStatic(ReportCreationUtilities.class);
    MockitoAnnotations.initMocks(this);
    mockLogger = mock(Logger.class);
  }

  @Test
  public void testGetPractitioners_returnsExpectedList() {
    Practitioner practitioner1 = new Practitioner();
    practitioner1.setId("pract-1");
    Practitioner practitioner2 = new Practitioner();
    practitioner2.setId("pract-2");
    Set<Resource> practitionerSet = new HashSet<>();
    practitionerSet.add(practitioner1);
    practitionerSet.add(practitioner2);
    KarProcessingData mockData = mock(KarProcessingData.class);
    when(mockData.getResourcesByType(ResourceType.Practitioner)).thenReturn(practitionerSet);
    PowerMockito.when(ReportCreationUtilities.getPractitioners(any(), any())).thenCallRealMethod();

    List<Practitioner> result =
        ReportCreationUtilities.getPractitioners(mockData, V3ParticipationType.ADM);

    assertEquals(2, result.size());
    assertTrue(result.contains(practitioner1));
    assertTrue(result.contains(practitioner2));
  }

  @Test
  public void testGetOrganization_returnsOrganization() {
    KarProcessingData mockData = mock(KarProcessingData.class);
    KarProcessingData Data = new KarProcessingData();
    Organization org = new Organization();
    org.setId("org-1");
    Organization org1 = new Organization();
    org.setId("org-2");
    Set<Resource> orgSet = new HashSet<>();
    orgSet.add(org);
    orgSet.add(org1);

    when(mockData.getResourcesByType(ResourceType.Organization)).thenReturn(orgSet);
    PowerMockito.when(ReportCreationUtilities.getOrganization(any())).thenCallRealMethod();

    Organization result = ReportCreationUtilities.getOrganization(mockData);

    assertNotNull(result);
  }

  @Test
  public void testGetOrganization_returnsNullWhenEmpty() {
    KarProcessingData mockData = mock(KarProcessingData.class);
    when(mockData.getResourcesByType(ResourceType.Organization)).thenReturn(new HashSet<>());
    PowerMockito.when(ReportCreationUtilities.getOrganization(any())).thenCallRealMethod();
    Organization result = ReportCreationUtilities.getOrganization(mockData);

    assertNull(result);
  }
}
