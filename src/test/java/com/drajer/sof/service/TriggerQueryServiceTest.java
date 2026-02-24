package com.drajer.sof.service;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirVersionEnum;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.Date;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class TriggerQueryServiceTest {

  @InjectMocks private TriggerQueryService triggerQueryService;

  //  @Mock private TriggerQueryDstu2Bundle generateDstu2Bundles;

  @Mock private TriggerQueryR4Bundle generateR4Bundles;

  private LaunchDetails launchDetails;
  private Date startDate;
  private Date endDate;

  @Before
  public void setUp() {
    launchDetails = new LaunchDetails();
    startDate = new Date();
    endDate = new Date();
  }

  @Test
  public void testGetData_R4_Success() throws Exception {
    launchDetails.setFhirVersion(FhirVersionEnum.R4.toString());

    org.hl7.fhir.r4.model.Bundle mockBundle = new org.hl7.fhir.r4.model.Bundle();
    mockBundle.addEntry(new BundleEntryComponent());

    when(generateR4Bundles.createR4Bundle(any(), any(), any(), any())).thenReturn(mockBundle);

    FhirData result = triggerQueryService.getData(launchDetails, startDate, endDate);

    assertTrue(result instanceof R4FhirData);
    assertEquals(1, ((R4FhirData) result).getData().getEntry().size());
    assertSame(
        "Returned bundle should be the same mock instance",
        mockBundle,
        ((R4FhirData) result).getData());
    assertNotNull(((R4FhirData) result).getData().getEntry().get(0));
    verify(generateR4Bundles, times(1)).createR4Bundle(any(), any(), any(), any());
  }

  @Test
  public void testGetData_R4_Exception() throws Exception {
    launchDetails.setFhirVersion(FhirVersionEnum.R4.toString());

    when(generateR4Bundles.createR4Bundle(any(), any(), any(), any()))
        .thenThrow(new RuntimeException("Test exception"));

    FhirData result = triggerQueryService.getData(launchDetails, startDate, endDate);

    assertTrue(result instanceof R4FhirData);
    assertNull("R4 bundle should be null if exception is thrown", ((R4FhirData) result).getData());
    verify(generateR4Bundles, times(1)).createR4Bundle(any(), any(), any(), any());
  }
}
