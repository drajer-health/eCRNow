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
public class LoadingQueryServiceTest {

  @InjectMocks private LoadingQueryService loadingQueryService;

  //  @Mock private LoadingQueryDstu2Bundle generateDSTU2Bundle;

  @Mock private LoadingQueryR4Bundle generateR4Bundle;

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

    when(generateR4Bundle.createR4Bundle(any(), any(), any(), any())).thenReturn(mockBundle);

    FhirData result = loadingQueryService.getData(launchDetails, startDate, endDate);

    assertNotNull("Result should not be null", result);
    assertTrue(result instanceof R4FhirData);
    R4FhirData r4Data = (R4FhirData) result;
    assertNotNull("Bundle should not be null", r4Data.getData());
    assertEquals("Bundle should contain 1 entry", 1, r4Data.getData().getEntry().size());
    assertSame("Returned bundle should match mock", mockBundle, r4Data.getData());

    verify(generateR4Bundle, times(1)).createR4Bundle(any(), any(), any(), any());
  }

  @Test
  public void testGetData_R4_Exception() throws Exception {

    launchDetails.setFhirVersion(FhirVersionEnum.R4.toString());

    when(generateR4Bundle.createR4Bundle(any(), any(), any(), any()))
        .thenThrow(new RuntimeException("Test exception"));

    FhirData result = loadingQueryService.getData(launchDetails, startDate, endDate);

    assertNotNull("Result should not be null", result);
    assertTrue(result instanceof R4FhirData);
    assertNull("Bundle should be null on exception", ((R4FhirData) result).getData());

    verify(generateR4Bundle, times(1)).createR4Bundle(any(), any(), any(), any());
  }
}
