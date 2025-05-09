package com.drajer.bsa.ehr.service.impl;

import static com.helger.commons.mock.CommonsAssert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.sof.utils.FhirContextInitializer;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class EhrFhirR4QueryServiceImplTest {

  @InjectMocks private EhrFhirR4QueryServiceImpl ehrFhirR4QueryService;

  @Mock private AuthorizationUtils authUtils;

  @Mock private HealthcareSettingsDao hsDao;

  @Mock private FhirContextInitializer fhirContextInitializer;

  @Mock private FhirContext fhirContext;

  @Mock private IGenericClient fhirClient;

  @Mock private KarProcessingData karProcessingData;

  @Mock private NotificationContext notificationContext;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(karProcessingData.hasValidAccessToken()).thenReturn(true);
    when(karProcessingData.getAccessToken()).thenReturn("mockAccessToken");

    // Mock FHIR Context Initialization
    Mockito.lenient().when(fhirContextInitializer.getFhirContext("R4")).thenReturn(fhirContext);
    Mockito.lenient().when(karProcessingData.hasValidAccessToken()).thenReturn(true);
    //
    // Mockito.lenient().doReturn(fhirClient).when(ehrFhirR4QueryService).getClient(any(KarProcessingData.class), any(FhirContext.class));
  }

  @Ignore
  @Test
  public void testGetFilteredData_PatientExists() {
    Mockito.lenient().when(fhirContextInitializer.getFhirContext("R4")).thenReturn(fhirContext);
    Mockito.lenient()
        .when(karProcessingData.getNotificationContext())
        .thenReturn(notificationContext);
    Mockito.lenient().when(notificationContext.getPatientId()).thenReturn("123");
    // Mockito.lenient(). when(ehrFhirR4QueryService.getClient(any(),
    // any())).thenReturn(fhirClient);

    Patient mockPatient = new Patient();
    mockPatient.setId("123");
    Mockito.lenient()
        .when(ehrFhirR4QueryService.getResourceById(any(), any(), eq("Patient"), eq("123"), true))
        .thenReturn(mockPatient);

    Map<String, ResourceType> resTypes = new HashMap<>();
    resTypes.put("Condition", ResourceType.Condition);

    Map<ResourceType, Set<Resource>> result =
        ehrFhirR4QueryService.getFilteredData(karProcessingData, resTypes);

    assertNotNull(result);
    assertTrue(result.containsKey(ResourceType.Patient));
    assertEquals(1, result.get(ResourceType.Patient).size());
    verify(ehrFhirR4QueryService).getResourceById(any(), any(), eq("Patient"), eq("123"), true);
  }
}
