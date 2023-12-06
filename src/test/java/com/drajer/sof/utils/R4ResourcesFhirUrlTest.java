package com.drajer.sof.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.Date;
import org.hl7.fhir.r4.model.Encounter;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest(FhirContextInitializer.class)
public class R4ResourcesFhirUrlTest {

  private static final Object URL_WITHOUT_LOINC_TRAVEL_CODE =
      "https://fhir-ehr-code.cerner.com/r4/ec2458f2-1e24-41c8-b71b-0e701af7583d/Observation?patient=12345&code=http://snomed.info/sct|161085007,http://snomed.info/sct|443846001,http://snomed.info/sct|420008001,http://snomed.info/sct|46521000175102,http://snomed.info/sct|34831000175105,http://snomed.info/sct|161086008";
  @InjectMocks R4ResourcesData r4ResourcesData;
  @Mock FhirContext context;

  @Mock IGenericClient client;

  @Mock LaunchDetails launchDetails;

  @Mock R4FhirData r4FhirData;

  @Mock Encounter encounter;

  @Captor ArgumentCaptor<String> urlCaptor;
  @Captor ArgumentCaptor<String> observationCaptor;
  @Captor ArgumentCaptor<FhirContext> contextCaptor;
  @Captor ArgumentCaptor<IGenericClient> clientCaptor;
  @Captor ArgumentCaptor<LaunchDetails> launchDetailsCaptor;

  @Before
  public void init() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getTravelObservationDataTest() {

    initializeMocks();
    r4ResourcesData.getTravelObservationData(
        context, client, launchDetails, r4FhirData, encounter, new Date(), new Date());
    PowerMockito.verifyStatic(FhirContextInitializer.class);
    // capturing url
    FhirContextInitializer.getResourceBundleByUrl(
        launchDetailsCaptor.capture(),
        clientCaptor.capture(),
        contextCaptor.capture(),
        observationCaptor.capture(),
        urlCaptor.capture());

    assertEquals(URL_WITHOUT_LOINC_TRAVEL_CODE, urlCaptor.getValue());
  }

  private void initializeMocks() {
    PowerMockito.mockStatic(FhirContextInitializer.class);
    ReflectionTestUtils.setField(r4ResourcesData, "excludeLoincTravelCode", Boolean.TRUE);
    Mockito.when(launchDetails.getEhrServerURL())
        .thenReturn("https://fhir-ehr-code.cerner.com/r4/ec2458f2-1e24-41c8-b71b-0e701af7583d");
    Mockito.when(launchDetails.getLaunchPatientId()).thenReturn("12345");
    Mockito.when(
            FhirContextInitializer.getResourceBundleByUrl(
                Mockito.any(LaunchDetails.class),
                Mockito.any(IGenericClient.class),
                Mockito.any(FhirContext.class),
                Mockito.anyString(),
                Mockito.anyString()))
        .thenReturn(null);
  }
}
