package com.drajer.ecrapp.fhir.utils.ecrretry;

import static org.mockito.Mockito.*;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.IReadExecutable;
import ca.uhn.fhir.rest.gclient.IReadTyped;
import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplate;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplateConfig;
import com.drajer.ecrapp.fhir.utils.RetryableException;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class EcrFhirRetryableReadTest {

  private LaunchDetails currentStateDetails;
  private ClientDetails clientDetails;

  @InjectMocks FHIRRetryTemplate fhirretryTemplate;

  @InjectMocks FHIRRetryTemplateConfig fhirRetryTemplateConfig;
  @InjectMocks FHIRRetryTemplateConfig.HttpMethodType httpMethodType;
  @InjectMocks SpringConfiguration springConfiguration;
  @Mock FhirContextInitializer fhirContextInitializer;

  @Before
  public void init() {
    MockitoAnnotations.initMocks(this);
    currentStateDetails =
        (LaunchDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/LaunchDetails/LaunchDetails.json", LaunchDetails.class);
    currentStateDetails.setLastUpdated(new Date());
    clientDetails =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDetail_IT_FullECR.json", ClientDetails.class);
  }

  @Test
  public void testRetryRead() {
    FhirContext context = mock(FhirContext.class);
    EcrFhirRetryClient retryClient = mock(EcrFhirRetryClient.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock((EcrFhirRetryableRead.class));
    IReadExecutable readExecutable = mock((IReadExecutable.class));
    Map<String, FHIRRetryTemplateConfig.HttpMethodType> map = new HashMap<>();
    currentStateDetails.setFhirVersion("R4");

    httpMethodType.setMaxRetries(3);
    httpMethodType.setRetryWaitTime(3000);
    httpMethodType.setRetryStatusCodes(
        new ArrayList<>(Arrays.asList(408, 429, 502, 503, 504, 500)));

    map.put("GET", httpMethodType);
    fhirRetryTemplateConfig.setMaxRetries(3);
    fhirRetryTemplateConfig.setRetryWaitTime(3000);
    fhirRetryTemplateConfig.setRetryStatusCodes(
        new ArrayList<>(Arrays.asList(408, 429, 502, 503, 504, 500)));
    fhirRetryTemplateConfig.setHttpMethodTypeMap(map);

    springConfiguration.setFhirRetryTemplateConfig(fhirRetryTemplateConfig);
    fhirretryTemplate.setRetryTemplate(springConfiguration.retryTemplate());
    when(retryClient.getRetryTemplate()).thenReturn(fhirretryTemplate);

    when(fhirContextInitializer.getFhirContext(currentStateDetails.getFhirVersion()))
        .thenReturn(context);

    when(fhirContextInitializer.createClient(context, currentStateDetails)).thenReturn(retryClient);

    when(retryClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId(currentStateDetails.getEncounterId())).thenReturn(readExecutable);
    when(readExecutable.execute())
        .thenThrow(new RetryableException("INTERNAL_SERVER_ERROR", 500, "GET"));
    try {
      retryClient
          .getRetryTemplate()
          .execute(
              retryContext -> {
                return readExecutable.execute();
              },
              null);
    } catch (Exception e) {
      verify(readExecutable, times(3)).execute();
    }
  }
}
