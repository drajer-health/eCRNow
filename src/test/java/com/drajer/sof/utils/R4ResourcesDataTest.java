package com.drajer.sof.utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.IReadExecutable;
import ca.uhn.fhir.rest.gclient.IReadTyped;
import ca.uhn.fhir.rest.server.exceptions.InternalErrorException;
import ca.uhn.fhir.rest.server.exceptions.ResourceNotFoundException;
import com.drajer.ecrapp.fhir.utils.RetryableException;
import com.drajer.ecrapp.fhir.utils.ecrretry.EcrFhirRetryClient;
import com.drajer.ecrapp.fhir.utils.ecrretry.EcrFhirRetryableRead;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.Date;
import org.apache.commons.lang3.time.DateUtils;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DocumentReference;
import org.hl7.fhir.r4.model.Encounter;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest(WorkflowService.class)
public class R4ResourcesDataTest {

  @Mock LaunchDetails launchDetails;
  @Mock R4FhirData r4FhirData;

  @Before
  public void setUp() {}

  @Test
  public void constructR4DocumentReferenceTest() {
    R4ResourcesData dataObject = new R4ResourcesData();
    String rrXml = TestUtils.getFileContentAsString("R4/Misc/TestRRXml.xml");
    DocumentReference result =
        dataObject.constructR4DocumentReference(
            rrXml, "P123456", "E98765", "1225652472001060", "text/xml");
    assertNotNull(result);
  }

  @Test
  public void testIsConditionActive() {

    R4ResourcesData dataObject = new R4ResourcesData();
    Condition cond1 = new Condition();

    DateTimeType d = new DateTimeType(new Date((System.currentTimeMillis() - 10000)));
    cond1.setAbatement(d);

    assertFalse(dataObject.isConditionActive(cond1));

    cond1.setAbatement(new DateTimeType(new Date((System.currentTimeMillis() + 10000))));

    assertTrue(dataObject.isConditionActive(cond1));
  }

  @Test
  public void testGetEncounterDataWith_404Exception() {
    String encounterId = "123456";
    FhirContext context = mock(FhirContext.class);
    PowerMockito.mockStatic(WorkflowService.class);
    R4ResourcesData resourcesData = new R4ResourcesData();
    resourcesData.resourceData = mock(FhirContextInitializer.class);
    EcrFhirRetryClient retryClient = mock(EcrFhirRetryClient.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock((EcrFhirRetryableRead.class));
    IReadExecutable readExecutable = mock((IReadExecutable.class));

    Date startDate = DateUtils.addDays(new Date(), -30);
    Date endDate = DateUtils.addDays(new Date(), -5);

    when(launchDetails.getEncounterId()).thenReturn(encounterId);
    when(resourcesData.resourceData.checkSkipResource(any(), any())).thenReturn(false);
    when(retryClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId(encounterId)).thenReturn(readExecutable);
    when(readExecutable.execute()).thenThrow(new ResourceNotFoundException("NOT_FOUND"));

    Encounter encounter =
        resourcesData.getEncounterData(
            context, retryClient, launchDetails, r4FhirData, startDate, endDate);
    PowerMockito.verifyStatic(WorkflowService.class, times(1));
    WorkflowService.cancelAllScheduledTasksForLaunch(eq(launchDetails), eq(true));
  }

  @Test
  public void testGetEncounterDataWith_retryableException() {
    String encounterId = "123456";
    FhirContext context = mock(FhirContext.class);
    PowerMockito.mockStatic(WorkflowService.class);
    R4ResourcesData resourcesData = new R4ResourcesData();
    resourcesData.resourceData = mock(FhirContextInitializer.class);
    EcrFhirRetryClient retryClient = mock(EcrFhirRetryClient.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock((EcrFhirRetryableRead.class));
    IReadExecutable readExecutable = mock((IReadExecutable.class));

    Date startDate = DateUtils.addDays(new Date(), -30);
    Date endDate = DateUtils.addDays(new Date(), -5);

    when(launchDetails.getEncounterId()).thenReturn(encounterId);
    when(resourcesData.resourceData.checkSkipResource(any(), any())).thenReturn(false);
    when(retryClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId(encounterId)).thenReturn(readExecutable);
    when(readExecutable.execute())
        .thenThrow(
            new RetryableException(
                new InternalErrorException("INTERNAL_SERVER_ERROR"), 500, "GET"));
    Encounter encounter =
        resourcesData.getEncounterData(
            context, retryClient, launchDetails, r4FhirData, startDate, endDate);
    PowerMockito.verifyStatic(WorkflowService.class, times(0));
    WorkflowService.cancelAllScheduledTasksForLaunch(eq(launchDetails), eq(true));
  }

  @Test
  public void testGetEncounterDataWith_404inRetryableException() {
    String encounterId = "123456";
    FhirContext context = mock(FhirContext.class);
    PowerMockito.mockStatic(WorkflowService.class);
    R4ResourcesData resourcesData = new R4ResourcesData();
    resourcesData.resourceData = mock(FhirContextInitializer.class);
    EcrFhirRetryClient retryClient = mock(EcrFhirRetryClient.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock((EcrFhirRetryableRead.class));
    IReadExecutable readExecutable = mock((IReadExecutable.class));

    Date startDate = DateUtils.addDays(new Date(), -30);
    Date endDate = DateUtils.addDays(new Date(), -5);

    when(launchDetails.getEncounterId()).thenReturn(encounterId);
    when(resourcesData.resourceData.checkSkipResource(any(), any())).thenReturn(false);
    when(retryClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId(encounterId)).thenReturn(readExecutable);
    when(readExecutable.execute())
        .thenThrow(new RetryableException(new ResourceNotFoundException("NOT_FOUND"), 404, "GET"));
    Encounter encounter =
        resourcesData.getEncounterData(
            context, retryClient, launchDetails, r4FhirData, startDate, endDate);
    PowerMockito.verifyStatic(WorkflowService.class, times(1));
    WorkflowService.cancelAllScheduledTasksForLaunch(eq(launchDetails), eq(true));
  }

  @Test
  public void testGetEncounterDataWith_InternalServerException() {
    String encounterId = "123456";
    FhirContext context = mock(FhirContext.class);
    PowerMockito.mockStatic(WorkflowService.class);
    R4ResourcesData resourcesData = new R4ResourcesData();
    resourcesData.resourceData = mock(FhirContextInitializer.class);
    EcrFhirRetryClient retryClient = mock(EcrFhirRetryClient.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock((EcrFhirRetryableRead.class));
    IReadExecutable readExecutable = mock((IReadExecutable.class));

    Date startDate = DateUtils.addDays(new Date(), -30);
    Date endDate = DateUtils.addDays(new Date(), -5);

    when(launchDetails.getEncounterId()).thenReturn(encounterId);
    when(resourcesData.resourceData.checkSkipResource(any(), any())).thenReturn(false);
    when(retryClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId(encounterId)).thenReturn(readExecutable);
    when(readExecutable.execute()).thenThrow(new InternalErrorException("INTERNAL_SERVER_ERROR"));
    Encounter encounter =
        resourcesData.getEncounterData(
            context, retryClient, launchDetails, r4FhirData, startDate, endDate);
    PowerMockito.verifyStatic(WorkflowService.class, times(0));
    WorkflowService.cancelAllScheduledTasksForLaunch(eq(launchDetails), eq(true));
  }
}
