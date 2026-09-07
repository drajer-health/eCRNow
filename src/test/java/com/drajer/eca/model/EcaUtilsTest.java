package com.drajer.eca.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.IReadExecutable;
import ca.uhn.fhir.rest.gclient.IReadTyped;
import ca.uhn.fhir.rest.server.exceptions.ResourceNotFoundException;
import com.drajer.cdafromr4.CdaEicrGeneratorFromR4;
import com.drajer.ecrapp.config.AppConfig;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.fhir.utils.ecrretry.EcrFhirRetryableRead;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.ecrapp.service.impl.EicrServiceImpl;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.apache.commons.lang3.time.DateUtils;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Period;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({
  ValueSetSingleton.class,
  ApplicationUtils.class,
  ActionRepo.class,
  CdaEicrGeneratorFromR4.class,
  WorkflowService.class
})
@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
public class EcaUtilsTest {

  private LaunchDetails mockDetails;
  private PatientExecutionState mockState;
  private Dstu2FhirData mockDstu2Data;
  private R4FhirData mockR4Data;
  private ValueSetSingleton mockValueSet;
  private ActionRepo mockActionRepo;
  private LoadingQueryService mockQuerySrvc;
  private EicrServiceImpl mockRRSrvc;

  private Encounter mockr4Encounter;

  private FhirContextInitializer mockFhirContextIntializer;

  private List<ActionData> codePaths;
  private List<CodeableConceptDt> ptCodes;
  private List<CodeableConcept> ptCodes1;
  private Set<String> codesToMatch;
  private Set<String> codesToMatchAgainst;
  private MatchTriggerStatus matchTriggerStatus;
  private AppConfig mockAppConfig;

  @SuppressWarnings("unused")
  private RelatedAction mockRelatedAction;

  @Before
  public void setUp() {

    // Mock required classes
    mockDetails = PowerMockito.mock(LaunchDetails.class);
    mockState = PowerMockito.mock(PatientExecutionState.class);
    mockDstu2Data = PowerMockito.mock(Dstu2FhirData.class);
    mockR4Data = PowerMockito.mock(R4FhirData.class);
    mockQuerySrvc = PowerMockito.mock(LoadingQueryService.class);
    mockRRSrvc = PowerMockito.mock(EicrServiceImpl.class);
    mockAppConfig = PowerMockito.mock(AppConfig.class);
    mockr4Encounter = PowerMockito.mock(Encounter.class);
    mockFhirContextIntializer = PowerMockito.mock(FhirContextInitializer.class);
    mockRelatedAction = PowerMockito.mock(RelatedAction.class);

    if (mockValueSet == null) {

      mockValueSet = PowerMockito.mock(ValueSetSingleton.class);
    }

    if (mockActionRepo == null) {

      mockActionRepo = PowerMockito.mock(ActionRepo.class);
      ;
    }

    PowerMockito.mockStatic(ValueSetSingleton.class);
    PowerMockito.mockStatic(ActionRepo.class);
    PowerMockito.mockStatic(ApplicationUtils.class);
  }

  @Test
  public void testMatchTriggerCodesForDSTU2_CovidTrue() {

    // Setup
    setupMockForMatchTrigger();
    when(mockDstu2Data.getCodesForExpression("mock test path")).thenReturn(ptCodes);
    when(ApplicationUtils.convertCodeableConceptsToString(ptCodes)).thenReturn(codesToMatch);
    when(mockValueSet.getEmergentValueSetsAsStringForGrouper(anyString()))
        .thenReturn(codesToMatchAgainst);
    when(mockDetails.getIsCovid()).thenReturn(true);

    // Test
    Boolean result =
        EcaUtils.matchTriggerCodesForDSTU2(codePaths, mockDstu2Data, mockState, mockDetails);

    // Validate
    assertEquals(true, matchTriggerStatus.getTriggerMatchStatus());
    assertEquals(true, result);
  }

  @Test
  public void testMatchTriggerCodesForDSTU2_CovidFalse() {

    // Setup
    setupMockForMatchTrigger();
    when(mockDstu2Data.getCodesForExpression("mock test path")).thenReturn(ptCodes);
    when(ApplicationUtils.convertCodeableConceptsToString(ptCodes)).thenReturn(codesToMatch);
    when(mockValueSet.getValueSetsAsStringForGrouper(anyString())).thenReturn(codesToMatchAgainst);
    when(mockDetails.getIsCovid()).thenReturn(false);

    // Test
    Boolean result =
        EcaUtils.matchTriggerCodesForDSTU2(codePaths, mockDstu2Data, mockState, mockDetails);

    // Validate
    assertEquals(true, matchTriggerStatus.getTriggerMatchStatus());
    assertEquals(true, result);
  }

  @Test
  public void testMatchTriggerCodesForR4_CovidTrue() {

    // Setup
    setupMockForMatchTrigger();
    when(mockR4Data.getR4CodesForExpression("mock test path")).thenReturn(ptCodes1);
    when(ApplicationUtils.convertR4CodeableConceptsToString(ptCodes1)).thenReturn(codesToMatch);
    when(mockValueSet.getEmergentValueSetsAsStringForGrouper(anyString()))
        .thenReturn(codesToMatchAgainst);
    when(mockDetails.getIsCovid()).thenReturn(true);

    // Test
    Boolean result = EcaUtils.matchTriggerCodesForR4(codePaths, mockR4Data, mockState, mockDetails);

    // Validate
    assertEquals(true, matchTriggerStatus.getTriggerMatchStatus());
    assertEquals(true, result);
  }

  @Test
  public void testMatchTriggerCodesForR4_CovidFalse() {

    // Setup
    setupMockForMatchTrigger();
    when(mockR4Data.getR4CodesForExpression("mock test path")).thenReturn(ptCodes1);
    when(ApplicationUtils.convertR4CodeableConceptsToString(ptCodes1)).thenReturn(codesToMatch);
    when(mockValueSet.getValueSetsAsStringForGrouper(anyString())).thenReturn(codesToMatchAgainst);
    when(mockDetails.getIsCovid()).thenReturn(false);

    // Test
    Boolean result = EcaUtils.matchTriggerCodesForR4(codePaths, mockR4Data, mockState, mockDetails);

    // Validate
    assertEquals(true, matchTriggerStatus.getTriggerMatchStatus());
    assertEquals(true, result);
  }

  @Test
  public void testCreateEicr_R4() {

    // SetUp
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(mockActionRepo.getEicrRRService()).thenReturn(mockRRSrvc);
    when(mockActionRepo.getLoadingQueryService()).thenReturn(mockQuerySrvc);
    when(mockQuerySrvc.getData(eq(mockDetails), eq(null), eq(null))).thenReturn(mockR4Data);

    PowerMockito.mockStatic(CdaEicrGeneratorFromR4.class);
    when(CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            any(R4FhirData.class),
            eq(mockDetails),
            any(Eicr.class),
            any(Integer.class),
            any(String.class)))
        .thenReturn("This is R4 EICR data");

    // Test
    Eicr eicr = EcaUtils.createEicr(mockDetails);

    // Validate
    assertEquals("This is R4 EICR data", eicr.getEicrData());
  }

  @Test
  public void testHasNewTriggerCodeMatches() {

    // Compare Old codes with New codes and Expects True
    PatientExecutionState oldState =
        (PatientExecutionState)
            TestUtils.getResourceAsObject(
                "R4/Misc/EcaUtils/OldState.json", PatientExecutionState.class);
    PatientExecutionState newState =
        (PatientExecutionState)
            TestUtils.getResourceAsObject(
                "R4/Misc/EcaUtils/NewState.json", PatientExecutionState.class);
    boolean hasNewMatchCodes = EcaUtils.hasNewTriggerCodeMatches(oldState, newState);
    assertTrue(hasNewMatchCodes);

    // No Old codes available
    oldState.getMatchTriggerStatus().setTriggerMatchStatus(false);
    boolean noOldCodes = EcaUtils.hasNewTriggerCodeMatches(oldState, newState);
    assertTrue(noOldCodes);

    // Both Old and New codes are not available
    oldState.getMatchTriggerStatus().setTriggerMatchStatus(false);
    newState.getMatchTriggerStatus().setTriggerMatchStatus(false);
    boolean noOldAndNewCodes = EcaUtils.hasNewTriggerCodeMatches(oldState, newState);
    assertFalse(noOldAndNewCodes);

    PatientExecutionState oState =
        (PatientExecutionState)
            TestUtils.getResourceAsObject(
                "R4/Misc/EcaUtils/OldState.json", PatientExecutionState.class);

    PatientExecutionState nState =
        (PatientExecutionState)
            TestUtils.getResourceAsObject(
                "R4/Misc/EcaUtils/NewStateWithDifferentPath.json", PatientExecutionState.class);
    boolean matchedCodes = EcaUtils.hasNewTriggerCodeMatches(oState, nState);
    assertTrue(matchedCodes);

    PatientExecutionState olState =
        (PatientExecutionState)
            TestUtils.getResourceAsObject(
                "R4/Misc/EcaUtils/OldStateWithDifferentPath.json", PatientExecutionState.class);

    boolean matchCodes = EcaUtils.hasNewTriggerCodeMatches(olState, nState);
    assertFalse(matchCodes);
  }

  @Test
  public void testLongRunningEncounter() {

    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getAppConfig()).thenReturn(mockAppConfig);
    when(mockAppConfig.getSuspendThreshold()).thenReturn(45);
    when(mockAppConfig.isEnableSuspend()).thenReturn(true);
    Date startDate = DateUtils.addDays(new Date(), -90);
    when(mockDetails.getStartDate()).thenReturn(startDate);

    boolean checkLongRunningEncounters = EcaUtils.checkLongRunningEncounters(mockDetails);
    assertTrue(checkLongRunningEncounters);
  }

  @Test
  public void testIfEncounterStartDateIsNull() throws Exception {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getFhirContextInitializer())
        .thenReturn(mockFhirContextIntializer);
    Date launchDetailsStartDate = DateUtils.addDays(new Date(), -20);
    FhirContext mockContext = Mockito.mock(FhirContext.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock(EcrFhirRetryableRead.class);
    IGenericClient mockClient = Mockito.mock(IGenericClient.class);
    IReadExecutable readExecutable = mock((IReadExecutable.class));
    when(mockDetails.getEncounterId()).thenReturn("123");
    when(mockDetails.getFhirVersion()).thenReturn("R4");
    when(mockDetails.getEhrServerURL()).thenReturn("");
    when(mockDetails.getAccessToken()).thenReturn("");
    when(mockDetails.getxRequestId()).thenReturn("");
    when(mockDetails.getStartDate()).thenReturn(launchDetailsStartDate);
    when(mockFhirContextIntializer.getFhirContext(mockDetails.getFhirVersion()))
        .thenReturn(mockContext);
    when(mockFhirContextIntializer.createClient(
            mockContext,
            mockDetails.getEhrServerURL(),
            mockDetails.getAccessToken(),
            mockDetails.getxRequestId(),
            null))
        .thenReturn(mockClient);
    Period period = new Period();
    period.setStart(null);
    period.setEnd(null);
    when(mockClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId(mockDetails.getEncounterId())).thenReturn(readExecutable);
    when(readExecutable.execute()).thenReturn(mockr4Encounter);
    when(mockClient.read().resource("Encounter").withId(mockDetails.getEncounterId()).execute())
        .thenReturn(mockr4Encounter);
    when(mockr4Encounter.getPeriod()).thenReturn(period);
    assertTrue(mockr4Encounter.getPeriod().getStart() == null);
    assertTrue(mockDetails.getStartDate() != null);
  }

  @Test
  public void testCheckEncounterClose_Dstu2WithEndDate() throws Exception {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getFhirContextInitializer())
        .thenReturn(mockFhirContextIntializer);

    FhirContext mockContext = Mockito.mock(FhirContext.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock(EcrFhirRetryableRead.class);
    IGenericClient mockClient = Mockito.mock(IGenericClient.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);

    when(mockDetails.getEncounterId()).thenReturn("encounter-123");
    when(mockDetails.getFhirVersion()).thenReturn("DSTU2");
    when(mockDetails.getEhrServerURL()).thenReturn("");
    when(mockDetails.getAccessToken()).thenReturn("");
    when(mockDetails.getxRequestId()).thenReturn("");

    when(mockFhirContextIntializer.getFhirContext("DSTU2")).thenReturn(mockContext);
    when(mockFhirContextIntializer.createClient(
            mockContext,
            mockDetails.getEhrServerURL(),
            mockDetails.getAccessToken(),
            mockDetails.getxRequestId(),
            null))
        .thenReturn(mockClient);

    ca.uhn.fhir.model.dstu2.resource.Encounter dstu2Encounter =
        new ca.uhn.fhir.model.dstu2.resource.Encounter();
    ca.uhn.fhir.model.dstu2.composite.PeriodDt period =
        new ca.uhn.fhir.model.dstu2.composite.PeriodDt();
    period.setStart(new ca.uhn.fhir.model.primitive.DateTimeDt(new Date()));
    period.setEnd(new ca.uhn.fhir.model.primitive.DateTimeDt(DateUtils.addDays(new Date(), 5)));
    dstu2Encounter.setPeriod(period);
    dstu2Encounter.setStatus(ca.uhn.fhir.model.dstu2.valueset.EncounterStateEnum.IN_PROGRESS);

    when(mockClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId("encounter-123")).thenReturn(readExecutable);
    when(readExecutable.execute()).thenReturn(dstu2Encounter);

    boolean isClosed = EcaUtils.checkEncounterClose(mockDetails);
    assertTrue(isClosed);
  }

  @Test
  public void testCheckEncounterClose_Dstu2WithNullPeriod() throws Exception {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getFhirContextInitializer())
        .thenReturn(mockFhirContextIntializer);

    FhirContext mockContext = Mockito.mock(FhirContext.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock(EcrFhirRetryableRead.class);
    IGenericClient mockClient = Mockito.mock(IGenericClient.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);

    when(mockDetails.getEncounterId()).thenReturn("encounter-456");
    when(mockDetails.getFhirVersion()).thenReturn("DSTU2");
    when(mockDetails.getEhrServerURL()).thenReturn("");
    when(mockDetails.getAccessToken()).thenReturn("");
    when(mockDetails.getxRequestId()).thenReturn("");

    when(mockFhirContextIntializer.getFhirContext("DSTU2")).thenReturn(mockContext);
    when(mockFhirContextIntializer.createClient(
            mockContext,
            mockDetails.getEhrServerURL(),
            mockDetails.getAccessToken(),
            mockDetails.getxRequestId(),
            null))
        .thenReturn(mockClient);

    ca.uhn.fhir.model.dstu2.resource.Encounter dstu2Encounter =
        new ca.uhn.fhir.model.dstu2.resource.Encounter();
    dstu2Encounter.setPeriod(null);
    dstu2Encounter.setStatus(ca.uhn.fhir.model.dstu2.valueset.EncounterStateEnum.IN_PROGRESS);

    when(mockClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId("encounter-456")).thenReturn(readExecutable);
    when(readExecutable.execute()).thenReturn(dstu2Encounter);

    boolean isClosed = EcaUtils.checkEncounterClose(mockDetails);
    assertFalse(isClosed);
  }

  @Test
  public void testCheckEncounterClose_Dstu2NotClosed() throws Exception {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getFhirContextInitializer())
        .thenReturn(mockFhirContextIntializer);

    FhirContext mockContext = Mockito.mock(FhirContext.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock(EcrFhirRetryableRead.class);
    IGenericClient mockClient = Mockito.mock(IGenericClient.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);

    when(mockDetails.getEncounterId()).thenReturn("encounter-789");
    when(mockDetails.getFhirVersion()).thenReturn("DSTU2");
    when(mockDetails.getEhrServerURL()).thenReturn("");
    when(mockDetails.getAccessToken()).thenReturn("");
    when(mockDetails.getxRequestId()).thenReturn("");

    when(mockFhirContextIntializer.getFhirContext("DSTU2")).thenReturn(mockContext);
    when(mockFhirContextIntializer.createClient(
            mockContext,
            mockDetails.getEhrServerURL(),
            mockDetails.getAccessToken(),
            mockDetails.getxRequestId(),
            null))
        .thenReturn(mockClient);

    ca.uhn.fhir.model.dstu2.resource.Encounter dstu2Encounter =
        new ca.uhn.fhir.model.dstu2.resource.Encounter();
    ca.uhn.fhir.model.dstu2.composite.PeriodDt period =
        new ca.uhn.fhir.model.dstu2.composite.PeriodDt();
    period.setStart(new ca.uhn.fhir.model.primitive.DateTimeDt(new Date()));
    dstu2Encounter.setPeriod(period);
    dstu2Encounter.setStatus(ca.uhn.fhir.model.dstu2.valueset.EncounterStateEnum.IN_PROGRESS);

    when(mockClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId("encounter-789")).thenReturn(readExecutable);
    when(readExecutable.execute()).thenReturn(dstu2Encounter);

    boolean isClosed = EcaUtils.checkEncounterClose(mockDetails);
    assertFalse(isClosed);
  }

  public void setupMockForMatchTrigger() {

    // CommonSetup
    ActionData ad = new ActionData();
    ad.setPath("mock test path");
    codePaths = new ArrayList<>();
    codePaths.add(ad);

    codesToMatch = new HashSet<>();
    codesToMatch.add("Code1");

    codesToMatchAgainst = new HashSet<>();
    codesToMatchAgainst.add("Code1");

    matchTriggerStatus = new MatchTriggerStatus();

    when(ValueSetSingleton.getInstance()).thenReturn(mockValueSet);
    when(mockState.getMatchTriggerStatus()).thenReturn(matchTriggerStatus);
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getRctcOid()).thenReturn("2.16.840.1.113762.1.4.1146.1123");
    when(ActionRepo.getInstance().getRctcVersion()).thenReturn("1");

    // Dstu2  Setup
    ptCodes = new ArrayList<>();
    ptCodes.add(new CodeableConceptDt());

    // R4 Setup
    ptCodes1 = new ArrayList<>();
    ptCodes1.add(new CodeableConcept());
  }

  @Test
  public void testCreateEicr_Success() {
    // Setup
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(mockActionRepo.getLoadingQueryService()).thenReturn(mockQuerySrvc);
    when(mockActionRepo.getEicrRRService()).thenReturn(mockRRSrvc);

    R4FhirData mockR4Data = mock(R4FhirData.class);
    when(mockQuerySrvc.getData(eq(mockDetails), any(), any())).thenReturn(mockR4Data);

    PowerMockito.mockStatic(CdaEicrGeneratorFromR4.class);
    when(CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            any(R4FhirData.class),
            eq(mockDetails),
            any(Eicr.class),
            any(Integer.class),
            anyString()))
        .thenReturn("EICR_DATA");

    when(mockDetails.getStartDate()).thenReturn(new Date());
    when(mockDetails.getEndDate()).thenReturn(new Date());
    when(mockDetails.getId()).thenReturn(1);
    when(mockDetails.getProviderUUID()).thenReturn("provider-123");

    // Test & Assert
    Eicr result = EcaUtils.createEicr(mockDetails);
    assertEquals("EICR_DATA", result.getEicrData());
    assertEquals((Integer) 1, result.getLaunchDetailsId());
    assertEquals("provider-123", result.getProviderUUID());
  }

  @Test
  public void testCreateEicr_NoLoadingService() {
    // Setup
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(mockActionRepo.getLoadingQueryService()).thenReturn(null);

    // Test & Assert - should throw exception
    try {
      EcaUtils.createEicr(mockDetails);
      fail("Should throw IllegalStateException");
    } catch (IllegalStateException e) {
      assertTrue(e.getMessage().contains("Spring Injection"));
    }
  }

  @Test
  public void testCreateEicr_NoFhirData() {
    // Setup
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(mockActionRepo.getLoadingQueryService()).thenReturn(mockQuerySrvc);
    when(mockQuerySrvc.getData(eq(mockDetails), any(), any())).thenReturn(null);

    // Test & Assert - should throw exception
    try {
      EcaUtils.createEicr(mockDetails);
      fail("Should throw IllegalStateException");
    } catch (IllegalStateException e) {
      assertTrue(e.getMessage().contains("No Fhir Data"));
    }
  }

  @Test
  public void testUpdateDetailStatus() {
    PatientExecutionState state = new PatientExecutionState();
    EcaUtils.updateDetailStatus(mockDetails, state);
    verify(mockDetails).setStatus(anyString());
  }

  @Test
  public void testCreateEicr_NullEicrGenerated() {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(mockActionRepo.getLoadingQueryService()).thenReturn(mockQuerySrvc);

    R4FhirData mockR4Data = mock(R4FhirData.class);
    when(mockQuerySrvc.getData(eq(mockDetails), any(), any())).thenReturn(mockR4Data);

    PowerMockito.mockStatic(CdaEicrGeneratorFromR4.class);
    when(CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            any(R4FhirData.class),
            eq(mockDetails),
            any(Eicr.class),
            any(Integer.class),
            anyString()))
        .thenReturn(null);

    try {
      EcaUtils.createEicr(mockDetails);
      fail("Should throw IllegalStateException");
    } catch (IllegalStateException e) {
      assertTrue(e.getMessage().contains("No Fhir Data"));
    }
  }

  @Test
  public void testCreateEicr_EmptyEicrGenerated() {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(mockActionRepo.getLoadingQueryService()).thenReturn(mockQuerySrvc);

    R4FhirData mockR4Data = mock(R4FhirData.class);
    when(mockQuerySrvc.getData(eq(mockDetails), any(), any())).thenReturn(mockR4Data);

    PowerMockito.mockStatic(CdaEicrGeneratorFromR4.class);
    when(CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            any(R4FhirData.class),
            eq(mockDetails),
            any(Eicr.class),
            any(Integer.class),
            anyString()))
        .thenReturn("");

    try {
      EcaUtils.createEicr(mockDetails);
      fail("Should throw IllegalStateException");
    } catch (IllegalStateException e) {
      assertTrue(e.getMessage().contains("No Fhir Data"));
    }
  }

  @Test
  public void testCheckEncounterClose_ResourceNotFound() throws Exception {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getFhirContextInitializer())
        .thenReturn(mockFhirContextIntializer);

    FhirContext mockContext = Mockito.mock(FhirContext.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock(EcrFhirRetryableRead.class);
    IGenericClient mockClient = Mockito.mock(IGenericClient.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);

    when(mockDetails.getEncounterId()).thenReturn("invalid-id");
    when(mockDetails.getFhirVersion()).thenReturn("R4");
    when(mockDetails.getEhrServerURL()).thenReturn("");
    when(mockDetails.getAccessToken()).thenReturn("");
    when(mockDetails.getxRequestId()).thenReturn("");

    when(mockFhirContextIntializer.getFhirContext("R4")).thenReturn(mockContext);
    when(mockFhirContextIntializer.createClient(mockContext, "", "", "", null))
        .thenReturn(mockClient);

    when(mockClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId("invalid-id")).thenReturn(readExecutable);
    when(readExecutable.execute()).thenThrow(new ResourceNotFoundException("Not found"));

    PowerMockito.mockStatic(WorkflowService.class);

    boolean result = EcaUtils.checkEncounterClose(mockDetails);

    assertFalse(result);
    PowerMockito.verifyStatic(WorkflowService.class);
    WorkflowService.cancelAllScheduledTasksForLaunch(mockDetails, true);
  }

  @Test
  public void testCheckEncounterClose_R4WithStartDateOnly() throws Exception {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getFhirContextInitializer())
        .thenReturn(mockFhirContextIntializer);

    FhirContext mockContext = Mockito.mock(FhirContext.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock(EcrFhirRetryableRead.class);
    IGenericClient mockClient = Mockito.mock(IGenericClient.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);

    when(mockDetails.getEncounterId()).thenReturn("r4-123");
    when(mockDetails.getFhirVersion()).thenReturn("R4");
    when(mockDetails.getEhrServerURL()).thenReturn("");
    when(mockDetails.getAccessToken()).thenReturn("");
    when(mockDetails.getxRequestId()).thenReturn("");

    when(mockFhirContextIntializer.getFhirContext("R4")).thenReturn(mockContext);
    when(mockFhirContextIntializer.createClient(mockContext, "", "", "", null))
        .thenReturn(mockClient);

    Encounter r4Encounter = new Encounter();
    Period period = new Period();
    Date startDate = new Date();
    period.setStart(startDate);
    period.setEnd(null);
    r4Encounter.setPeriod(period);
    r4Encounter.setStatus(Encounter.EncounterStatus.INPROGRESS);

    when(mockClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId("r4-123")).thenReturn(readExecutable);
    when(readExecutable.execute()).thenReturn(r4Encounter);

    boolean result = EcaUtils.checkEncounterClose(mockDetails);

    assertFalse(result);
    verify(mockDetails).setStartDate(startDate);
  }

  @Test
  public void testCheckEncounterClose_R4WithEndDate() throws Exception {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getFhirContextInitializer())
        .thenReturn(mockFhirContextIntializer);

    FhirContext mockContext = Mockito.mock(FhirContext.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock(EcrFhirRetryableRead.class);
    IGenericClient mockClient = Mockito.mock(IGenericClient.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);

    when(mockDetails.getEncounterId()).thenReturn("r4-456");
    when(mockDetails.getFhirVersion()).thenReturn("R4");
    when(mockDetails.getEhrServerURL()).thenReturn("");
    when(mockDetails.getAccessToken()).thenReturn("");
    when(mockDetails.getxRequestId()).thenReturn("");

    when(mockFhirContextIntializer.getFhirContext("R4")).thenReturn(mockContext);
    when(mockFhirContextIntializer.createClient(mockContext, "", "", "", null))
        .thenReturn(mockClient);

    Encounter r4Encounter = new Encounter();
    Period period = new Period();
    Date startDate = new Date();
    Date endDate = DateUtils.addDays(startDate, 3);
    period.setStart(startDate);
    period.setEnd(endDate);
    r4Encounter.setPeriod(period);
    r4Encounter.setStatus(Encounter.EncounterStatus.INPROGRESS);

    when(mockClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId("r4-456")).thenReturn(readExecutable);
    when(readExecutable.execute()).thenReturn(r4Encounter);

    boolean result = EcaUtils.checkEncounterClose(mockDetails);

    assertTrue(result);
    verify(mockDetails).setStartDate(startDate);
    verify(mockDetails).setEndDate(endDate);
  }

  @Test
  public void testCheckEncounterClose_R4WithClosedStatus() throws Exception {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getFhirContextInitializer())
        .thenReturn(mockFhirContextIntializer);

    FhirContext mockContext = Mockito.mock(FhirContext.class);
    IRead read = mock(EcrFhirRetryableRead.class);
    IReadTyped<IBaseResource> readType = mock(EcrFhirRetryableRead.class);
    IGenericClient mockClient = Mockito.mock(IGenericClient.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);

    when(mockDetails.getEncounterId()).thenReturn("r4-789");
    when(mockDetails.getFhirVersion()).thenReturn("R4");
    when(mockDetails.getEhrServerURL()).thenReturn("");
    when(mockDetails.getAccessToken()).thenReturn("");
    when(mockDetails.getxRequestId()).thenReturn("");

    when(mockFhirContextIntializer.getFhirContext("R4")).thenReturn(mockContext);
    when(mockFhirContextIntializer.createClient(mockContext, "", "", "", null))
        .thenReturn(mockClient);

    Encounter r4Encounter = new Encounter();
    Period period = new Period();
    period.setStart(new Date());
    r4Encounter.setPeriod(period);
    r4Encounter.setStatus(Encounter.EncounterStatus.FINISHED);

    when(mockClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId("r4-789")).thenReturn(readExecutable);
    when(readExecutable.execute()).thenReturn(r4Encounter);

    boolean result = EcaUtils.checkEncounterClose(mockDetails);

    assertTrue(result);
  }

  @Test
  public void testCheckEncounterClose_Dstu2WithEndDates() throws Exception {
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(ActionRepo.getInstance().getFhirContextInitializer())
        .thenReturn(mockFhirContextIntializer);

    FhirContext ctx = Mockito.mock(FhirContext.class);
    IGenericClient client = Mockito.mock(IGenericClient.class);
    IRead read = mock(IRead.class);
    IReadTyped readTyped = mock(IReadTyped.class);
    IReadExecutable readExec = mock(IReadExecutable.class);

    when(mockFhirContextIntializer.getFhirContext("DSTU2")).thenReturn(ctx);
    when(mockFhirContextIntializer.createClient(any(), any(), any(), any(), any()))
        .thenReturn(client);
    when(client.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readTyped);
    when(readTyped.withId("123")).thenReturn(readExec);

    ca.uhn.fhir.model.dstu2.resource.Encounter dstu2 =
        new ca.uhn.fhir.model.dstu2.resource.Encounter();
    ca.uhn.fhir.model.dstu2.composite.PeriodDt period =
        new ca.uhn.fhir.model.dstu2.composite.PeriodDt();
    period.setEnd(new ca.uhn.fhir.model.primitive.DateTimeDt(new Date()));
    dstu2.setPeriod(period);

    when(readExec.execute()).thenReturn(dstu2);
    when(mockDetails.getEncounterId()).thenReturn("123");
    when(mockDetails.getFhirVersion()).thenReturn("DSTU2");

    assertTrue(EcaUtils.checkEncounterClose(mockDetails));
  }

  @Test
  public void testCheckEncounterClose_NullDetails() {
    assertFalse(EcaUtils.checkEncounterClose(null));
  }

  @Test
  public void testCheckEncounterClose_NullEncounterId() {
    when(mockDetails.getEncounterId()).thenReturn(null);
    assertFalse(EcaUtils.checkEncounterClose(mockDetails));
  }

  @Test
  public void testCheckEncounterClose_EmptyEncounterId() {
    when(mockDetails.getEncounterId()).thenReturn("");
    assertFalse(EcaUtils.checkEncounterClose(mockDetails));
  }

  @Test
  public void testRecheckTriggerCodes() {
    AbstractAction mockAction = mock(AbstractAction.class);
    Set<AbstractAction> actions = new HashSet<>(Arrays.asList(mockAction));
    Map<EventTypes.EcrActionTypes, Set<AbstractAction>> actionMap = new HashMap<>();
    actionMap.put(EventTypes.EcrActionTypes.MATCH_TRIGGER, actions);

    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(mockActionRepo.getActions()).thenReturn(actionMap);
    when(mockActionRepo.getLaunchService()).thenReturn(mock(LaunchService.class));

    PatientExecutionState expectedState = new PatientExecutionState();
    PowerMockito.mockStatic(ApplicationUtils.class);
    when(ApplicationUtils.getDetailStatus(any())).thenReturn(expectedState);

    PatientExecutionState result = EcaUtils.recheckTriggerCodes(mockDetails, null);

    assertNotNull(result);
  }
}
