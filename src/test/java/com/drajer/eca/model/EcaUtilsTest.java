package com.drajer.eca.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.IReadExecutable;
import ca.uhn.fhir.rest.gclient.IReadTyped;
import com.drajer.cdafromdstu2.Dstu2CdaEicrGenerator;
import com.drajer.cdafromr4.CdaEicrGeneratorFromR4;
import com.drajer.ecrapp.config.AppConfig;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.fhir.utils.ecrretry.EcrFhirRetryableRead;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.impl.EicrServiceImpl;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
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
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({
  ValueSetSingleton.class,
  ApplicationUtils.class,
  Dstu2CdaEicrGenerator.class,
  ActionRepo.class,
  CdaEicrGeneratorFromR4.class
})
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
  public void testCreateEicr_Dstu2() {

    // SetUp
    when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
    when(mockActionRepo.getEicrRRService()).thenReturn(mockRRSrvc);
    when(mockActionRepo.getLoadingQueryService()).thenReturn(mockQuerySrvc);
    when(mockQuerySrvc.getData(eq(mockDetails), eq(null), eq(null))).thenReturn(mockDstu2Data);

    PowerMockito.mockStatic(Dstu2CdaEicrGenerator.class);
    when(Dstu2CdaEicrGenerator.convertDstu2FhirBundletoCdaEicr(
            any(Dstu2FhirData.class), eq(mockDetails), any(Eicr.class)))
        .thenReturn("This is DSTU2 EICR data");

    // Test
    Eicr eicr = EcaUtils.createEicr(mockDetails);

    // Validate
    assertEquals("This is DSTU2 EICR data", eicr.getEicrData());
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
            any(R4FhirData.class), eq(mockDetails), any(Eicr.class)))
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
    boolean checkEncounterClose = EcaUtils.checkEncounterClose(mockDetails);
    assertTrue(mockr4Encounter.getPeriod().getStart() == null);
    assertTrue(mockDetails.getStartDate() != null);
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
}
