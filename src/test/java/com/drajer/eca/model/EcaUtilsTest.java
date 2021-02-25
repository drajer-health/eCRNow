package com.drajer.eca.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import com.drajer.cdafromdstu2.Dstu2CdaEicrGenerator;
import com.drajer.cdafromr4.CdaEicrGeneratorFromR4;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.impl.EicrServiceImpl;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.test.util.TestUtils;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
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

  private List<ActionData> codePaths;
  private List<CodeableConceptDt> ptCodes;
  private List<CodeableConcept> ptCodes1;
  private Set<String> codesToMatch;
  private Set<String> codesToMatchAgainst;
  private MatchTriggerStatus matchTriggerStatus;

  @Before
  public void setUp() {

    // Mock required classes
    mockDetails = PowerMockito.mock(LaunchDetails.class);
    mockState = PowerMockito.mock(PatientExecutionState.class);
    mockDstu2Data = PowerMockito.mock(Dstu2FhirData.class);
    mockR4Data = PowerMockito.mock(R4FhirData.class);
    mockQuerySrvc = PowerMockito.mock(LoadingQueryService.class);
    mockRRSrvc = PowerMockito.mock(EicrServiceImpl.class);

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
    when(mockValueSet.getCovidValueSetsAsStringForGrouper(anyString()))
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
    when(mockValueSet.getCovidValueSetsAsStringForGrouper(anyString()))
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
