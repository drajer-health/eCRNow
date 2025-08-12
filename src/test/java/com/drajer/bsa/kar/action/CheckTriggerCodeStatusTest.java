// package com.drajer.bsa.kar.action;
//
// import static org.junit.Assert.*;
// import static org.mockito.Mockito.times;
// import static org.mockito.Mockito.verify;
// import static org.powermock.api.mockito.PowerMockito.*;
//
// import com.drajer.eca.model.MatchedTriggerCodes;
// import java.util.*;
// import org.hl7.fhir.r4.model.*;
// import org.javatuples.Pair;
// import org.junit.Test;
// import org.junit.runner.RunWith;
// import org.mockito.InjectMocks;
// import org.mockito.Mockito;
// import org.powermock.modules.junit4.PowerMockRunner;
//
// @RunWith(PowerMockRunner.class)
// public class CheckTriggerCodeStatusTest {
//
//  @InjectMocks CheckTriggerCodeStatus checkTriggerCodeStatus;
//
//  @Test
//  public void testGetMatchedCodewhenCodeMatches_shouldReturnTrueWithReportableMatch() {
//
//    CodeableConcept codeableConcept = new CodeableConcept();
//    codeableConcept.setDisallowExtensions(true);
//    codeableConcept.setId("76324");
//    Coding coding = new Coding();
//    coding.setCode("123456");
//    coding.setDisplay("Annual wellness visit (procedure)");
//    coding.setSystem("http://snomed.info/sct");
//    List<Coding> codingList = new ArrayList<>();
//    codingList.add(coding);
//    codeableConcept.setCoding(codingList);
//    Set<String> matchableCodes = new HashSet<>();
//    matchableCodes.add("http://snomed.info/sct|123456");
//    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
//    mtc.setMatchedCodes(matchableCodes);
//    mtc.setValueSet("Example VS");
//    mtc.setValueSetOid("1.2.3.4");
//    mtc.setValueSetVersion("20240101");
//    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
//    matchedCodes.add(mtc);
//    checkTriggerCodeStatus.setMatchedCodes(matchedCodes);
//
//    Pair<Boolean, ReportableMatchedTriggerCode> result =
//        checkTriggerCodeStatus.getMatchedCode(codeableConcept);
//    assertTrue(result.getValue0());
//    ReportableMatchedTriggerCode matched = result.getValue1();
//    assertEquals(matchableCodes, matched.getAllMatches());
//    assertEquals("123456", matched.getCode());
//    assertEquals("http://snomed.info/sct", matched.getCodeSystem());
//    assertEquals("Example VS", matched.getValueSet());
//    assertEquals("1.2.3.4", matched.getValueSetOid());
//    assertEquals("20240101", matched.getValueSetVersion());
//  }
//
//  @Test
//  public void testGetMatchedCode_whenCodeDoesNotMatch_shouldReturnFalseAndNull() {
//    CodeableConcept codeableConcept = new CodeableConcept();
//    codeableConcept.setDisallowExtensions(true);
//    codeableConcept.setId("76324");
//    Coding coding = new Coding();
//    coding.setCode("8976");
//    coding.setDisplay("Annual wellness visit (procedure)");
//    coding.setSystem("http://not.match/sct");
//    List<Coding> codingList = new ArrayList<>();
//    codingList.add(coding);
//    codeableConcept.setCoding(codingList);
//    Set<String> matchableCodes = new HashSet<>();
//    matchableCodes.add("http://snomed.info/sct|123456");
//    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
//    mtc.setMatchedCodes(matchableCodes);
//    mtc.setValueSet("Example VS");
//    mtc.setValueSetOid("1.2.3.4");
//    mtc.setValueSetVersion("20240101");
//    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
//    matchedCodes.add(mtc);
//    checkTriggerCodeStatus.setMatchedCodes(matchedCodes);
//
//    Pair<Boolean, ReportableMatchedTriggerCode> result =
//        checkTriggerCodeStatus.getMatchedCode(codeableConcept);
//
//    assertFalse(result.getValue0());
//    assertNull(result.getValue1());
//  }
//
//  @Test
//  public void testGetMatchedTriggerCodes_whenValuesContainMatch_shouldReturnMatchedTriggerCode() {
//    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
//    mtc.setMatchedPath("Observation");
//    mtc.setValueSet("COVID");
//    mtc.setValueSetVersion("2025");
//    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
//    matchedCodes.add(mtc);
//    checkTriggerCodeStatus.setMatchedCodes(matchedCodes);
//
//    MatchedTriggerCodes result =
//        checkTriggerCodeStatus.getMatchedTriggerCodes("Observation", "COVID", "2025");
//
//    assertNotNull(result);
//    assertEquals("COVID", result.getValueSet());
//    assertEquals("2025", result.getValueSetVersion());
//  }
//
//  @Test
//  public void testGetMatchedTriggerCodes_whenValuesDoNotContainMatch_shouldReturnNull() {
//    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
//    mtc.setMatchedPath("Observation");
//    mtc.setValueSet("COVID");
//    mtc.setValueSetVersion("2024");
//    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
//    matchedCodes.add(mtc);
//    checkTriggerCodeStatus.setMatchedCodes(matchedCodes);
//
//    MatchedTriggerCodes result =
//        checkTriggerCodeStatus.getMatchedTriggerCodes("Condition", "Canser", "2025");
//
//    assertNull(result);
//  }
//
//  @Test
//  public void testContainsMatches_whenMatchedTriggerCodeReturnsTrue_shouldReturnTrue() {
//    ResourceType resourceType = ResourceType.Observation;
//    MatchedTriggerCodes mtc = Mockito.mock(MatchedTriggerCodes.class);
//    when(mtc.containsMatch(resourceType)).thenReturn(true);
//    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
//    matchedCodes.add(mtc);
//    checkTriggerCodeStatus.setMatchedCodes(matchedCodes);
//
//    boolean result = checkTriggerCodeStatus.containsMatches(resourceType);
//
//    assertTrue(result);
//  }
//
//  @Test
//  public void testContainsMatches_whenMatchedTriggerCodeReturnsFalse_shouldReturnFalse() {
//    ResourceType resourceType = ResourceType.Observation;
//    MatchedTriggerCodes mtc = Mockito.mock(MatchedTriggerCodes.class);
//    when(mtc.containsMatch(resourceType)).thenReturn(false);
//    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
//    matchedCodes.add(mtc);
//    checkTriggerCodeStatus.setMatchedCodes(matchedCodes);
//
//    boolean result = checkTriggerCodeStatus.containsMatches(resourceType);
//
//    assertFalse(result);
//  }
//
//  @Test
//  public void testAddMatchedTriggerCodes_whenValid_shouldAddToList() {
//    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
//    mtc.setValueSet("TestVS");
//
//    checkTriggerCodeStatus.addMatchedTriggerCodes(mtc);
//
//    List<MatchedTriggerCodes> result = checkTriggerCodeStatus.getMatchedCodes();
//    assertNotNull(result);
//    assertEquals(1, result.size());
//    assertEquals("TestVS", result.get(0).getValueSet());
//  }
//
//  @Test
//  public void testAddMatchedCodes_whenNoExistingMatch_shouldCreateAndAddNew() {
//
//    Set<String> codes = new HashSet<>();
//    codes.add("http://loinc.org|1234-5");
//
//    String valueSet = "TestVS";
//    String path = "Observation.code";
//    String version = "20240101";
//
//    checkTriggerCodeStatus.addMatchedCodes(codes, valueSet, path, version);
//
//    List<MatchedTriggerCodes> matched = checkTriggerCodeStatus.getMatchedCodes();
//    assertEquals(1, matched.size());
//
//    MatchedTriggerCodes mtc = matched.get(0);
//    assertEquals(valueSet, mtc.getValueSet());
//    assertEquals(version, mtc.getValueSetVersion());
//    assertEquals(path, mtc.getMatchedPath());
//    assertEquals(codes, mtc.getMatchedCodes());
//    assertTrue(checkTriggerCodeStatus.getTriggerMatchStatus());
//  }
//
//  @Test
//  public void testAddMatchedCodes_whenExistingMatch() {
//    MatchedTriggerCodes existing = new MatchedTriggerCodes();
//    existing.setValueSet("TestVS");
//    existing.setValueSetVersion("20240101");
//    existing.setMatchedPath("Observation.code");
//    checkTriggerCodeStatus.addMatchedTriggerCodes(existing);
//
//    Set<String> newCodes = new HashSet<>();
//    newCodes.add("http://loinc.org|6789-0");
//
//    checkTriggerCodeStatus.addMatchedCodes(newCodes, "TestVS", "Observation.code", "20240101");
//
//    List<MatchedTriggerCodes> matched = checkTriggerCodeStatus.getMatchedCodes();
//    assertEquals(1, matched.size());
//
//    assertEquals(newCodes, matched.get(0).getMatchedCodes());
//    assertTrue(checkTriggerCodeStatus.getTriggerMatchStatus());
//  }
//
//  @Test
//  public void testSetTriggerMatchStatus_shouldSetValueCorrectly() {
//    checkTriggerCodeStatus.setTriggerMatchStatus(true);
//
//    assertTrue(checkTriggerCodeStatus.getTriggerMatchStatus());
//  }
//
//  @Test
//  public void testSetMatchedResources_shouldStoreResourcesCorrectly() {
//    String Id = "obs-1";
//    Observation observation = new Observation();
//    Set<Resource> resourceSet = new HashSet<>();
//    resourceSet.add(observation);
//
//    Map<String, Set<Resource>> testMap = new HashMap<>();
//    testMap.put(Id, resourceSet);
//
//    checkTriggerCodeStatus.setMatchedResources(testMap);
//
//    Map<String, Set<Resource>> result = checkTriggerCodeStatus.getMatchedResources();
//
//    assertNotNull(result);
//    assertEquals(1, result.size());
//    assertTrue(result.containsKey(Id));
//    assertTrue(result.get(Id).contains(observation));
//  }
//
//  @Test
//  public void testLog() {
//    MatchedTriggerCodes mtc = mock(MatchedTriggerCodes.class);
//    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
//    matchedCodes.add(mtc);
//    checkTriggerCodeStatus.setMatchedCodes(matchedCodes);
//    checkTriggerCodeStatus.setTriggerMatchStatus(true);
//
//    checkTriggerCodeStatus.log();
//
//    verify(mtc, times(1)).log();
//  }
//
//  @Test
//  public void testIsCodePresent_Found() {
//    String matchedPath = "Observation";
//    String code = "435";
//    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
//    mtc.setMatchedPath("Observation");
//    mtc.addCode("435");
//    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
//    matchedCodes.add(mtc);
//    checkTriggerCodeStatus.setMatchedCodes(matchedCodes);
//
//    boolean result = checkTriggerCodeStatus.isCodePresent(code, matchedPath);
//    assertTrue(result);
//  }
//
//  @Test
//  public void testIsCodePresent_IsNotFound() {
//    String matchedPath = "Observation";
//    String code = "231";
//    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
//    mtc.setMatchedPath("Observation");
//    mtc.addCode("435");
//    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
//    matchedCodes.add(mtc);
//    checkTriggerCodeStatus.setMatchedCodes(matchedCodes);
//
//    boolean result = checkTriggerCodeStatus.isCodePresent(code, matchedPath);
//    assertFalse(result);
//  }
// }
