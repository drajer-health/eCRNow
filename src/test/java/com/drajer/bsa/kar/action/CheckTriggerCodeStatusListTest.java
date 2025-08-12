package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.anyString;
import static org.powermock.api.mockito.PowerMockito.*;

import com.drajer.eca.model.MatchedTriggerCodes;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
public class CheckTriggerCodeStatusListTest {

  @InjectMocks CheckTriggerCodeStatusList checkTriggerCodeStatusList;

  @Mock CheckTriggerCodeStatus checkTriggerCodeStatus;

  @Before
  public void setUp() {
    CheckTriggerCodeStatus status = new CheckTriggerCodeStatus();
    Set<String> matchableCodes = new HashSet<>();
    matchableCodes.add("http://snomed.info/sct|123456");
    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
    mtc.setMatchedCodes(matchableCodes);
    mtc.setValueSet("Example VS");
    mtc.setValueSetOid("1.2.3.4");
    mtc.setValueSetVersion("20240101");
    mtc.setMatchedPath("path");
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    matchedCodes.add(mtc);
    status.setMatchedCodes(matchedCodes);
    status.setTriggerMatchStatus(true);
    status.setActionId("id1");
    Set<CheckTriggerCodeStatus> statuses = new HashSet<>();
    statuses.add(status);
    checkTriggerCodeStatusList.setStatuses(statuses);
  }

  @Test
  public void testAddCheckTriggerCodeStatus_AddsStatus() {
    CheckTriggerCodeStatus mockstatus = mock(CheckTriggerCodeStatus.class);
    Set<CheckTriggerCodeStatus> statuses = new HashSet<>();
    statuses.add(mockstatus);
    checkTriggerCodeStatusList.addCheckTriggerCodeStatus(mockstatus);

    Set<CheckTriggerCodeStatus> result = checkTriggerCodeStatusList.getStatuses();
    assertEquals(2, result.size(), "Statuses set should contain one element after adding");
    assertTrue("Statuses set should contain the added status", result.contains(mockstatus));
  }

  @Test
  public void testAddCheckTriggerCodeStatus_AddStatusesIsNull() {
    CheckTriggerCodeStatus ctcs = new CheckTriggerCodeStatus();
    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
    mtc.setValueSet("NewVS");
    mtc.setValueSetOid("5.6.7.8");
    mtc.setValueSetVersion("20250101");
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    matchedCodes.add(mtc);
    ctcs.setMatchedCodes(matchedCodes);
    ctcs.setTriggerMatchStatus(true);
    ctcs.setActionId("id2");
    Set<CheckTriggerCodeStatus> statuses = new HashSet<>();
    statuses.add(ctcs);
    checkTriggerCodeStatusList.setStatuses(null);
    checkTriggerCodeStatusList.addCheckTriggerCodeStatus(ctcs);

    Set<CheckTriggerCodeStatus> result = checkTriggerCodeStatusList.getStatuses();
    assertEquals(1, result.size(), "Statuses set should contain one element after adding");
    assertTrue("Statuses set should contain the added status", result.contains(ctcs));
  }

  @Test
  public void testCompareCodes_NewCodeNotPresentInExisting_ReturnsTrue() {
    CheckTriggerCodeStatus status = new CheckTriggerCodeStatus();
    Set<String> matchableCodes = new HashSet<>();
    matchableCodes.add("http://snomed.info/sct|999999");
    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
    mtc.setMatchedCodes(matchableCodes);
    mtc.setValueSet("NewVS");
    mtc.setValueSetOid("5.6.7.8");
    mtc.setValueSetVersion("20250101");
    mtc.setMatchedPath("newPath");
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    matchedCodes.add(mtc);
    status.setMatchedCodes(matchedCodes);
    status.setTriggerMatchStatus(true);
    status.setActionId("id2");
    Set<CheckTriggerCodeStatus> statuses = new HashSet<>();
    statuses.add(status);
    CheckTriggerCodeStatusList currentTriggerMatchStatus = new CheckTriggerCodeStatusList();
    currentTriggerMatchStatus.setStatuses(statuses);

    when(checkTriggerCodeStatus.isCodePresent(anyString(), anyString())).thenReturn(false);

    Boolean result = checkTriggerCodeStatusList.compareCodes(currentTriggerMatchStatus);

    assertTrue("Should return true when new unmatched code is present", result);
  }

  @Test
  public void testCompareCodes_NewCodeNotPresentStatusesIsNull_ReturnsTrue() {
    checkTriggerCodeStatusList.setStatuses(null);
    CheckTriggerCodeStatus status = new CheckTriggerCodeStatus();
    Set<String> matchableCodes = new HashSet<>();
    matchableCodes.add("http://snomed.info/sct|999999");
    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
    mtc.setMatchedCodes(matchableCodes);
    mtc.setValueSet("NewVS");
    mtc.setValueSetOid("5.6.7.8");
    mtc.setValueSetVersion("20250101");
    mtc.setMatchedPath("newPath");
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    matchedCodes.add(mtc);
    status.setMatchedCodes(matchedCodes);
    status.setTriggerMatchStatus(true);
    status.setActionId("id2");
    Set<CheckTriggerCodeStatus> statuses = new HashSet<>();
    statuses.add(status);
    CheckTriggerCodeStatusList currentTriggerMatchStatus = new CheckTriggerCodeStatusList();
    currentTriggerMatchStatus.setStatuses(statuses);

    when(checkTriggerCodeStatus.isCodePresent(anyString(), anyString())).thenReturn(false);

    Boolean result = checkTriggerCodeStatusList.compareCodes(currentTriggerMatchStatus);

    assertTrue("Should return true when new unmatched code is present", result);
  }

  @Test
  public void testCompareCodes_AllCodesAlreadyPresent_Returnsfalse() {
    CheckTriggerCodeStatus status = new CheckTriggerCodeStatus();
    Set<String> matchableCodes = new HashSet<>();
    matchableCodes.add("http://snomed.info/sct|123456");
    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
    mtc.setMatchedCodes(matchableCodes);
    mtc.setValueSet("Example VS");
    mtc.setValueSetOid("1.2.3.4");
    mtc.setValueSetVersion("20240101");
    mtc.setMatchedPath("path");
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    matchedCodes.add(mtc);
    status.setMatchedCodes(matchedCodes);
    status.setTriggerMatchStatus(true);
    status.setActionId("id1");
    Set<CheckTriggerCodeStatus> statuses = new HashSet<>();
    statuses.add(status);
    CheckTriggerCodeStatusList currentTriggerMatchStatus = new CheckTriggerCodeStatusList();
    currentTriggerMatchStatus.setStatuses(statuses);

    Boolean result = checkTriggerCodeStatusList.compareCodes(currentTriggerMatchStatus);

    assertFalse(result, "Should return false when all codes already exist");
  }

  @Test
  public void testIsCodePresent_CodeExists_ReturnsTrue() {
    when(checkTriggerCodeStatus.isCodePresent("http://snomed.info/sct|123456", "path"))
        .thenReturn(true);

    Boolean result =
        checkTriggerCodeStatusList.isCodePresent("http://snomed.info/sct|123456", "path");

    assertTrue("Expected true since code is present", result);
  }

  @Test
  public void testIsCodePresent_CodeNotExists_ReturnsFalse() {
    when(checkTriggerCodeStatus.isCodePresent("http://new.info/sct|9877", "path2"))
        .thenReturn(false);

    Boolean result = checkTriggerCodeStatusList.isCodePresent("code2", "path2");

    assertFalse(result, "Expected false since code is not present");
  }
}
