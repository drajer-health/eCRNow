package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.drajer.eca.model.MatchedTriggerCodes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import org.junit.Test;

/**
 * CheckTriggerCodeStatusList decides whether a reporting run matched anything new compared with the
 * previous run. The codes are the ones the eRSD trigger value sets actually contain: SNOMED
 * 840539006 (disease caused by SARS-CoV-2) and LOINC 94500-6 (SARS-CoV-2 RNA, NAA with probe).
 */
public class CheckTriggerCodeStatusListTest {

  private static final String COVID_CONDITION = "http://snomed.info/sct|840539006";
  private static final String COVID_LAB_TEST = "http://loinc.org|94500-6";
  private static final String CONDITION_PATH = "Condition.code";
  private static final String OBSERVATION_PATH = "Observation.code";

  @Test
  public void testAddCheckTriggerCodeStatus_addsToExistingSetAndRecreatesANullOne() {
    CheckTriggerCodeStatusList list = new CheckTriggerCodeStatusList();
    CheckTriggerCodeStatus condition = matched(CONDITION_PATH, COVID_CONDITION);
    CheckTriggerCodeStatus labTest = matched(OBSERVATION_PATH, COVID_LAB_TEST);

    list.addCheckTriggerCodeStatus(condition);
    list.addCheckTriggerCodeStatus(labTest);

    assertEquals(2, list.getStatuses().size());

    list.setStatuses(null);
    list.addCheckTriggerCodeStatus(condition);

    assertEquals(
        "a null set must be replaced rather than dereferenced", 1, list.getStatuses().size());
    assertTrue(list.getStatuses().contains(condition));
  }

  @Test
  public void testIsCodePresent_requiresTheCodeOnTheSamePath() {
    CheckTriggerCodeStatusList list = new CheckTriggerCodeStatusList();
    list.addCheckTriggerCodeStatus(matched(CONDITION_PATH, COVID_CONDITION));
    list.addCheckTriggerCodeStatus(matched(OBSERVATION_PATH, COVID_LAB_TEST));

    assertTrue(list.isCodePresent(COVID_CONDITION, CONDITION_PATH));
    assertTrue(list.isCodePresent(COVID_LAB_TEST, OBSERVATION_PATH));
    assertFalse(
        "the right code on the wrong path is a different match",
        list.isCodePresent(COVID_CONDITION, OBSERVATION_PATH));
    assertFalse(list.isCodePresent("http://snomed.info/sct|0000000", CONDITION_PATH));
    assertFalse(new CheckTriggerCodeStatusList().isCodePresent(COVID_CONDITION, CONDITION_PATH));
  }

  /** With no previous run to compare against, anything matched now is new. */
  @Test
  public void testCompareCodes_withNoPreviousStatusesOnlyNewMatchesCount() {
    CheckTriggerCodeStatusList nothingBefore = new CheckTriggerCodeStatusList();
    CheckTriggerCodeStatusList matchedNow = new CheckTriggerCodeStatusList();
    matchedNow.addCheckTriggerCodeStatus(matched(CONDITION_PATH, COVID_CONDITION));

    assertTrue(nothingBefore.compareCodes(matchedNow));
    assertFalse(nothingBefore.compareCodes(new CheckTriggerCodeStatusList()));

    CheckTriggerCodeStatusList noStatusesAtAll = new CheckTriggerCodeStatusList();
    noStatusesAtAll.setStatuses(null);
    assertFalse(nothingBefore.compareCodes(noStatusesAtAll));
  }

  @Test
  public void testCompareCodes_reportsOnlyCodesNotSeenInThePreviousRun() {
    CheckTriggerCodeStatusList previous = new CheckTriggerCodeStatusList();
    previous.addCheckTriggerCodeStatus(matched(CONDITION_PATH, COVID_CONDITION));

    CheckTriggerCodeStatusList same = new CheckTriggerCodeStatusList();
    same.addCheckTriggerCodeStatus(matched(CONDITION_PATH, COVID_CONDITION));
    assertFalse("the same code on the same path is not new", previous.compareCodes(same));

    CheckTriggerCodeStatusList newCode = new CheckTriggerCodeStatusList();
    newCode.addCheckTriggerCodeStatus(matched(CONDITION_PATH, COVID_CONDITION, COVID_LAB_TEST));
    assertTrue("an extra code on the path is new", previous.compareCodes(newCode));

    CheckTriggerCodeStatusList newPath = new CheckTriggerCodeStatusList();
    newPath.addCheckTriggerCodeStatus(matched(OBSERVATION_PATH, COVID_CONDITION));
    assertTrue("the same code on another path is new", previous.compareCodes(newPath));
  }

  /** Matched values are compared together with matched codes. */
  @Test
  public void testCompareCodes_alsoComparesMatchedValues() {
    CheckTriggerCodeStatusList previous = new CheckTriggerCodeStatusList();
    previous.addCheckTriggerCodeStatus(matched(OBSERVATION_PATH, COVID_LAB_TEST));

    MatchedTriggerCodes withValue = new MatchedTriggerCodes();
    withValue.setMatchedPath(OBSERVATION_PATH);
    withValue.addValue("http://loinc.org|94534-5");
    CheckTriggerCodeStatus status = new CheckTriggerCodeStatus();
    status.setMatchedCodes(new ArrayList<>(Collections.singletonList(withValue)));
    CheckTriggerCodeStatusList current = new CheckTriggerCodeStatusList();
    current.addCheckTriggerCodeStatus(status);

    assertTrue(previous.compareCodes(current));
  }

  private CheckTriggerCodeStatus matched(String path, String... codes) {
    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setMatchedPath(path);
    matchedTriggerCodes.setMatchedCodes(new HashSet<>(Arrays.asList(codes)));
    CheckTriggerCodeStatus status = new CheckTriggerCodeStatus();
    status.setMatchedCodes(new ArrayList<>(Collections.singletonList(matchedTriggerCodes)));
    return status;
  }
}
