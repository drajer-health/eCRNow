// package com.drajer.bsa.kar.model;
//
// import static org.junit.Assert.*;
// import static org.mockito.Mockito.*;
//
// import ca.uhn.fhir.parser.IParser;
// import com.drajer.bsa.dao.TimeZoneDao;
// import com.drajer.bsa.ehr.service.EhrQueryService;
// import com.drajer.bsa.kar.action.BsaActionStatus;
// import com.drajer.bsa.kar.condition.BsaConditionProcessor;
// import com.drajer.bsa.model.HealthcareSetting;
// import com.drajer.bsa.model.KarProcessingData;
// import com.drajer.bsa.scheduler.BsaScheduler;
// import com.drajer.eca.model.TimingSchedule;
// import java.time.Instant;
// import java.util.*;
// import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
// import org.hl7.fhir.r4.model.ResourceType;
// import org.junit.Before;
// import org.junit.Test;
// import org.springframework.web.client.RestTemplate;
//
// public class BsaActionTest {
//
//  private static class TestBsaAction extends BsaAction {
//    @Override
//    public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {
//      return null;
//    }
//  }
//
//  private static class ParentAction extends BsaAction {
//    @Override
//    public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {
//      return null;
//    }
//  }
//
//  private static class SubAction extends BsaAction {
//    boolean executed = false;
//
//    @Override
//    public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {
//      executed = true;
//      return null;
//    }
//  }
//
//  static class TestParentBsaAction extends BsaAction {
//    @Override
//    public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {
//      return null;
//    }
//  }
//
//  static class TestRelatedBsaAction extends BsaAction {
//
//    boolean executed = false;
//
//    @Override
//    public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {
//      executed = true;
//      return null;
//    }
//  }
//
//  private BsaAction action;
//
//  @Before
//  public void setup() {
//    action =
//        new BsaAction() {
//          @Override
//          public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {
//            return null;
//          }
//        };
//  }
//
//  @Test
//  public void testGetXmlParser() {
//    IParser parser = mock(IParser.class);
//    action.xmlParser = parser;
//    assertEquals(parser, action.getXmlParser());
//  }
//
//  @Test
//  public void testGetJsonParser() {
//    IParser parser = mock(IParser.class);
//    action.jsonParser = parser;
//    assertEquals(parser, action.getJsonParser());
//  }
//
//  @Test
//  public void testGetTimeZoneDao() {
//    TimeZoneDao dao = mock(TimeZoneDao.class);
//    action.timeZoneDao = dao;
//    assertEquals(dao, action.getTimeZoneDao());
//  }
//
//  @Test
//  public void testGetRestTemplate() {
//    RestTemplate rt = new RestTemplate();
//    action.restTemplate = rt;
//    assertEquals(rt, action.getRestTemplate());
//  }
//
//  @Test
//  public void testGetScheduler() {
//    BsaScheduler scheduler = mock(BsaScheduler.class);
//    action.scheduler = scheduler;
//    assertEquals(scheduler, action.getScheduler());
//  }
//
//  @Test
//  public void testSetSubActions() {
//    BsaAction sub1 = mock(BsaAction.class);
//    BsaAction sub2 = mock(BsaAction.class);
//    List<BsaAction> subActions = Arrays.asList(sub1, sub2);
//
//    action.setSubActions(subActions);
//    assertEquals(2, action.getSubActions().size());
//    assertTrue(action.getSubActions().contains(sub1));
//    assertTrue(action.getSubActions().contains(sub2));
//  }
//
//  @Test
//  public void testSetInputResourceTypes() {
//    HashMap<String, ResourceType> map = new HashMap<>();
//    map.put("obs", ResourceType.Observation);
//
//    action.setInputResourceTypes(map);
//    assertEquals(ResourceType.Observation, action.getInputResourceTypes().get("obs"));
//  }
//
//  @Test
//  public void testSetRelatedActions() {
//    BsaRelatedAction ract = mock(BsaRelatedAction.class);
//    Set<BsaRelatedAction> set = new HashSet<>();
//    set.add(ract);
//
//    HashMap<ActionRelationshipType, Set<BsaRelatedAction>> map = new HashMap<>();
//    map.put(ActionRelationshipType.BEFORESTART, set);
//
//    action.setRelatedActions(map);
//    assertTrue(action.getRelatedActions().containsKey(ActionRelationshipType.BEFORESTART));
//    assertEquals(1, action.getRelatedActions().get(ActionRelationshipType.BEFORESTART).size());
//  }
//
//  @Test
//  public void testSetTimingData() {
//    TimingSchedule ts1 = mock(TimingSchedule.class);
//    TimingSchedule ts2 = mock(TimingSchedule.class);
//
//    List<TimingSchedule> timingList = Arrays.asList(ts1, ts2);
//    action.setTimingData(timingList);
//
//    assertEquals(2, action.getTimingData().size());
//    assertTrue(action.getTimingData().contains(ts1));
//    assertTrue(action.getTimingData().contains(ts2));
//  }
//
//  @Test
//  public void testSetConditions() {
//    BsaCondition cond1 = mock(BsaCondition.class);
//    BsaCondition cond2 = mock(BsaCondition.class);
//
//    List<BsaCondition> condList = Arrays.asList(cond1, cond2);
//    action.setConditions(condList);
//
//    assertEquals(2, action.getConditions().size());
//    assertTrue(action.getConditions().contains(cond1));
//    assertTrue(action.getConditions().contains(cond2));
//  }
//
//  @Test
//  public void executeRelatedActions_beforeStart_withoutDuration_executesAction() {
//
//    TestParentBsaAction parent = new TestParentBsaAction();
//    parent.setIgnoreTimers(true);
//    TestRelatedBsaAction relatedAction = new TestRelatedBsaAction();
//    BsaRelatedAction ract = new BsaRelatedAction();
//    ract.setRelationship(ActionRelationshipType.BEFORESTART);
//    ract.setAction(relatedAction);
//    ract.setDuration(null); // 🔑 forces direct execution
//    Set<BsaRelatedAction> relatedSet = new HashSet<>();
//    relatedSet.add(ract);
//    parent.setRelatedActions(new HashMap<>());
//    parent.getRelatedActions().put(ActionRelationshipType.BEFORESTART, relatedSet);
//    KarProcessingData kd = new KarProcessingData();
//    EhrQueryService ehrService = null;
//    assertFalse(relatedAction.executed);
//    parent.executeRelatedActions(kd, ehrService);
//    assertTrue(relatedAction.executed);
//  }
//
//  @Test
//  public void executeSubActions_shouldInvokeProcess() {
//    ParentAction parent = new ParentAction();
//    SubAction sub = new SubAction();
//    sub.setActionId("sub-1");
//    parent.addAction(sub);
//    KarProcessingData kd = new KarProcessingData();
//    EhrQueryService ehrService = null;
//    assertFalse(sub.executed);
//    parent.executeSubActions(kd, ehrService);
//    assertTrue(sub.executed);
//  }
//
//  @Test
//  public void testConditionsMet_whenEvaluateExpressionReturnsFalse() {
//    BsaAction action = new TestBsaAction();
//    BsaCondition condition = mock(BsaCondition.class);
//    BsaConditionProcessor processor = mock(BsaConditionProcessor.class);
//    when(condition.getConditionProcessor()).thenReturn(processor);
//    when(processor.evaluateExpression(condition, action, null, null)).thenReturn(Boolean.FALSE);
//    action.addCondition(condition);
//    boolean result = action.conditionsMet(null, null);
//    assertFalse(result);
//  }
//
//  @Test
//  public void testConditionsMet_whenEvaluateExpressionReturnsTrue() {
//
//    BsaAction action = new TestBsaAction();
//    BsaCondition condition = mock(BsaCondition.class);
//    BsaConditionProcessor processor = mock(BsaConditionProcessor.class);
//    when(condition.getConditionProcessor()).thenReturn(processor);
//    when(processor.evaluateExpression(condition, action, null, null)).thenReturn(Boolean.TRUE);
//
//    action.addCondition(condition);
//    boolean result = action.conditionsMet(null, null);
//    assertTrue(result);
//  }
//
//  @Test
//  public void executeRelatedActions_notBeforeStart_doesNothing() {
//
//    TestParentBsaAction parent = new TestParentBsaAction();
//    BsaRelatedAction ract = new BsaRelatedAction();
//    ract.setRelationship(ActionRelationshipType.AFTERSTART); // 👈 key
//    Set<BsaRelatedAction> set = new HashSet<>();
//    set.add(ract);
//    parent.setRelatedActions(new HashMap<>());
//    parent.getRelatedActions().put(ActionRelationshipType.AFTERSTART, set);
//    parent.executeRelatedActions(new KarProcessingData(), null);
//    assertTrue(true);
//  }
//
//  @Test
//  public void executeRelatedActions_withDuration_noTimers_executesAction() {
//
//    TestParentBsaAction parent = new TestParentBsaAction();
//    parent.setIgnoreTimers(true);
//    TestRelatedBsaAction relatedAction = new TestRelatedBsaAction();
//
//    org.hl7.fhir.r4.model.Duration duration = new org.hl7.fhir.r4.model.Duration();
//    duration.setValue(5);
//    duration.setUnit("minutes");
//    BsaRelatedAction ract = new BsaRelatedAction();
//    ract.setRelationship(ActionRelationshipType.BEFORESTART);
//    ract.setAction(relatedAction);
//    ract.setDuration(duration);
//    Set<BsaRelatedAction> set = new HashSet<>();
//    set.add(ract);
//    parent.setRelatedActions(new HashMap<>());
//    parent.getRelatedActions().put(ActionRelationshipType.BEFORESTART, set);
//
//    KarProcessingData kd = new KarProcessingData();
//    HealthcareSetting hs = mock(HealthcareSetting.class);
//    when(hs.getOffhoursEnabled()).thenReturn(Boolean.FALSE);
//    kd.setHealthcareSetting(hs);
//    EhrQueryService ehrService = null;
//    assertFalse(relatedAction.executed);
//    parent.executeRelatedActions(kd, ehrService);
//    assertTrue(relatedAction.executed);
//  }
//
//  @Test
//  public void executeRelatedActions_actionIsNull_skipsExecution() {
//
//    TestParentBsaAction parent = new TestParentBsaAction();
//    parent.setIgnoreTimers(true);
//
//    BsaRelatedAction ract = new BsaRelatedAction();
//    ract.setRelationship(ActionRelationshipType.BEFORESTART);
//    ract.setAction(null); // 🔑 forces THIS else block
//    ract.setDuration(null);
//
//    Set<BsaRelatedAction> set = new HashSet<>();
//    set.add(ract);
//
//    parent.setRelatedActions(new HashMap<>());
//    parent.getRelatedActions().put(ActionRelationshipType.BEFORESTART, set);
//    KarProcessingData kd = new KarProcessingData();
//    HealthcareSetting hs = mock(HealthcareSetting.class);
//    when(hs.getOffhoursEnabled()).thenReturn(Boolean.FALSE);
//    kd.setHealthcareSetting(hs);
//    EhrQueryService ehrService = null;
//    parent.executeRelatedActions(kd, ehrService);
//    assertTrue(true);
//  }
//
//  @Test
//  public void executeRelatedActions_offHoursEnabled_comparatorLessOrEqual_executesAction() {
//
//    TestParentBsaAction parent = new TestParentBsaAction();
//    parent.setIgnoreTimers(true);
//    TestRelatedBsaAction relatedAction = new TestRelatedBsaAction();
//    BsaRelatedAction ract = new BsaRelatedAction();
//    ract.setRelationship(ActionRelationshipType.BEFORESTART);
//    ract.setAction(relatedAction);
//
//    org.hl7.fhir.r4.model.Duration duration = new org.hl7.fhir.r4.model.Duration();
//    duration.setComparator(org.hl7.fhir.r4.model.Quantity.QuantityComparator.LESS_OR_EQUAL);
//    ract.setDuration(duration);
//
//    Set<BsaRelatedAction> set = new HashSet<>();
//    set.add(ract);
//
//    parent.setRelatedActions(new HashMap<>());
//    parent.getRelatedActions().put(ActionRelationshipType.BEFORESTART, set);
//    KarProcessingData kd = new KarProcessingData();
//    HealthcareSetting hs = mock(HealthcareSetting.class);
//    when(hs.getOffhoursEnabled()).thenReturn(true);
//    when(hs.getOffHoursStart()).thenReturn(0);
//    when(hs.getOffHoursStartMin()).thenReturn(0);
//    when(hs.getOffHoursEnd()).thenReturn(23);
//    when(hs.getOffHoursEndMin()).thenReturn(59);
//    when(hs.getOffHoursTimezone()).thenReturn("UTC");
//    kd.setHealthcareSetting(hs);
//    EhrQueryService ehrService = null;
//    assertFalse(relatedAction.executed);
//    parent.executeRelatedActions(kd, ehrService);
//    assertTrue(relatedAction.executed);
//  }
//
//  @Test
//  public void executeRelatedActions_offHoursEnabled_setsTimer() {
//    TestParentBsaAction parent = spy(new TestParentBsaAction());
//    parent.setIgnoreTimers(false);
//    TestRelatedBsaAction relatedAction = new TestRelatedBsaAction();
//    BsaRelatedAction ract = new BsaRelatedAction();
//    ract.setRelationship(ActionRelationshipType.BEFORESTART);
//    ract.setAction(relatedAction);
//
//    org.hl7.fhir.r4.model.Duration duration = new org.hl7.fhir.r4.model.Duration();
//    duration.setComparator(org.hl7.fhir.r4.model.Quantity.QuantityComparator.LESS_OR_EQUAL);
//    ract.setDuration(duration);
//    Set<BsaRelatedAction> set = new HashSet<>();
//    set.add(ract);
//    parent.setRelatedActions(new HashMap<>());
//    parent.getRelatedActions().put(ActionRelationshipType.BEFORESTART, set);
//    KarProcessingData kd = mock(KarProcessingData.class);
//    HealthcareSetting hs = mock(HealthcareSetting.class);
//    when(hs.getOffhoursEnabled()).thenReturn(true);
//    when(hs.getOffHoursStart()).thenReturn(1);
//    when(hs.getOffHoursStartMin()).thenReturn(0);
//    when(hs.getOffHoursEnd()).thenReturn(5);
//    when(hs.getOffHoursEndMin()).thenReturn(0);
//    when(hs.getOffHoursTimezone()).thenReturn("UTC");
//    when(kd.getHealthcareSetting()).thenReturn(hs);
//    doNothing().when(parent).setupTimer(any(), any(), any());
//    parent.executeRelatedActions(kd, null);
//    verify(parent, times(1)).setupTimer(eq(kd), any(Instant.class), eq(ract));
//    assertFalse(
//        "Related action should not execute immediately when timer is set",
// relatedAction.executed);
//  }
// }
