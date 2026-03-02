package com.drajer.eca.model;

import static org.junit.Assert.*;

import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;

public class AbstractActionTest {

  private AbstractAction action;

  private static class TestAction extends AbstractAction {
    @Override
    public void execute(Object obj, EventTypes.WorkflowEvent launchType, String taskInstanceId) {}

    @Override
    public void print() {}
  }

  @Before
  public void setup() {
    action = new TestAction();
  }

  @Test
  public void testSetPreConditions() {
    List<AbstractCondition> conditions = new ArrayList<>();
    action.setPreConditions(conditions);
    assertSame(conditions, action.getPreConditions());
  }

  @Test
  public void testSetRelatedActions() {
    List<RelatedAction> related = new ArrayList<>();
    action.setRelatedActions(related);
    assertSame(related, action.getRelatedActions());
  }

  @Test
  public void testSetTimingData() {
    List<TimingSchedule> timing = new ArrayList<>();
    action.setTimingData(timing);
    assertSame(timing, action.getTimingData());
  }

  @Test
  public void testGetAndSetActionId() {
    String id = "action-123";
    action.setActionId(id);
    assertEquals(id, action.getActionId());
  }

  @Test
  public void testGetSetTriggerDataAndAddActionData() {
    assertNull(action.getTriggerData());
    ActionData ad = new ActionData();
    action.addActionData(ad);
    assertNotNull(action.getTriggerData());
    assertEquals(1, action.getTriggerData().size());
    List<ActionData> list = new ArrayList<>();
    list.add(new ActionData());
    action.setTriggerData(list);
    assertEquals(list, action.getTriggerData());
  }

  @Test
  public void testAddConditionAndSetPreConditions() {
    assertNull(action.getPreConditions());
    AbstractCondition cond =
        new AbstractCondition() {
          @Override
          public Boolean evaluate(Object obj) {
            return true;
          }

          @Override
          public void print() {}
        };
    action.addCondition(cond);
    assertNotNull(action.getPreConditions());
    assertEquals(1, action.getPreConditions().size());
    assertSame(cond, action.getPreConditions().get(0));
    List<AbstractCondition> condList = new ArrayList<>();
    condList.add(cond);
    action.setPreConditions(condList);
    assertEquals(condList, action.getPreConditions());
  }

  @Test
  public void testActionIdGetterSetter() {
    action.setActionId("action-123");
    assertEquals("action-123", action.getActionId());
  }

  @Test
  public void testAddRelatedActionAndSetters() {
    RelatedAction ra =
        new RelatedAction() {
          @Override
          public void print() {}
        };
    assertNull(action.getRelatedActions());
    action.addRelatedAction(ra);
    assertNotNull(action.getRelatedActions());
    assertEquals(1, action.getRelatedActions().size());
    assertSame(ra, action.getRelatedActions().get(0));
    List<RelatedAction> list = new ArrayList<>();
    list.add(ra);
    action.setRelatedActions(list);
    assertEquals(list, action.getRelatedActions());
  }

  @Test
  public void testAddTimingDataAndSetters() {
    TimingSchedule ts =
        new TimingSchedule() {
          @Override
          public void print() {}
        };
    assertNull(action.getTimingData());
    action.addTimingData(ts);
    assertNotNull(action.getTimingData());
    assertEquals(1, action.getTimingData().size());
    assertSame(ts, action.getTimingData().get(0));
    List<TimingSchedule> list = new ArrayList<>();
    list.add(ts);
    action.setTimingData(list);
    assertEquals(list, action.getTimingData());
  }

  @Test
  public void testAddTriggerDataGetterSetter() {
    ActionData ad =
        new ActionData() {
          @Override
          public void print() {}
        };
    assertNull(action.getTriggerData());
    action.addActionData(ad);
    assertNotNull(action.getTriggerData());
    assertEquals(1, action.getTriggerData().size());
    assertSame(ad, action.getTriggerData().get(0));
    List<ActionData> list = new ArrayList<>();
    list.add(ad);
    action.setTriggerData(list);
    assertEquals(list, action.getTriggerData());
  }

  @Test
  public void testAddPreConditionsAndMatchCondition() {
    AbstractCondition cond =
        new AbstractCondition() {
          @Override
          public Boolean evaluate(Object obj) {
            return true;
          }

          @Override
          public void print() {}
        };

    assertNull(action.getPreConditions());
    action.addCondition(cond);
    assertNotNull(action.getPreConditions());
    assertEquals(1, action.getPreConditions().size());
    assertTrue(action.matchCondition(new LaunchDetails()));
    AbstractCondition condFalse =
        new AbstractCondition() {
          @Override
          public Boolean evaluate(Object obj) {
            return false;
          }

          @Override
          public void print() {}
        };
    action.addCondition(condFalse);
    assertFalse(action.matchCondition(new LaunchDetails()));
  }

  @Test
  public void testPrintBaseRunsWithoutError() {
    AbstractCondition cond =
        new AbstractCondition() {
          @Override
          public Boolean evaluate(Object obj) {
            return true;
          }

          @Override
          public void print() {}
        };

    RelatedAction ra =
        new RelatedAction() {
          @Override
          public void print() {}
        };

    TimingSchedule ts =
        new TimingSchedule() {
          @Override
          public void print() {}
        };

    ActionData ad =
        new ActionData() {
          @Override
          public void print() {}
        };

    action.addCondition(cond);
    action.addRelatedAction(ra);
    action.addTimingData(ts);
    action.addActionData(ad);
    action.setActionId("action-001");

    action.printBase();
    assertEquals("action-001", action.getActionId());
    assertNotNull(action.getPreConditions());
    assertTrue(action.getPreConditions().contains(cond));
    assertNotNull(action.getRelatedActions());
    assertTrue(action.getRelatedActions().contains(ra));
    assertNotNull(action.getTimingData());
    assertTrue(action.getTimingData().contains(ts));
    assertNotNull(action.getTriggerData());
    assertTrue(action.getTriggerData().contains(ad));
  }
}
