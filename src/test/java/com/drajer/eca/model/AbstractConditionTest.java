package com.drajer.eca.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.hl7.fhir.r4.model.PlanDefinition.ActionConditionKind;
import org.junit.Before;
import org.junit.Test;

public class AbstractConditionTest {

  private AbstractCondition condition;

  @Before
  public void setUp() {
    condition =
        new AbstractCondition() {
          @Override
          public Boolean evaluate(Object obj) {
            return true;
          }

          @Override
          public void print() {}
        };
  }

  @Test
  public void testConditionTypeSetAndGet() {
    condition.setConditionType(ActionConditionKind.APPLICABILITY);
    assertEquals(ActionConditionKind.APPLICABILITY, condition.getConditionType());
  }

  @Test
  public void testPrintBaseWithConditionType() {
    condition.setConditionType(ActionConditionKind.APPLICABILITY);
    condition.printBase();
    assertEquals(ActionConditionKind.APPLICABILITY, condition.getConditionType());
  }

  @Test
  public void testPrintBaseWithNullConditionType() {
    condition.setConditionType(null);
    condition.printBase();
    assertNull(condition.getConditionType());
  }
}
