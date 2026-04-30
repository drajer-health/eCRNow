package com.drajer.eca.model;

import static org.junit.Assert.*;

import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.junit.Before;
import org.junit.Test;
import org.powermock.api.mockito.PowerMockito;

public class RelatedActionTest {

  private RelatedAction relatedAction;
  private AbstractAction mockAbstractAction;
  private Duration mockDuration;

  @Before
  public void setUp() {
    relatedAction = new RelatedAction();
    mockAbstractAction = PowerMockito.mock(AbstractAction.class);

    mockDuration = new Duration();
    mockDuration.setValue(5.0);
    mockDuration.setUnit("hours");
  }

  @Test
  public void testSetAndGetRelationship() {
    relatedAction.setRelationship(ActionRelationshipType.AFTER);
    ActionRelationshipType rel = relatedAction.getRelationship();
    assertNotNull("Relationship should not be null", rel);
    assertEquals("Relationship should be AFTER", ActionRelationshipType.AFTER, rel);
  }

  @Test
  public void testSetAndGetRelatedAction() {
    relatedAction.setRelatedAction(mockAbstractAction);
    AbstractAction act = relatedAction.getRelatedAction();
    assertNotNull("RelatedAction should not be null", act);
    assertSame("RelatedAction should be the same object", mockAbstractAction, act);
  }
}
