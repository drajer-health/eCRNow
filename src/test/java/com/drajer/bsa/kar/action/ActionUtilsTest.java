package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

import org.hl7.fhir.r4.model.Meta;
import org.hl7.fhir.r4.model.OperationOutcome;
import org.hl7.fhir.r4.model.OperationOutcome.IssueSeverity;
import org.hl7.fhir.r4.model.OperationOutcome.OperationOutcomeIssueComponent;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockitoAnnotations;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RunWith(PowerMockRunner.class)
@PrepareForTest({ActionUtils.class, LoggerFactory.class})
public class ActionUtilsTest {

  private Logger mockLogger;

  @Before
  public void setUp() {

    MockitoAnnotations.initMocks(this);
    mockLogger = mock(Logger.class);
    mockStatic(LoggerFactory.class);
    when(LoggerFactory.getLogger(ActionUtils.class)).thenReturn(mockLogger);
  }

  @Test
  public void testGetMeta() {
    String version = "1.0.0";
    String profile = "http://example.com/fhir/StructureDefinition/example";

    Meta meta = ActionUtils.getMeta(version, profile);

    assertNotNull(meta);
    assertEquals(version, meta.getVersionId());
    assertEquals(1, meta.getProfile().size());
    assertEquals(profile, meta.getProfile().get(0).getValueAsString());
    assertNotNull(meta.getLastUpdated());
  }

  @Test
  public void testOperationOutcomeHasErrors_WithErrors() {
    OperationOutcome outcome = new OperationOutcome();
    OperationOutcomeIssueComponent issue = new OperationOutcomeIssueComponent();
    issue.setSeverity(IssueSeverity.ERROR);
    outcome.addIssue(issue);
    boolean result = ActionUtils.operationOutcomeHasErrors(outcome);

    assertTrue(result);
  }

  @Test
  public void testOperationOutcomeHasErrors_NoErrors() {
    OperationOutcome outcome = new OperationOutcome();
    OperationOutcomeIssueComponent issue = new OperationOutcomeIssueComponent();
    issue.setSeverity(IssueSeverity.WARNING);
    outcome.addIssue(issue);

    boolean result = ActionUtils.operationOutcomeHasErrors(outcome);
    assertFalse(result);
  }
}
