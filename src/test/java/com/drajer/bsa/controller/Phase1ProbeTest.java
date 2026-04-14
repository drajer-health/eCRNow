package com.drajer.bsa.controller;

import static com.drajer.bsa.controller.ExpectedOutcome.*;

import java.util.Arrays;
import java.util.Collection;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.springframework.test.context.TestPropertySource;

/**
 * Probe: verifies that the current kars/rulefilters KAR pipeline can produce a REPORTED outcome at
 * all. Uses the existing ex-trigger-laborder-in scenario (REPORTED cases are commented out in
 * RuleFiltersERSDFhirPathOnlyTest with a note that they go stale — this isolates whether the
 * staleness is the only blocker).
 */
@RunWith(Parameterized.class)
@TestPropertySource(
    properties = {
      "kar.directory=src/test/resources/Bsa/Scenarios/kars/rulefilters",
      "cql.enabled=false"
    })
public class Phase1ProbeTest extends BaseKarsTest {

  public Phase1ProbeTest(TestCaseInfo testCaseInfo) {
    super(testCaseInfo);
  }

  @Test
  public void test() throws Exception {
    super.testScenarioAndValidate();
  }

  @Parameters(name = "{0}")
  public static Collection<TestCaseInfo> data() {
    return Arrays.asList(
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_FhirPathOnly",
            "http://hl7.org/fhir/us/ecr/PlanDefinition/plandefinition-ersd-instance-example",
            "ex-trigger-laborder-in",
            REPORTED),
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_FhirPathOnly",
            "http://hl7.org/fhir/us/ecr/PlanDefinition/plandefinition-ersd-instance-example",
            "ex-trigger-labresult-in",
            REPORTED));
  }
}
