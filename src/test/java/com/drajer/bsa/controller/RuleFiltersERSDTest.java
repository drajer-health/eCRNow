package com.drajer.bsa.controller;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import java.util.Arrays;
import java.util.Collection;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.springframework.test.context.TestPropertySource;

@RunWith(Parameterized.class)
@TestPropertySource(
    properties = {"kar.directory=src/test/resources/Bsa/Scenarios/kars/rulefilters"})
public class RuleFiltersERSDTest extends BaseKarsTest {
  protected FhirContext fhirContext = FhirContext.forCached(FhirVersionEnum.R4);

  public RuleFiltersERSDTest(TestCaseInfo testCaseInfo) {
    super(testCaseInfo);
  }

  @Test
  public void test() throws Exception {
    super.testScenarioAndValidate();
  }

  // This generates a list of "TestCaseInfos" that describe the scenario
  // Comment out lines that don't work to temporarily disable test cases.
  // for eCSD tests the main points are
  // 1. Whether or not a reporting Bundle is generated (i.e. initial-pop = true)
  // 2. The Bundle contains a Measure report (TODO: validate that the other
  // resources are present)
  // 3. The MeasureReport has the correct info for the test case
  @Parameters(name = "{0}")
  public static Collection<TestCaseInfo> data() {
    return Arrays.asList(
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_Example", "ex-trigger-all-criteria-missing", false),
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_Example", "ex-trigger-all-criteria-out", false),
        new TestCaseInfo("PlanDefinition_eRSD_Instance_Example", "ex-trigger-condition-in", false),
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_Example", "ex-trigger-encounter-completed-in", true),
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_Example", "ex-trigger-encounter-completed-out", true),
        new TestCaseInfo("PlanDefinition_eRSD_Instance_Example", "ex-trigger-encounter-in", true),
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_Example", "ex-trigger-encounter-inprogress-in", true),
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_Example", "ex-trigger-encounter-inprogress-out", true),
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_Example", "ex-trigger-encounter-immunization-in", true),
        new TestCaseInfo("PlanDefinition_eRSD_Instance_Example", "ex-trigger-in", true),
        new TestCaseInfo("PlanDefinition_eRSD_Instance_Example", "ex-trigger-laborder-in", true),
        new TestCaseInfo("PlanDefinition_eRSD_Instance_Example", "ex-trigger-labresult-in", true),
        new TestCaseInfo("PlanDefinition_eRSD_Instance_Example", "ex-trigger-labtest-in", true),
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_Example", "ex-trigger-medicationadministration-in", true),
        new TestCaseInfo(
            "PlanDefinition_eRSD_Instance_Example", "ex-trigger-medicationorder-in", true),
        new TestCaseInfo("PlanDefinition_eRSD_Instance_Example", "ex-trigger-trigger-out", true));
  }
}