package com.drajer.bsa.controller;

import ca.uhn.fhir.context.FhirContext;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.springframework.test.context.TestPropertySource;

import java.util.Arrays;
import java.util.Collection;

import static com.drajer.bsa.controller.ExpectedOutcome.*;
import static com.drajer.bsa.controller.ExpectedOutcome.TRIGGERED_ONLY;

@RunWith(Parameterized.class)
@TestPropertySource(
        properties = {"kar.directory=src/test/resources/Bsa/Scenarios/kars/depression"})
public class DepressionECSDTest extends BaseKarsTest {
    protected FhirContext fhirContext = FhirContext.forR4();
    public DepressionECSDTest(TestCaseInfo testCaseInfo) {
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
    @Parameterized.Parameters(name = "{0}")
    public static Collection<TestCaseInfo> data() {
        return Arrays.asList(
                new TestCaseInfo(
                        "ChronicDSDepressionScreeningAndFollowUp",
                        "http://hl7.org/fhir/us/chronic-ds/PlanDefinition/ChronicDSDepressionScreeningAndFollowUp",
                        "Depression-trigger-in-adolescent",
                        TRIGGERED_ONLY),
                new TestCaseInfo(
                        "ChronicDSDepressionScreeningAndFollowUp",
                        "http://hl7.org/fhir/us/chronic-ds/PlanDefinition/ChronicDSDepressionScreeningAndFollowUp",
                        "Depression-trigger-in-adult",
                        TRIGGERED_ONLY),
                new TestCaseInfo(
                        "ChronicDSDepressionScreeningAndFollowUp",
                        "http://hl7.org/fhir/us/chronic-ds/PlanDefinition/ChronicDSDepressionScreeningAndFollowUp",
                        "Depression-trigger-missing",
                        NOT_TRIGGERED),
                new TestCaseInfo(
                        "ChronicDSDepressionScreeningAndFollowUp",
                        "http://hl7.org/fhir/us/chronic-ds/PlanDefinition/ChronicDSDepressionScreeningAndFollowUp",
                        "Depression-trigger-out",
                        NOT_TRIGGERED));
    }
}
