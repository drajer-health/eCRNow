package com.drajer.bsa.controller;

import static com.drajer.bsa.controller.ExpectedOutcome.*;

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
    properties = {"kar.directory=src/test/resources/Bsa/Scenarios/kars/bloodpressure"})
public class ChronicBPECSDTest extends BaseKarsTest {
  protected FhirContext fhirContext = FhirContext.forCached(FhirVersionEnum.R4);

  public ChronicBPECSDTest(TestCaseInfo testCaseInfo) {
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
            "ChronicDSControllingBloodPressure", "bp-report-denom-exc-in", REPORTED, 1, 1, 1, 0),
        new TestCaseInfo(
            "ChronicDSControllingBloodPressure", "bp-report-ip-in", REPORTED, 1, 1, 0, 0),
        new TestCaseInfo("ChronicDSControllingBloodPressure", "bp-report-ip-out", TRIGGERED_ONLY),
        new TestCaseInfo(
            "ChronicDSControllingBloodPressure", "bp-report-numer-in", REPORTED, 1, 1, 0, 1),
        new TestCaseInfo(
            "ChronicDSControllingBloodPressure", "bp-report-numer-out", REPORTED, 1, 1, 0, 0),
        new TestCaseInfo(
            "ChronicDSControllingBloodPressure", "bp-trigger-condition-in", TRIGGERED_ONLY),
        new TestCaseInfo(
            "ChronicDSControllingBloodPressure", "bp-trigger-condition-missing", NOT_TRIGGERED),
        new TestCaseInfo(
            "ChronicDSControllingBloodPressure", "bp-trigger-condition-out", NOT_TRIGGERED),
        new TestCaseInfo("ChronicDSControllingBloodPressure", "bp-trigger-in", TRIGGERED_ONLY),
        new TestCaseInfo("ChronicDSControllingBloodPressure", "bp-trigger-out", NOT_TRIGGERED));
  }
}
