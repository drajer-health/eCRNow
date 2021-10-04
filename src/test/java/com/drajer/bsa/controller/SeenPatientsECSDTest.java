package com.drajer.bsa.controller;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import java.util.Arrays;
import java.util.Collection;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.springframework.test.context.TestPropertySource;
import org.junit.Test;

@RunWith(Parameterized.class)
@TestPropertySource(properties = {"kar.directory=src/test/resources/Bsa/Scenarios/kars/seenpatients"})
public class SeenPatientsECSDTest extends BaseKarsTest {
  protected FhirContext fhirContext = FhirContext.forCached(FhirVersionEnum.R4);

  public SeenPatientsECSDTest(TestCaseInfo testCaseInfo) {
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
  // 2. The Bundle contains a Measure report (TODO: validate that the other resources are present)
  // 3. The MeasureReport has the correct info for the test case
  @Parameters(name = "{0}")
  public static Collection<TestCaseInfo> data() {
    return Arrays.asList(
        new TestCaseInfo("ChronicDSSeenPatients", "seen-patients-no-trigger-in", false, 0), // TODO: Should trigger = false
        new TestCaseInfo(
            "ChronicDSSeenPatients",
            "seen-patients-reported-ip-mp-in-high",
            false,
            1), // TODO: Should trigger = false
        new TestCaseInfo(
            "ChronicDSSeenPatients", "seen-patients-reported-ip-mp-in-low", true, 1),
        new TestCaseInfo(
            "ChronicDSSeenPatients",
            "seen-patients-reported-ip-mp-in-mid",
            true,
            1),
        new TestCaseInfo(
            "ChronicDSSeenPatients",
            "seen-patients-reported-ip-mp-out-post",
            false,
            0), 
        new TestCaseInfo(
            "ChronicDSSeenPatients",
            "seen-patients-reported-ip-mp-out-prior",
            false,
            0),
        new TestCaseInfo(
            "ChronicDSSeenPatients", "seen-patients-sde-in", true, 1),
        new TestCaseInfo(
            "ChronicDSSeenPatients", "seen-patients-trigger-in", false, 1));
  }
}
