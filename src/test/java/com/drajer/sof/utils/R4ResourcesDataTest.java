package com.drajer.sof.utils;

import static org.junit.Assert.assertNotNull;

import com.drajer.test.util.TestUtils;
import org.hl7.fhir.r4.model.DocumentReference;
import org.junit.Test;

public class R4ResourcesDataTest {

  @Test
  public void constructR4DocumentReferenceTest() {
    R4ResourcesData dataObject = new R4ResourcesData();
    String rrXml = TestUtils.getFileContentAsString("R4/Misc/TestRRXml.xml");
    DocumentReference result = dataObject.constructR4DocumentReference(rrXml, "P123456", "E98765");
    assertNotNull(result);
  }
}
