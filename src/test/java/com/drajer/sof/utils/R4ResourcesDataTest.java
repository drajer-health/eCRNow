package com.drajer.sof.utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.drajer.test.util.TestUtils;
import java.util.Date;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DocumentReference;
import org.junit.Before;
import org.junit.Test;

public class R4ResourcesDataTest {

  @Before
  public void setUp() {}

  @Test
  public void constructR4DocumentReferenceTest() {
    R4ResourcesData dataObject = new R4ResourcesData();
    String rrXml = TestUtils.getFileContentAsString("R4/Misc/TestRRXml.xml");
    DocumentReference result =
        dataObject.constructR4DocumentReference(
            rrXml, "P123456", "E98765", "1225652472001060", "text/xml");
    assertNotNull(result);
  }

  @Test
  public void testIsConditionActive() {

    R4ResourcesData dataObject = new R4ResourcesData();
    Condition cond1 = new Condition();

    DateTimeType d = new DateTimeType(new Date((System.currentTimeMillis() - 10000)));
    cond1.setAbatement(d);

    assertFalse(dataObject.isConditionActive(cond1));

    cond1.setAbatement(new DateTimeType(new Date((System.currentTimeMillis() + 10000))));

    assertTrue(dataObject.isConditionActive(cond1));
  }
}
