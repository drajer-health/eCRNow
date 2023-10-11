package com.drajer.cdafromdstu2;

import static org.junit.Assert.assertTrue;

import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.composite.QuantityDt;
import ca.uhn.fhir.model.primitive.DateDt;
import ca.uhn.fhir.model.primitive.DateTimeDt;
import com.drajer.cda.utils.CdaGeneratorConstants;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import org.junit.Before;
import org.junit.Test;

public class Dstu2CdaFhirUtilitiesTest {

  @Before
  public void setUp() {}

  @Test
  public void testGetPeriodXmlForValueElement() {

    PeriodDt dt = null;
    String elName = CdaGeneratorConstants.EFF_TIME_EL_NAME;

    String exResultLowValue = "<low nullFlavor=\"NI\"/>";
    String exResultHighValue = "<high nullFlavor=\"NI\"/>";

    String result = Dstu2CdaFhirUtilities.getPeriodXmlForValueElement(dt, elName);

    assertTrue(result.contains(exResultLowValue));
    assertTrue(result.contains(exResultHighValue));
  }

  @Test
  public void testGetStringForQuantity() {

    QuantityDt dt = new QuantityDt();
    dt.setValue(10.1);
    dt.setCode("mg");
    dt.setUnit("milligrams");

    String exResultUnit = "mg";
    String exResultValue = "10.1";

    String result = Dstu2CdaFhirUtilities.getStringForQuantity(dt);

    assertTrue(result.contains(exResultUnit));
    assertTrue(result.contains(exResultValue));

    QuantityDt dt1 = new QuantityDt();
    dt1.setValue(10.2);

    String exResultValue1 = "10.2";

    String result1 = Dstu2CdaFhirUtilities.getStringForQuantity(dt1);

    assertTrue(result1.contains(exResultValue1));

    QuantityDt dt2 = new QuantityDt();
    dt2.setValue(10.3);
    dt2.setUnit("kgs");

    String exResultValue2 = "10.3";
    String exResultUnit2 = "kgs";

    String result2 = Dstu2CdaFhirUtilities.getStringForQuantity(dt2);

    assertTrue(result2.contains(exResultValue2));
    assertTrue(result2.contains(exResultUnit2));

    QuantityDt dt3 = new QuantityDt();

    String result3 = Dstu2CdaFhirUtilities.getStringForQuantity(dt3);

    assertTrue(result3.contains(CdaGeneratorConstants.UNKNOWN_VALUE));
  }

  @Test
  public void testGetStringForIDataType() {

    DateDt dt = new DateDt();

    String inputString = "23-11-2023";
    DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy");
    Date inputDate = null;
    try {
      inputDate = dateFormat.parse(inputString);
    } catch (ParseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    dt.setValue(inputDate);

    String exResult = "11-23-2023";

    String result = Dstu2CdaFhirUtilities.getStringForIDataType(dt);

    assertTrue(result.equalsIgnoreCase(exResult));

    DateTimeDt d2 = new DateTimeDt();
    d2.setValue(inputDate);

    String result2 = Dstu2CdaFhirUtilities.getStringForIDataType(d2);

    assertTrue(result.equalsIgnoreCase(exResult));

    PeriodDt p = new PeriodDt();

    p.setStart(d2);
    p.setEnd(d2);

    String exResult2 = "11-23-2023|11-23-2023";

    String result3 = Dstu2CdaFhirUtilities.getStringForIDataType(p);

    assertTrue(result3.equalsIgnoreCase(exResult2));
  }
}
