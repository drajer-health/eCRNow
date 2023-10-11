package com.drajer.cdafromdstu2;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cdafromr4.CdaFhirUtilities;

import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.composite.QuantityDt;

public class Dstu2CdaFhirUtilitiesTest {
	
	@Before
	  public void setUp() {
	  }

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

}
