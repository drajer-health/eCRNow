package com.drajer.cdafromdstu2;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cdafromr4.CdaFhirUtilities;

import ca.uhn.fhir.model.dstu2.composite.PeriodDt;

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

}
