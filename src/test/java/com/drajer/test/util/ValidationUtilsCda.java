package com.drajer.test.util;

import static org.junit.Assert.assertEquals;

import org.hl7.v3.II;
import org.hl7.v3.POCDMT000040PatientRole;
import org.hl7.v3.POCDMT000040RecordTarget;

public class ValidationUtilsCda {
	
	public static void validateRecordTarget(POCDMT000040RecordTarget expected, POCDMT000040RecordTarget actual) {
		
		if (expected.getPatientRole() != null) {
			validatePatientRole(expected.getPatientRole(), actual.getPatientRole());
		}
		
	}
	
	public static void validatePatientRole(POCDMT000040PatientRole expected, POCDMT000040PatientRole actual) {
		
		assertEquals( expected.getId().size(),actual.getId().size());
		
		for (II Id: expected.getId()) {
			
			//assertEquals(Id.e.getExtension(), )
		}
	}

}
