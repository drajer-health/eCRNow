package com.drajer.sof.service;

import java.util.Date;

import com.drajer.sof.model.FhirData;

public class TriggerQueryService implements AbstractQueryService {

	@Override
	public FhirData getData(String patId, String encId, Date start, Date end) {
		
		
		// TODO Auto-generated method stub
		
				// Access the Token Details and pull the data based on service
		
				// Based on the version of EMR, create either Dstu2FhirData or STU3 or R4.
				
				// Data to be pulled.
				
				// Get Encounters for Patient based on encId. (Create a method to get encounters)
				// If encId is null, find encounters within the start and end time provided.
				// Add to the bundle.
				// As you are adding to the bundle within Fhir Data, add the codeable concept also to the list of encounterCodes.

				// Get Conditions for Patient (Write a method)
				// Filter the conditions based on encounter Reference if Encounter Reference is present.
				// If encounter is not present, then filter based on times (Start and end, if Condition time is between start and end times) -- Do this later.
				// Add to the bundle
				// As you are adding to the bundle within Fhir Data, add the codeable concept also to the list of ConditionCodes.

				
				// Get Observations for Patients and laboratory category (Write a method).
				// Filter the observations based on encounter Reference if encounter is present.
				// If encounter is not present, then filter based on times (Start and end, if observation time is between start and end times) -- Do this later.
				// Add to the bundle
				// As you are adding to the bundle within Fhir Data, add the codeable concept also to the list of labResultCodes.

		
				// Get MedicationAdministration for Patients and laboratory category (Write a method).
				// Filter the MedicationAdministrations based on encounter Reference if encounter is present.
				// If encounter is not present, then filter based on times (Start and end, if medicationadministration time is between start and end times) -- Do this later.
				// Add to the bundle
				// As you are adding to the bundle within Fhir Data, add the codeable concept also to the list of medicationCodes.

				// Get DiagnosticOrders for Patients  (Write a method).
				// Filter the Diagnostic Orders based on encounter Reference if encounter is present.
				// If encounter is not present, then filter based on times (Start and end, if diagnostic order time is between start and end times) -- Do this later.
				// Add to the bundle
				// As you are adding to the bundle within Fhir Data, add the codeable concept also to the list of diagnosticOrderCodes.

		
		return null;
	}
}
