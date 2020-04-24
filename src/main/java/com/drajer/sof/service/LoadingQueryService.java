package com.drajer.sof.service;

import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.utils.Dstu2ResourcesData;
import com.drajer.sof.utils.FhirContextInitializer;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.model.dstu2.composite.ResourceReferenceDt;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Condition;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticReport;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.model.dstu2.resource.Immunization;
import ca.uhn.fhir.model.dstu2.resource.MedicationAdministration;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import ca.uhn.fhir.model.dstu2.resource.Patient;
import ca.uhn.fhir.model.dstu2.resource.Practitioner;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Encounter.Participant;
import ca.uhn.fhir.rest.client.api.IGenericClient;

@Component
public class LoadingQueryService implements AbstractQueryService {

	@Autowired
	FhirContextInitializer fhirContextInitializer;

	@Autowired
	Dstu2ResourcesData dstu2ResourcesData;

	private final Logger logger = LoggerFactory.getLogger(LoadingQueryService.class);

	@Override
	public FhirData getData(LaunchDetails launchDetails, Date start, Date end) {

		Bundle bundle = new Bundle();
		Dstu2FhirData dstu2FhirData = new Dstu2FhirData();
		// Access the Token Details and pull the data based on service
		// Based on the version of EMR, create either Dstu2FhirData or STU3 or R4.
		// Data to be pulled.
		if (launchDetails.getFhirVersion().equalsIgnoreCase(FhirVersionEnum.DSTU2.toString())) {
			logger.info("Initializing FHIR Context for Version::::" + launchDetails.getFhirVersion());
			FhirContext context = fhirContextInitializer.getFhirContext(launchDetails.getFhirVersion());
			logger.info("Initializing Client");
			IGenericClient client = fhirContextInitializer.createClient(context, launchDetails.getEhrServerURL(),
					launchDetails.getAccessToken());

			// GET Patient Details and Add to Bundle
			try {
				logger.info("Get Patient Data");
				Patient patient = (Patient) fhirContextInitializer.getResouceById(launchDetails, client, context,
						"Patient", launchDetails.getLaunchPatientId());
				Entry patientEntry = new Entry();
				patientEntry.setResource(patient);
				bundle.addEntry(patientEntry);
			} catch (Exception e) {
				logger.error("Error in getting Patient Data");
			}

			// Step 1: Get Encounters for Patient based on encId. (Create a method to get
			// encounters)
			// If encId is null, find encounters for patient within the start and end time
			// provided.
			// Add to the bundle.
			// As you are adding to the bundle within Fhir Data, add the codeable concept
			// also to the list of encounterCodes.
			Encounter encounter = null;
			try {
				logger.info("Get Encounter Data");
				encounter = dstu2ResourcesData.getEncounterData(context, client, launchDetails, dstu2FhirData, start,
						end);
				if(encounter.getParticipant() != null) {
					List<Participant> participants = encounter.getParticipant();
					for(Participant participant: participants) {
						if(participant.getIndividual() != null) {
							ResourceReferenceDt practitionerReference = participant.getIndividual();
							Practitioner practitioner = (Practitioner) fhirContextInitializer.getResouceById(launchDetails, client, context, "Practitioner", practitionerReference.getReference().getIdPart());
							Entry practitionerEntry = new Entry().setResource(practitioner);
							bundle.addEntry(practitionerEntry);	
						}
					}
				}
				Entry encounterEntry = new Entry().setResource(encounter);
				bundle.addEntry(encounterEntry);
			} catch (Exception e) {
				logger.error("Error in getting Encounter Data");
			}

			// Step 2: Get Conditions for Patient (Write a method)
			// Filter the conditions based on encounter Reference if Encounter Reference is
			// present.
			// If encounter is not present, then filter based on times (Start and end, if
			// Condition time is between start and end times) -- Do this later.
			// Add to the bundle
			// As you are adding to the bundle within Fhir Data, add the codeable concept
			// also to the list of ConditionCodes.
			try {
				logger.info("Get Condition Data");
				List<Condition> conditionsList = dstu2ResourcesData.getConditionData(context, client, launchDetails,
						dstu2FhirData, encounter, start, end);
				logger.info("Filtered ConditionsList---->" + conditionsList.size());
				for (Condition condition : conditionsList) {
					Entry conditionsEntry = new Entry().setResource(condition);
					bundle.addEntry(conditionsEntry);
				}
			} catch (Exception e) {
				logger.error("Error in getting Condition Data");
			}

			// Get Observations for Patients and laboratory category (Write a method).
			// Filter the observations based on encounter Reference if encounter is present.
			// If encounter is not present, then filter based on times (Start and end, if
			// observation time is between start and end times) -- Do this later.
			// Add to the bundle
			// As you are adding to the bundle within Fhir Data, add the codeable concept
			// also to the list of labResultCodes.
			try {
				logger.info("Get Observation Data");
				List<Observation> observationList = dstu2ResourcesData.getObservationData(context, client,
						launchDetails, dstu2FhirData, encounter, start, end);
				logger.info("Filtered Observations---->" + observationList.size());
				for (Observation observation : observationList) {
					Entry observationsEntry = new Entry().setResource(observation);
					bundle.addEntry(observationsEntry);
				}
			} catch (Exception e) {
				logger.error("Error in getting Observation Data");
			}

			// Get MedicationAdministration for Patients and laboratory category (Write a
			// method).
			// Filter the MedicationAdministrations based on encounter Reference if
			// encounter is present.
			// If encounter is not present, then filter based on times (Start and end, if
			// medicationadministration time is between start and end times) -- Do this
			// later.
			// Add to the bundle
			// As you are adding to the bundle within Fhir Data, add the codeable concept
			// also to the list of medicationCodes.
			try {
				logger.info("Get MedicationAdministration Data");
				List<MedicationAdministration> medAdministrationsList = dstu2ResourcesData
						.getMedicationAdministrationData(context, client, launchDetails, dstu2FhirData, encounter,
								start, end);
				logger.info("Filtered MedicationAdministration----------->" + medAdministrationsList.size());
				for (MedicationAdministration medAdministration : medAdministrationsList) {
					Entry medAdministrationEntry = new Entry().setResource(medAdministration);
					bundle.addEntry(medAdministrationEntry);
				}
			} catch (Exception e) {
				logger.error("Error in getting the MedicationAdministration Data");
			}

			// Get DiagnosticOrders for Patients (Write a method).
			// Filter the Diagnostic Orders based on encounter Reference if encounter is
			// present.
			// If encounter is not present, then filter based on times (Start and end, if
			// diagnostic order time is between start and end times) -- Do this later.
			// Add to the bundle
			// As you are adding to the bundle within Fhir Data, add the codeable concept
			// also to the list of diagnosticOrderCodes.
			/*
			 * try { logger.info("Get DiagnosticOrder Data"); List<DiagnosticOrder>
			 * diagnosticOrdersList = dstu2ResourcesData.getDiagnosticOrderData(context,
			 * client, launchDetails, dstu2FhirData, encounter, start, end);
			 * System.out.println("Filtered DiagnosticOrders----------->"
			 * +diagnosticOrdersList.size()); for(DiagnosticOrder diagnosticOrder:
			 * diagnosticOrdersList) { Entry diagnosticOrderEntry= new
			 * Entry().setResource(diagnosticOrder); bundle.addEntry(diagnosticOrderEntry);
			 * } }catch(Exception e) {
			 * logger.error("Error in getting the DiagnosticOrder Data"); }
			 */

			// Get Immunizations for Patients and laboratory category (Write a method).
			// Filter the Immunizations based on encounter Reference if encounter is
			// present.
			// If encounter is not present, then filter based on times (Start and end, if
			// Immunizations time is between start and end times) -- Do this later.
			// Add to the bundle
			try {
				List<Immunization> immunizationsList = dstu2ResourcesData.getImmunizationData(context, client,
						launchDetails, dstu2FhirData, encounter, start, end);
				logger.info("Filtered Immunizations----------->" + immunizationsList.size());
				for (Immunization immunization : immunizationsList) {
					Entry immunizationEntry = new Entry().setResource(immunization);
					bundle.addEntry(immunizationEntry);
				}
			} catch (Exception e) {
				logger.error("Error in getting the Immunization Data");
			}

			// Get Diagnostic Reports for Patients (Write a method).
			// Filter the Diagnostic Reports based on encounter Reference if encounter is
			// present.
			// If encounter is not present, then filter based on times (Start and end, if
			// diagnostic Reports time is between start and end times) -- Do this later.
			// Add to the bundle
			try {
				List<DiagnosticReport> diagnosticReportList = dstu2ResourcesData.getDiagnosticReportData(context,
						client, launchDetails, dstu2FhirData, encounter, start, end);
				logger.info("Filtered DiagnosticReports----------->" + diagnosticReportList.size());
				for (DiagnosticReport diagnosticReport : diagnosticReportList) {
					Entry diagnosticReportEntry = new Entry().setResource(diagnosticReport);
					bundle.addEntry(diagnosticReportEntry);
				}
			} catch (Exception e) {
				logger.error("Error in getting the DiagnosticReport Data");
			}

			// Setting bundle to FHIR Data
			logger.info("------------------------------CodeableConcept Codes------------------------------");
			logger.info("Encounter Codes Size=====>" + dstu2FhirData.getEncounterCodes().size());
			logger.info("Conditions Codes Size=====>" + dstu2FhirData.getConditionCodes().size());
			logger.info("Observation Codes Size=====>" + dstu2FhirData.getLabResultCodes().size());
			logger.info("Medication Codes Size=====>" + dstu2FhirData.getMedicationCodes().size());
			logger.info("Immunization Codes Size=====>" + dstu2FhirData.getImmuniationCodes().size());
			logger.info("DiagnosticReport Codes Size=====>" + dstu2FhirData.getDiagnosticReportCodes().size());
			// logger.info("DiagnosticOrders Codes
			// Size=====>"+dstu2FhirData.getDiagnosticOrderCodes().size());

			dstu2FhirData.setData(bundle);
			logger.info("Bundle Entry Size====>" + dstu2FhirData.getData().getEntry().size());

		}
		return null;
	}
}
