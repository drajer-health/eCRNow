package com.drajer.sof.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;

@Component
public class R4ResourcesData {

	@Autowired
	FhirContextInitializer resourceData;

	private List<CodeableConcept> findEncounterCodes(Encounter encounter) {
		List<CodeableConcept> encounterCodes = new ArrayList<CodeableConcept>();
		if (encounter.getType() != null) {
			encounterCodes = encounter.getType();
		}
		return encounterCodes;
	}

	public Encounter getEncounterData(FhirContext context, IGenericClient client, LaunchDetails launchDetails,
			R4FhirData r4FhirData, Date start, Date end) {
		Encounter encounter = new Encounter();
		// If Encounter Id is present in Launch Details
		if (launchDetails.getEncounterId() != null) {
			encounter = (Encounter) resourceData.getResouceById(launchDetails, client, context, "Encounter",
					launchDetails.getEncounterId());
			r4FhirData.setR4EncounterCodes(findEncounterCodes(encounter));
		} else {
			// If Encounter Id is not Present in Launch Details Get Encounters by Patient Id
			// and Find the latest Encounter
			Bundle bundle = (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context, "Encounter");
			Map<Encounter, Date> encounterMap = new HashMap<Encounter, Date>();
			for (BundleEntryComponent entry : bundle.getEntry()) {
				Encounter encounterEntry = (Encounter) entry.getResource();
				// Checking if Period element exists in Encounter. If Exists compare period is
				// falling in between Start and End Date
				if (!encounterEntry.getPeriod().isEmpty()) {
					Period period = encounterEntry.getPeriod();
					if (period.getStart().after(start) || period.getEnd().before(end)) {
						encounterMap.put(encounterEntry, encounterEntry.getMeta().getLastUpdated());
					}
					// If period is not present using LastUpdatedDate in meta information to filter
					// the Encounter
				} else {
					Date lastUpdatedDateTime = encounterEntry.getMeta().getLastUpdated();
					if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
						encounterMap.put(encounterEntry, encounterEntry.getMeta().getLastUpdated());
					}
				}
			}
			encounter = Collections.max(encounterMap.entrySet(), Map.Entry.comparingByValue()).getKey();
			r4FhirData.setR4EncounterCodes(findEncounterCodes(encounter));
		}
		return encounter;
	}

	private List<CodeableConcept> findConditionCodes(Condition condition) {
		List<CodeableConcept> conditionCodes = new ArrayList<CodeableConcept>();
		if (!condition.getCode().isEmpty() && condition.getCode() != null) {
			conditionCodes.add(condition.getCode());
		}
		return conditionCodes;
	}

	public List<Condition> getConditionData(FhirContext context, IGenericClient client, LaunchDetails launchDetails,
			R4FhirData r4FhirData, Encounter encounter, Date start, Date end) {
		Bundle bundle = (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context, "Condition");
		List<Condition> conditions = new ArrayList<>();
		List<CodeableConcept> conditionCodes = new ArrayList<CodeableConcept>();
		// Filter Conditions based on Encounter Reference
		if (!encounter.getIdElement().getValue().isEmpty() && encounter != null) {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				Condition condition = (Condition) entry.getResource();
				if (!condition.getEncounter().isEmpty()) {
					if (condition.getEncounter().getReferenceElement().getIdPart()
							.equals(encounter.getIdElement().getIdPart())) {
						conditions.add(condition);
						conditionCodes.addAll(findConditionCodes(condition));
					}
				}
			}
			// If Encounter Id is not present using start and end dates to filter conditions
		} else {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				Condition condition = (Condition) entry.getResource();
				// Checking If Date Recorded present in Condition resource
				if (condition.getRecordedDate() != null) {
					if (condition.getRecordedDate().after(start) && condition.getRecordedDate().before(end)) {
						conditions.add(condition);
						conditionCodes.addAll(findConditionCodes(condition));
					}
					// If Date Recorded is not present using LastUpdatedDate
				} else {
					Date lastUpdatedDateTime = condition.getMeta().getLastUpdated();
					if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
						conditions.add(condition);
						conditionCodes.addAll(findConditionCodes(condition));
					}
				}
			}
		}
		r4FhirData.setR4ConditionCodes(conditionCodes);
		return conditions;
	}

	private List<CodeableConcept> findLaboratoryCodes(Observation observation) {
		List<CodeableConcept> observationCodes = new ArrayList<CodeableConcept>();

		if (!observation.getCode().isEmpty() && observation.getCode() != null) {
			observationCodes.add(observation.getCode());
		}
		return observationCodes;
	}

	public List<Observation> getObservationData(FhirContext context, IGenericClient client, LaunchDetails launchDetails,
			R4FhirData r4FhirData, Encounter encounter, Date start, Date end) {
		Bundle bundle = (Bundle) resourceData.getObservationByPatientId(launchDetails, client, context, "Observation",
				"laboratory");
		List<Observation> observations = new ArrayList<>();
		List<CodeableConcept> observationCodes = new ArrayList<CodeableConcept>();
		// Filter Observations based on Encounter Reference
		if (!encounter.getIdElement().getValue().isEmpty() && encounter != null) {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				Observation observation = (Observation) entry.getResource();
				if (!observation.getEncounter().isEmpty()) {
					if (observation.getEncounter().getReferenceElement().getIdPart()
							.equals(encounter.getIdElement().getIdPart())) {
						observations.add(observation);
						observationCodes.addAll(findLaboratoryCodes(observation));
					}
				}
			}
			// If Encounter Id is not present using start and end dates to filter
			// Observations
		} else {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				Observation observation = (Observation) entry.getResource();
				// Checking If Issued Date is present in Observation resource
				if (observation.getIssued() != null) {
					if (observation.getIssued().after(start) && observation.getIssued().before(end)) {
						observations.add(observation);
						observationCodes.addAll(findLaboratoryCodes(observation));
					}
					// If Issued date is not present, Checking for Effective Date
				} else if (!observation.getEffective().isEmpty()) {
					Type effectiveDate = (Type) observation.getEffectiveDateTimeType();
					Date effDate = effectiveDate.dateTimeValue().getValue();
					if (effDate.after(start) && effDate.before(end)) {
						observations.add(observation);
						observationCodes.addAll(findLaboratoryCodes(observation));
					}
					// If Issued and Effective Date are not present looking for LastUpdatedDate
				} else {
					Date lastUpdatedDateTime = observation.getMeta().getLastUpdated();
					if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
						observations.add(observation);
						observationCodes.addAll(findLaboratoryCodes(observation));
					}
				}
			}
		}
		r4FhirData.setR4LabResultCodes(observationCodes);
		return observations;
	}

	private List<CodeableConcept> findMedicationCodes(MedicationAdministration medAdministration) {
		List<CodeableConcept> medicationCodes = new ArrayList<CodeableConcept>();

		if (!medAdministration.getMedication().isEmpty() && medAdministration.getMedication() != null) {
			if (medAdministration.getMedication() instanceof CodeableConcept) {
				// Handle Codeable Concept
				CodeableConcept medicationCode = (CodeableConcept) medAdministration.getMedication();
				medicationCodes.add(medicationCode);
			} else {
				// Handle Reference data types
			}
		}
		return medicationCodes;
	}

	public List<MedicationAdministration> getMedicationAdministrationData(FhirContext context, IGenericClient client,
			LaunchDetails launchDetails, R4FhirData r4FhirData, Encounter encounter, Date start, Date end) {
		Bundle bundle = (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context,
				"MedicationAdministration");
		List<MedicationAdministration> medAdministrations = new ArrayList<>();
		List<CodeableConcept> medicationCodes = new ArrayList<CodeableConcept>();
		// Filter MedicationAdministrations based on Encounter Reference
		if (bundle != null && !encounter.getIdElement().getValue().isEmpty() && encounter != null) {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				MedicationAdministration medAdministration = (MedicationAdministration) entry.getResource();
				if (!medAdministration.getContext().isEmpty()) {
					if (medAdministration.getContext().getReferenceElement().getIdPart()
							.equals(encounter.getIdElement().getIdPart())) {
						medAdministrations.add(medAdministration);
						medicationCodes.addAll(findMedicationCodes(medAdministration));
					}
				}
			}
			// If Encounter Id is not present using start and end dates to filter
			// MedicationAdministrations
		} else if (bundle != null) {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				MedicationAdministration medAdministration = (MedicationAdministration) entry.getResource();
				// Checking If Effective Date is present in MedicationAdministration resource
				if (medAdministration.getEffectiveDateTimeType() != null) {
					Type effectiveDateTime = (Type) medAdministration.getEffectiveDateTimeType();
					Date effDate = effectiveDateTime.dateTimeValue().getValue();
					if (effDate.after(start) && effDate.before(end)) {
						medAdministrations.add(medAdministration);
						medicationCodes.addAll(findMedicationCodes(medAdministration));
					}
				}
				// If Effective Date is not present looking for LastUpdatedDate
				else {
					Date lastUpdatedDateTime = medAdministration.getMeta().getLastUpdated();
					if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
						medAdministrations.add(medAdministration);
						medicationCodes.addAll(findMedicationCodes(medAdministration));
					}
				}
			}
		}
		r4FhirData.setR4MedicationCodes(medicationCodes);
		return medAdministrations;
	}

	private List<CodeableConcept> findDiagnosticReportCodes(DiagnosticReport diagnosticReport) {
		List<CodeableConcept> diagnosticReportCodes = new ArrayList<CodeableConcept>();

		if (!diagnosticReport.getCode().isEmpty() && diagnosticReport.getCode() != null) {
			diagnosticReportCodes.add(diagnosticReport.getCode());
		}
		return diagnosticReportCodes;
	}

	public List<DiagnosticReport> getDiagnosticReportData(FhirContext context, IGenericClient client,
			LaunchDetails launchDetails, R4FhirData r4FhirData, Encounter encounter, Date start, Date end) {
		Bundle bundle = (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context,
				"DiagnosticReport");
		List<DiagnosticReport> diagnosticReports = new ArrayList<>();
		List<CodeableConcept> diagnosticReportCodes = new ArrayList<CodeableConcept>();
		// Filter DiagnosticReports based on Encounter Reference
		if (!encounter.getIdElement().getValue().isEmpty() && encounter != null) {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				DiagnosticReport diagnosticReport = (DiagnosticReport) entry.getResource();
				if (!diagnosticReport.getEncounter().isEmpty()) {
					if (diagnosticReport.getEncounter().getReferenceElement().getIdPart()
							.equals(encounter.getIdElement().getIdPart())) {
						diagnosticReports.add(diagnosticReport);
						diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
					}
				}
			}
			// If Encounter Id is not present using start and end dates to filter
			// DiagnosticOrders
		} else {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				DiagnosticReport diagnosticReport = (DiagnosticReport) entry.getResource();
				// Checking If Issued Date is present in Observation resource
				if (diagnosticReport.getIssued() != null) {
					if (diagnosticReport.getIssued().after(start) && diagnosticReport.getIssued().before(end)) {
						diagnosticReports.add(diagnosticReport);
						diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
					}
					// If Issued date is not present, Checking for Effective Date
				} else if (!diagnosticReport.getEffective().isEmpty()) {
					Type effectiveDate = (Type) diagnosticReport.getEffective();
					Date effDate = effectiveDate.dateTimeValue().getValue();
					if (effDate.after(start) && effDate.before(end)) {
						diagnosticReports.add(diagnosticReport);
						diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
					}
					// If Issued and Effective Date are not present looking for LastUpdatedDate
				} else {
					Date lastUpdatedDateTime = diagnosticReport.getMeta().getLastUpdated();
					if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
						diagnosticReports.add(diagnosticReport);
						diagnosticReportCodes.addAll(findDiagnosticReportCodes(diagnosticReport));
					}
				}
			}
		}
		r4FhirData.setR4DiagnosticReportCodes(diagnosticReportCodes);
		return diagnosticReports;
	}

	private List<CodeableConcept> findImmunizationCodes(Immunization immunization) {

		List<CodeableConcept> immunizationCodes = new ArrayList<CodeableConcept>();
		if (!immunization.getVaccineCode().isEmpty() && immunization.getVaccineCode() != null) {
			immunizationCodes.add(immunization.getVaccineCode());
		}

		return immunizationCodes;
	}

	public List<Immunization> getImmunizationData(FhirContext context, IGenericClient client,
			LaunchDetails launchDetails, R4FhirData r4FhirData, Encounter encounter, Date start, Date end) {
		Bundle bundle = (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context, "Immunization");
		List<Immunization> immunizations = new ArrayList<>();
		List<CodeableConcept> immunizationCodes = new ArrayList<CodeableConcept>();
		// Filter Immunizations based on Encounter Reference
		if (!encounter.getIdElement().getValue().isEmpty() && encounter != null) {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				Immunization immunization = (Immunization) entry.getResource();
				if (!immunization.getEncounter().isEmpty()) {
					if (immunization.getEncounter().getReferenceElement().getIdPart()
							.equals(encounter.getIdElement().getIdPart())) {
						immunizations.add(immunization);
						immunizationCodes.addAll(findImmunizationCodes(immunization));
					}
				}
			}
			// If Encounter Id is not present using start and end dates to filter
			// Immunizations
		} else {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				Immunization immunization = (Immunization) entry.getResource();
				// Checking If Immunization DateTime is present in Immunization
				// resource
				if (immunization.getOccurrenceDateTimeType() != null) {
					if (immunization.getOccurrenceDateTimeType().dateTimeValue().getValue().after(start)
							&& immunization.getOccurrenceDateTimeType().dateTimeValue().getValue().before(end)) {
						immunizations.add(immunization);
						immunizationCodes.addAll(findImmunizationCodes(immunization));
					}
				}
				// If Immunization Date is not present looking for LastUpdatedDate
				else {
					Date lastUpdatedDateTime = immunization.getMeta().getLastUpdated();
					if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
						immunizations.add(immunization);
						immunizationCodes.addAll(findImmunizationCodes(immunization));
					}
				}
			}
		}
		r4FhirData.setR4ImmunizationCodes(immunizationCodes);
		return immunizations;
	}
	
	private List<CodeableConcept> findServiceRequestCodes(ServiceRequest serviceRequest) {
		List<CodeableConcept> serviceRequestCodes = new ArrayList<CodeableConcept>();
		if (!serviceRequest.getCode().isEmpty() && serviceRequest.getCode() != null) {
			serviceRequestCodes.add(serviceRequest.getCode());
		}
		return serviceRequestCodes;
	}

	public List<ServiceRequest> getServiceRequestData(FhirContext context, IGenericClient client,
			LaunchDetails launchDetails, R4FhirData r4FhirData, Encounter encounter, Date start, Date end) {
		Bundle bundle = (Bundle) resourceData.getResourceByPatientId(launchDetails, client, context, "ServiceRequest");
		List<ServiceRequest> serviceRequests = new ArrayList<>();
		List<CodeableConcept> serviceRequestCodes = new ArrayList<CodeableConcept>();
		// Filter ServiceRequests based on Encounter Reference
		if (!encounter.getIdElement().getValue().isEmpty() && encounter != null) {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();
				if (!serviceRequest.getEncounter().isEmpty()) {
					if (serviceRequest.getEncounter().getReferenceElement().getIdPart()
							.equals(encounter.getIdElement().getIdPart())) {
						serviceRequests.add(serviceRequest);
						serviceRequestCodes.addAll(findServiceRequestCodes(serviceRequest));
					}
				}
			}
			// If Encounter Id is not present using start and end dates to filter
			// ServiceRequests
		} else {
			for (BundleEntryComponent entry : bundle.getEntry()) {
				ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();
				// Checking If Immunization DateTime is present in Immunization
				// resource
				if (serviceRequest.getOccurrenceDateTimeType() != null) {
					if (serviceRequest.getOccurrenceDateTimeType().dateTimeValue().getValue().after(start)
							&& serviceRequest.getOccurrenceDateTimeType().dateTimeValue().getValue().before(end)) {
						serviceRequests.add(serviceRequest);
						serviceRequestCodes.addAll(findServiceRequestCodes(serviceRequest));
					}
				}
				// If ServiceRequest Date is not present looking for LastUpdatedDate
				else {
					Date lastUpdatedDateTime = serviceRequest.getMeta().getLastUpdated();
					if (lastUpdatedDateTime.after(start) && lastUpdatedDateTime.before(end)) {
						serviceRequests.add(serviceRequest);
						serviceRequestCodes.addAll(findServiceRequestCodes(serviceRequest));
					}
				}
			}
		}
		r4FhirData.setR4ServiceRequestCodes(serviceRequestCodes);
		return serviceRequests;
	}

}
