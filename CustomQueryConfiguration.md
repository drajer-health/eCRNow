# 1. Introduction to Custom Query and Default Query #

The eCRNow App invokes FHIR APIs to retrieve data from the EHR to examine, evaluate and create the eICR.
The APIs used by the eCRNow App are externalized and can be configured.

By default a query (API request) is provided for each data element to be extracted from the EHR as part of the ERSD file.
This query is called a "DEFAULT QUERY".

An example to extract conditions is shown below:

										"id": "conditions",
                                        "extension":
                                        [
                                            {
                                                "url": "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-fhirquerypattern-extension",
                                                "valueString": "Condition?patient=Patient/{{context.patientId}}"
                                            }
                                        ],
                                        
The eCRNow App retrieves the Condition data from the EHR and populates the conditions list using the query provided in the valueString.

However if you want to optimize the query and only retrieve Conditions which are active, the following query would be better suited. 

conditions=/Condition?patient=Patient/{{context.patientId}}&clinical-status=http://terminology.hl7.org/CodeSystem/condition-clinical|active

In this example, we are retrieving only active Condition Resources and populating the variable called conditions.

This kind of configured query is called a "CUSTOM QUERY".

The custom queries are helpful to avoid large amounts of data and limit the number of API calls. For example, you may include medication resources
with all MedicationRequest queries, or you can filter data by dates and times etc. 

# 2. Context Variables: #

In order to make custom queries work the following context variables are supported currently.

patientId of the launchPatient.
encounterId for which the patient was launched in the eCRNow App.
encounterStartDate for timeboxing the start point for the data.
encounterEndDate for timeboxing the end point for the data.

The following are some examples of queries with these context variables.

patient=/Patient/{{context.patientId}}
encounter=/Encounter/{{context.encounterId}}
conditions=/Condition?patient=Patient/{{context.patientId}}&clinical-status=http://terminology.hl7.org/CodeSystem/condition-clinical|active
labTests=/Observation?patient=Patient/{{context.patientId}}&category=http://terminology.hl7.org/CodeSystem/observation-category|laboratory&date=ge{{context.encounterStartDate}

# 3. Configuring the custom queries # 

In order to specify the custom queries, the following variable is used