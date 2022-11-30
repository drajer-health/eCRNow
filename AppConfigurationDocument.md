# 1. Introduction #

The eCRNow App can be configured for a healthcare setting following the instructions in the document 

https://github.com/drajer-health/eCRNow/blob/Release-3.0/documents/eCR%20Now%20App%20Configuration%20Guide_Release3.0.docx

# 2. Application.Properties available for configuration #

This section describes configuration properties typically performed for all health care settings and are not something that would be changed 
through the UI or APIs.

## 2.1 DataBase configuration ##

By default the eCRNow App uses the postgresql database, however it can be changed using the below properties to suit your environment.

jdbc.driverClassName=org.postgresql.Driver
jdbc.url=jdbc:postgresql://localhost:5432/postgres
hibernate.dialect=org.hibernate.dialect.PostgreSQL95Dialect
hibernate.show_sql=false
hibernate.format_sql=false
hibernate.hbm2ddl.auto=update

## 2.2 Changing Default Port of the App ##

Use the property below to change the default port of the app from 8081 to another port.

server.port=8081

## 2.3 Changing eCRNow Log File location ##

Use the property below to change the log file location.

logging.file.name=//users//nbashyam//Downloads//ecrNow.log

## 2.4 Configuring Schema and Schematron Validation ##

Use the property below to configure the schema validation.

xsd.schemas.location=//users//nbashyam//Downloads//schemas//CDA_SDTC.xsd

Use the property below to configure the schematron validation.

schematron.file.location=//users//nbashyam//Downloads//CDAR2_IG_PHCASERPT_R2_STU1.1_SCHEMATRON.sch

## 2.5 Security Configuration for protecting your RESTful endpoints and the eCRNow APIs ##

There are many instances where eCRNow may call a RESTful endpoint hosted by the EHR vendor to hand-off an eICR for submission 
or for handling a Reportability Response. In these cases the EHR vendor may require a specific authorization token to invoke the API.
In order to embed the required authorization token, the mechanisms identified in security configuration should be followed.

Similarly there are instructions in the security configuration section on how to protect the eCRNow API endpoints with appropriate security tokens from your authorization server.

## 2.6 Configuring number of retries for a timer during execution ##

There are instances where a timer execution may fail and it needs to be retried.
The number of retries can be configured using the property 

timer.retries=2

If the timers fail repeatedly, the timer will be removed from execution after the retry counts are exhausted.

## 2.7 Configuring number of parallel eCRNow threads in a container ##

In order to process more patients in container based on timers, the following property can be adjusted. 
In this case, a default of 10 parallel patients can be processed by the expiring timers within the container.
 
db-scheduler.threads=10

## 2.8 Specifying the directory to save generated artifacts such as eICRs, TriggerQuery and Loading Query JSON files for debugging ##

This is controlled by the bsa.output.directory property.

You also have to set the save.debug.files=true if you want the files to be saved. If this is set to false, the files would not be saved.

## 2.9 Throttling configuration ## 

Sometimes the EHR server may be busy and cannot service requests from the eCRNow App. In order to avoid failures because the server is busy you can enable throttling of the eCRNow App.
Follow the Throttling instructions to configure the app.

## 2.10 Custom Queries ## 

By default, the eCRNow app executes default queries present in the ERSD files. The default queries are based on 
US Core 3.1.1 and may not be the most efficient way of getting data out from the EHRs. 

For e.g the default query to retrieve Conditions for a Patient is /Condition?subject=Patient/123 which retrieves all Conditions.
However if you want to configure a query that removes the inactive or resolved conditions you can do something like :

/Condition?patient=Patient/{{context.patientId}}&clinical-status=http://terminology.hl7.org/CodeSystem/condition-clinical|active

Read custom query configuration to further understand how to configure custom queries.

## 2.11 Off Hour Configuration ##

EHRs are used by Providers as part of the care delivery operations and there may be large amount of load on the EHRs during the day time and there may be a desire
to reduce the load during the working hours or peak usage hours. 
In these cases, it is beneficial if the eCRNow App can be configured to only process patients whose data has to be reported immediately and delay the processing of the other patients to 
off hours (non peak hours). This kind of configuration can be achieved by the OffHour Configuration.


