# 1. eCR Now Background
The purpose of the eCR Now FHIR App is to enable EHR companies that have not had electronic case reporting (eCR) capabilites to support  public health reporting for COVID-19. The App will be able to also support full eCR, but implementation will be dependent on public health agency readiness.
For more information on electronic case reporting please refer to the following links.

* [More information on electronic case reporting (eCR)](https://ecr.aimsplatform.org)

* [More background information on ecrNow FHIR App](https://ecr.aimsplatform.org/ecr-now-fhir-app)

# 2. App Architecture # 
The app is architected as a Java Spring Boot app using Java Spring framework for the application layer, Hibernate and PostgreSQL for the backend technologies and ReactJS for the front end. The app can be instantiated as a micro service and containerized using Docker or other technologies for deployments.

The app components are as described below:

## 2.1 SMART on FHIR Module: ##
This module has the ability to collect patient and encounter context for the eCR reporting via SMART on FHIR Launch currently. Other launch mechanisms to collect the necessary context are being explored with EHR companies. The module authenticates using SMART on FHIR OAuth protocols. Once authenticated and authorized the module interacts with the EHR and queries for data that are needed to determine if a patient report for COVID reporting should be triggered. This is done by a set of queries called “Trigger Queries”. Once a patient qualifies and an eICR has to be created, the module queries the EHR for the necessary data to create the eICR. The queries used to collect data for creating an eICR are called “Loading Queries”. Currently the app supports FHIR DSTU2 and the queries used are based on the [Argonaut Data Query Implementation Guide Version 1.0.0](https://www.fhir.org/guides/argonaut/r2/). The app also supports FHIR Release 4 and the queries are based on [US Core STU3 Release 3.1.1 based on FHIR R4](
This module has the ability to collect patient and encounter context for the eCR reporting via SMART on FHIR Launch currently. Other launch mechanisms to collect the necessary context are being explored with EHR companies. The module authenticates using SMART on FHIR OAuth protocols. Once authenticated and authorized the module interacts with the EHR and queries for data that are needed to determine if a patient report for COVID reporting should be triggered. This is done by a set of queries called “Trigger Queries”. Once a patient qualifies and an eICR has to be created, the module queries the EHR for the necessary data to create the eICR. The queries used to collect data for creating an eICR are called “Loading Queries”. Currently the app supports FHIR DSTU2 and FHIR R4. The FHIR DSTU2 queries are based on the [Argonaut Data Query Implementation Guide Version 1.0.0](https://www.fhir.org/guides/argonaut/r2/). The FHIR Release 4 queries are based on [US Core STU3 Release 3.1.1 based on FHIR R4](https://www.hl7.org/fhir/us/core/).

## 2.2 ERSD Module: ##
This module ingests the trigger code definitions and timing parameter definitions for eCR reporting. These definitions are published using the Electronic Reporting and Surveillance Distribution (eRSD) transactions and profiles which are outlined and described in the [eCR FHIR IG version 1.0.0](https://hl7.org/fhir/us/ecr/). The ERSD module uses FHIR Release 4.

## 2.3 ECA Module: ##
The ECA module implements a very primitive light weight Event, Condition, Action model to handle the different events in the eCR reporting workflows. The current workflows handle the following events and actions. 
* Matching Trigger Codes
* Create eICR
* Close Out eICR 
* Validate eICR
* Submit eICR

## 2.4 HL7 CDA Generator Module: ##
The HL7 CDA Generator module is a very simple set of transforms to create a eICR report following the [HL7 CDA® R2 Implementation Guide: Public Health Case Report, Release 2: the Electronic Initial Case Report (eICR), Release 1, STU Release 1.1 - US Realm](https://www.hl7.org/implement/standards/product_brief.cfm?product_id=436). The reason for using the above version is to leverage the existing infrastructure that is being used by the AIMS APHL shared services platform, the CSTE / CDC RCKMS decision support engine and the infrastructure at, and connecting, the public health agencies. In the future as we migrate to newer versions of the implementation guides including FHIR output as well. 

## 2.5 Routing Module: ##
The routing module is used to submit/transmit the eICR created to the public health agencies. Currently the Direct Transport is integrated into the app. Future versions will add other modes of submission which may include IHE XDR, HL7 FHIR among others. 

# 3. Build and Deploy #

## 3.1 Pre-Requisites: ##
The following technologies should have been installed on your machine where you will build, test and deploy your applications.

* Java 8 or higher.
* PostgresSQL Database 10.x or higher.
* NodeJS 12.4.1 or above
* Maven 3.3.x or higher.
* git tool.

## 3.2 Steps to Build and Run the App: ##

1. Clone the repository

```git clone https://github.com/drajer-health/eCRNow.git```

2. Create a PostgresSQL database that you will use for the project.

```$ createdb -h <host> -p <port> -U <user> <database_name>```
  You can also use a tool like pgAdmin that gives a nice user interface to create the database.
  
3. Configuration Changes: 

**Backend:** 

File: src/main/resources/application.properties

Change the database user name, password to reflect the database that was created in step 2 above. 

*ERSD FILE LOCATION*

Change the ERSD File location to reflect where to find the eRSD file that contains the trigger code definitions.
The ERSD file can be downloaded by registering for an account at : https://ersd.aimsplatform.org 
Currently this link only works state-side and if you are out of country, please contact us to figure out how we can get you a copy of it through the right means.

*SCHEMATRON FILE LOCATION*

Change the Schematron File location to reflect where to find the actual eICR Schematron.
The schematron file can be downloaded from https://gforge.hl7.org/gf/project/pher/scmsvn/?action=browse&path=%2Ftrunk%2FPHCASERPT_eICR%2Fschematron%2F with the appropriate HL7 authorization. 

*SCHEMA FILE LOCATION*

Change the Schematron File location to reflect where to find the actual CDA Schema which has SDTC extensions. The schema has to be downloaded from HL7 as part of the CDA.
The attribute ```xsd.schemas.location``` has to be set to the CDA XSD with SDTC extensions. 

Change the logfile location to reflect where you want to log the data.

4. Build the App by running the following maven command.

```mvn clean install```

5. Run the App using the following command from the eCRNow project root directory.

```java -Djdbc.username=postgres -Djdbc.password=postgres -Dsecurity.key=test123 -jar ./target/ecr-now.war```

The security.key is something that you can configure in the environment or your container approach and is used for encrypting sensitive information such as clientids, client secrets and direct transport account information in the database.

6. **App configuration for EHR server:** 

Once the App is up and running, you can access the App configuration screen by launching the URL:

http://<localhost:8081>/clientDetails

Follow the App Configuration Guide present in documents folder to configure the app before using it for testing.
Only after the app is configured, you will be able to use it for reporting.

6.1 App Launch Mechanisms

The app supports three different launch mechanisms.
1. Regular SMART on FHIR ehr launch with a UI splash page that can be customized.
2. Regular SMART on FHIR ehr launch with no UI (Headless launch) that can be configured with the backend launch and redirect URIs in the controllers.
3. API based launch along with System Account access which can be used to integrate with systems which generate patient/encounter lists throughout the day and can report on these patients/encounters at the end of the day. Once such integration uses the ADT feeds. 

## 3.3 Verification Steps: ##
To check if you have downloaded the app properly and are being able to run it, follow the steps below.

1. When the Spring Boot App starts up, it parses the eRSD File definition, and the hibernate process will create the following tables in the database, verify that these tables are present in the database.
* launch_details --- Maintains the context for each patient for reporting.
* client_details --- Maintains configuration information for each EHR server.
* eicr -- Contains each eICR created.
* reportability_response -- Contains each Reportability Response received from PHA.

This normally finishes in less than 10 seconds.

2. Verification of a SMART App Launch

Register for a client id in an EHR development sandbox and follow the eCR App Configuation step to reflect the client ids, the scopes and the app urls as described previously.
Once you launch the app with a Patient and Encounter context, the app will kick off its proessing and go through various steps and produce an eICR. 

You can verify that the eICR is produced by checking the eICR table in the database. 

**Note:** For testing purposes, we are allowing the same patient and encounter to be used multiple times to help debug, similarly the timing schedules are shortened to about 10 seconds, and the trigger code matching logic will allow the next step (Creating an eICR for testing purposes) to execute even if the test data does not match the value sets.

# 4. Production Deployment Considerations:
Organizations implementing in production settings should consider the following:
1. Properly securing the app user interface if it is used within the enterprise and protecting access.
2. Implement organization policies around database settings (ports), schemas, encryption.
3. Implement other security best practices.

