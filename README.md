# 1. eCR Now Background
The purpose of the eCR Now FHIR App is to enable vendors to perform eCR reporting for COVID-19 for sites which do not currently have eCR reporting implemented via other mechanisms that are currently supported by the public health agencies. 

# 2. App Architecture # 
The app is architected as a Java Spring Boot app using Java Spring framework for the application layer, Hibernate and Postgres for the backend technologies and ReactJS for the front end. The app can be instantiated as a micro service and containerized using Docker or other technologies for deployments. 

The app components are as described below:

## 2.1 SMART on FHIR Module: ##
This module has the ability to collect patient and encounter context for the eCR reporting via SMART on FHIR Launch currently. Other launch mechanisms to collect the necessary context are being explored with vendors. The module authenticates using SMART on FHIR OAuth protocols. Once authenticated and authorized the module interacts with the EHR and queries for data that is needed to examine if a Patient qualifies for COVID reporting. This is done by a set of queries called Trigger Queries. Once a patient qualifies and an eICR has to be created, the module queries the EHR for the necessary data to create the eICR. The queries used to collect data for creating an eICR are called Loading Queries. 
Currently the app supports FHIR DSTU2 and the queries used are based on the [Argonaut Data Query Implementation Guide Version 1.0.0](https://www.fhir.org/guides/argonaut/r2/). FHIR STU3 and FHIR Release 4 are in the pipeline to be supported.

## 2.2 ERSD Module: ##
This module ingests the trigger code defintions, timing parameter defintions for eCR reporting. These defintions are published using the Electronic Reporting and Surveillance Distribution (ERSD) transactions and profiles which are outlined and described in the [eCR FHIR IG version 1.0.0](http://hl7.org/fhir/us/ecr/). The ERSD module uses FHIR Release 4.

## 2.3 ECA Module: ##
The ECA module implements a very primitive light weight Event, Condition, Action model to handle the different events in the eCR reporting workflows. The current workflows handle the following events and actions. 
* Matching Trigger Codes
* Create eICR
* Close Out eICR 
* Submit eICR

## 2.4 HL7 CDA Generator Module: ##
The HL7 CDA Generator module is a very simple set of transforms to create a eICR report following the [HL7 CDA® R2 Implementation Guide: Public Health Case Report, Release 2: the Electronic Initial Case Report (eICR), Release 1, STU Release 1.1 - US Realm](https://www.hl7.org/implement/standards/product_brief.cfm?product_id=436). The reason for using the above version is to leverage the existing infrastructure that is being used by public health agencies. In the future as we migrate to newer versions of the implementation guide, the module will be changed to use the transforms published by the IGs.

## 2.5 Routing Module: ##
The routing module is used to submit/transmit the eICR created to the public health agencies. Currently the Direct Transport is integrated into the app. Future versions will add other modes of submission which may include IHE XDR, HL7 FHIR among others. 

# 3. Build and Deploy #

## 3.1 Pre-Requisites: ##
The following technologies should have been installed on your machine where you will build, test and deploy your applications.

* Java 8 or higher.
* PostgresSql Database 10.x or higher.
* NodeJS 12.4.1 or above
* Maven 3.3.x or higher.
* git tool.

## 3.2 Steps to Build and Run the App: ##

1. Clone the repository

```git clone https://github.com/drajer-health/eCRNow.git```

2. Create a Postgres database that you will use for the project.

```$ createdb -h <host> -p <port> -U <user> <database_name>```
  You can also use a tool like pgAdmin that gives a nice user interface to create the database.
  
3. Configuration Changes: 

**Backend:** 

File: src/main/resources/application.properties

Change the database user name, password to reflect the database that was created in step 2 above. 

Change the ERSD File location to reflect where to find the ersd file that contains the trigger code definitions.

Change the logfile location to reflect where you want to log the data.

**Frontend:** 

File: eCRNow/frontend/public/config.js

**CLIENT_ID:** This should be the value of **Client Id** received when you registered the app with the specific EHR sandbox.

**SCOPES:** This is a comma seperated list of scopes that was selected as part of the app registration. 

**Client_EndPoint:** Update this value when the application is being deployed to a test or production server with the right URL. To run and test in local machine, you can use the existing value.

```
var CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX';
var SCOPES = 'launch,online_access,offline_access,user/Patient.read,user/Condition.read';
var Client_Endpoint = 'http://localhost:8081/';
```

File: eCRNow/frontend/.env

**PUBLIC_URL:** Update this value when the application is being deployed to a test or production server with the right URL. To run and test in local machine, you can use the existing value.

```PUBLIC_URL=http://localhost:8081```

4. Build the App by running the following maven command.

```mvn clean install```

5. Run the App using the following command from the eCRNow project root directory.

```java -jar ./target/ecr-now.jar```



