# 1. eCR Now Background
The purpose of the eCR Now FHIR App is to enable EHR companies that have not had electronic case reporting (eCR) capabilites to support  public health reporting for COVID-19. The App will be able to also support full eCR, but implementation will be dependent on public health agency readiness.
For more information on electronic case reporting please refer to the following links.

* [More information on electronic case reporting (eCR)](https://ecr.aimsplatform.org)

* [More background information on ecrNow FHIR App](https://ecr.aimsplatform.org/general/ecr-now-fhir-app)

# 2. App Architecture #
The app is architected as a Java Spring Boot app using Java Spring framework for the application layer, Hibernate and PostgreSQL for the backend technologies and ReactJS for the front end. The app can be instantiated as a micro service and containerized using Docker or other technologies for deployments.

The app components are as described below:

## 2.1 SMART on FHIR Module: ##
This module has the ability to collect patient and encounter context for the eCR reporting via SMART on FHIR Launch currently. Other launch mechanisms to collect the necessary context are being explored with EHR companies. The module authenticates using SMART on FHIR OAuth protocols. Once authenticated and authorized the module interacts with the EHR and queries for data that are needed to determine if a patient report should be generated. This is done by a set of queries called “Trigger Queries”. Once a patient qualifies and an eICR has to be created, the module queries the EHR for the necessary data to create the eICR. The queries used to collect data for creating an eICR are called “Loading Queries”. Currently the app supports FHIR DSTU2 and the queries used are based on the [Argonaut Data Query Implementation Guide Version 1.0.0](https://www.fhir.org/guides/argonaut/r2/). The app also supports FHIR Release 4 and the queries are based on [US Core STU3 Release 3.1.1 based on FHIR R4](
This module has the ability to collect patient and encounter context for the eCR reporting via SMART on FHIR Launch currently. Other launch mechanisms to collect the necessary context are being explored with EHR companies. The module authenticates using SMART on FHIR OAuth protocols specifically SMART on FHIR Backend Services.

## 2.2 ERSD Module: ##
This module ingests the trigger code definitions and timing parameter definitions for eCR reporting . These definitions are published using the Electronic Reporting and Surveillance Distribution (eRSD) transactions and profiles which are outlined and described in the [eCR FHIR IG version 1.0.0](https://hl7.org/fhir/us/ecr/). The ERSD module uses FHIR Release 4 and there are multiple versions of ERSD that have been released currently. The different versions are available at [ERSD files](https://ersd.aimsplatform.org/). The eCRNow App versions and their support for the various versions of the ERSD are as follows:

* ERSDv1 - Supported by all versions of the eCRNow App.
* ERSDv2 - Supported by eCRNow App version 3.1.1 and above.
* ERSDv3 - Supported by eCRNow App version 3.1.6 and above.

## 2.3 ECA Module: ##
The ECA module implements a very primitive light weight Event, Condition, Action model to handle the different events in the eCR reporting workflows. The current workflows handle the following events and actions.
* Matching Trigger Codes
* Creating eICR
* Validating eICR
* Submitting eICR

## 2.4 HL7 CDA Generator Module: ##
The HL7 CDA Generator module is a very simple set of transforms to create a eICR report following the [HL7 CDA® R2 Implementation Guide: Public Health Case Report, Release 2: the Electronic Initial Case Report (eICR), Release 1, STU Release 1.1 - US Realm](https://www.hl7.org/implement/standards/product_brief.cfm?product_id=436). The reason for using the above version is to leverage the existing infrastructure that is being used by the AIMS APHL shared services platform, the CSTE / CDC RCKMS decision support engine and the infrastructure at, and connecting, the public health agencies. The CDA Generator Module also currently suppors [CDA eICR 3.1](https://www.hl7.org/implement/standards/product_brief.cfm?product_id=436) version required by HTI1 regulation. 

## 2.5 Routing Module: ##
The routing module is used to submit/transmit the eICR created to the public health agencies. Currently the Direct Transport is integrated into the app. Future versions will add other modes of submission which may include IHE XDR, HL7 FHIR among others.

## 2.6 Properties
| Properties                                   | Description                                                                                                                                                                                                                                                                                    |
|----------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ecr.fhir.pagecount.enabled                   | Setting this property to `true` will add `_count` parameter to supported FHIR resource calls. Default value is `true`.                                                                                                                                                                         
| ecr.fhir.pagecount.value                     | This property is used only if `ecr.fhir.pagecount.enabled` is set to `true`. This property defines the number of items retreived per paging call. Default value is `500`.                                                                                                                      
| ecr.fhir.pagecount.resources                 | This property is used only if `ecr.fhir.pagecount.enabled` is set to `true`. List of resources for which `_count` parameter should be added. Resources should be sepearted by pipe character.
| ecr.fhir.query-by-period.enabled             | Setting this property to `true` will add either `date` or `_lastUpdated` parameter to supported FHIR resource calls. Encounter start date will be used for the query. Default value is `true`. `Example: date=ge2014-09-24, _lastUpdated=ge2014-09-24T00:00:00.000Z`                           
| ecr.fhir.query-by-period.uselastquerytime    | This property is used only if `ecr.fhir.query-by-period.enabled` is `true`.  This will use the last match trigger query date time instead of encounter start date durig the TrigggerQuery.                                                                                                       
| ecr.fhir.query-by-period.date.resources      | This property is used only if `ecr.fhir.query-by-period.enabled` is `true`. List of resources for which `date` parameter should be added. Resources should be sepearted by pipe character.
| ecr.fhir.query-by-period.lastupdated.resources| This property is used only if `ecr.fhir.query-by-period.enabled` is `true`. List of resources for which `_lastUpdated` parameter should be added. Resources should be sepearted by pipe character.
| ecr.fhir.query-by-encounter.enabled          | Setting this property to `true` will add `encounter` parameter to supported FHIR resources. Default value is `true`.                                                                                                                                                                           
| ecr.fhir.query-by-encounter.resources        | This property is used only if `ecr.fhir.query-by-encounter.enabled` is `true`. List of resources for which `encounter` parameter should be added. Resources should be sepearted by pipe character.
| ecr.fhir.skip.resources                      | Skips the FHIR calls for resources mentioned. Resources should be seperated by pipe character.                                                                                                                                                                                                     
| ecr.fhir.skip.triggerquery.resources         | Skips the FHIR calls for resources during Trigger query. Resources should be seperated by pipe character                                                                                                                                                                                           
| ecr.rr.processorphanrr                       | Setting this property to `true`, will enable processing of RRs to EHR which doesn't find match row in eicr table for eicr_doc_id. Default value is `false`.                                                                                                                                    
| ecr.fhir.retry.enabled                       | Setting this property to `true` will allow FHIR calls to retry on failures. Default value is `true`.                                                                                                                                                                                           
| ecrfhirretrytemplate.maxRetries              | Used when `ecr.fhir.retry.enabled` is `true`. Number of times to retry. Default value is `3`.                                                                                                                                                                                                  
| ecrfhirretrytemplate.retryWaitTimeInMillis   | Used when `ecr.fhir.retry.enabled` is `true`. Time interval between retries in millseconds. Default value is `1000` millsec.                                                                                                                                                                   
| ecrfhirretrytemplate.retryStatusCodes        | Used when `ecr.fhir.retry.enabled` is `true`. Httpstatus codes for which to retry, onlyfailure with these codes will be retried.Default values is `408, 429, 502, 503, 504, 500`.                                                                                                              
| ecrfhirretrytemplate.httpMethodTypeMap.      | Above retry properties can be set at different Httpmethod level. `Example: ecrfhirretrytemplate.httpMethodTypeMap.GET.maxRetries=3 ecrfhirretrytemplate.httpMethodTypeMap.GET.retryWaitTimeInMillis=1000 ecrfhirretrytemplate.httpMethodTypeMap.GET.retryStatusCodes=408, 429, 502, 503, 504, 500`
| longencounter.enableSuspend                  | Setting this property to `true` will suspend the long running encounter. Default value is false
| longencounter.suspendThreshold               | Used when `longencounter.enableSuspend` is `true`. Threshold day to suspend long running encounter. Default value is `45` days
| validate.eicr.cdar11                         | Allows the validation of a eICR based on CDA Release 1.1 for ECR when set to true.  
| validate.eicr.cdar31                         | Allows the validation of a eICR based on CDA Release 3.1 for ECR when set to true.  
| validate.eicr.fhir=false                     | Allows the validation of a eICR based on ECR FHIR IG when set to true.  

# 3. eCRNow-UI Project and its relationship to eCRNow:
The eCRNow-UI project and application is used to configure the eCRNow App. Although the UI is not mandatory to be used, it is preferrable as it makes it easier to configure the eCRNow App. The eCRNow-UI repository can be found here: https://github.com/drajer-health/eCRNow-UI. The instructions to build, deploy and start the eCRNow-UI is present in the eCRNow-UI project. The eCRNow App Configuration Guide is present in the eCRNow App documents folder which contains the instructions on how to configure the eCRNow App.


# 4. Build and Deploy #

## 4.1 Pre-Requisites: ##
The following technologies should have been installed on your machine where you will build, test and deploy your applications.

* Java 8 is supported by all eCRNow App releases.
* Java 17 is supported by eCRNow App Release 3.1.5 or later releases. 
* Pick a specific release from the master branch and install the JDK that you would like to use.
* PostgresSQL Database 10.x or higher.
* NodeJS 12.4.1 or above
* Maven 3.3.x or higher.
* git tool.

## 4.2 Steps to Build and Run the App: ##

1. Clone the repository

```git clone https://github.com/drajer-health/eCRNow.git```

2. Create a PostgresSQL database that you will use for the project.

```$ createdb -h <host> -p <port> -U <user> <database_name>```
  You can also use a tool like pgAdmin that gives a nice user interface to create the database.

3. Configuration Changes:

**Backend:**

File: src/main/resources/application.properties
The database name and password along with an encryption key are passed as command line arguments or environment variables as detailed in Step 5 below. 

*ERSD FILE LOCATION*

Change the ERSD File location to reflect where to find the eRSD file that contains the trigger code definitions.
The ERSD file can be downloaded by registering for an account at : https://ersd.aimsplatform.org
Currently this link only works state-side and if you are out of country, please contact us to figure out how we can get you a copy of it through the right means.


* For Release 2.X of the application, the ERSD v1 Specification Bundle should be used.
* For Release 3.1.1 and above of the eCRNow App using the systemLaunch API, the ERSD v1 Specification Bundle should be used.
* For Release 3.1.1 and above of the eCRNow App using the launchPatient API, the ERSD v2 Specification Bundle should be used.
* For Release 3.1.5 and above of the eCRNow App using the launchPatient API, either ERSD v2 or ERSD v3 Bundles can be used. 

*SCHEMATRON FILE LOCATION*

Change the Schematron File location to reflect where to find the actual eICR Schematron.
* CDA eICR R1.1 : The schematron file can be downloaded from [https://github.com/HL7/CDA-phcaserpt-1.1.1/tree/main/validation](https://github.com/HL7/CDA-phcaserpt-1.1.1/tree/main/validation)
* CDA eICR R3.1 : The schematron file can be downloaded from [https://github.com/HL7/CDA-phcaserpt-1.3.0/tree/main/validation](https://github.com/HL7/CDA-phcaserpt-1.3.0/tree/main/validation)

*SCHEMA FILE LOCATION*

Change the Schematron File location to reflect where to find the actual CDA Schema which has SDTC extensions. The schema has to be downloaded from HL7 as part of the CDA.
The attribute ```xsd.schemas.location``` has to be set to the CDA XSD with SDTC extensions.

Change the logfile location to reflect where you want to log the data.

4. Build the App by running the following maven command.
```
mvn clean install -Djava.version=<JAVA VERSION>
```

For Java 8

```
mvn clean install -Djava.version=1.8
```

For Java 17

```
mvn clean install -Djava.version=17
```

5. Run the App using the following command from the eCRNow project root directory.

```java -Djdbc.username=postgres -Djdbc.password=postgres -Dsecurity.key=test123 -Djava.version=<JAVA VERSION> -jar ./target/ecr-now.war```

The security.key is something that you can configure in the environment or your container approach and is used for encrypting sensitive information such as clientids, client secrets and direct transport account information in the database.

NOTE: If you are using Windows system to run the application, then use the command formatted like below.
```java "-Djdbc.username=postgres" "-Djdbc.password=postgres" "-Dsecurity.key=test123" "-Djava.version=<JAVA VERSION>" -jar .\target\ecr-now.war```
 
# 6.  Running the Application with SSL.

 ## Prerequisites:
Java Version: Ensure that you have the required version of Java installed.

Keystore File: You will need a valid keystore file (.p12 or .jks) for SSL encryption.

#### 6.1  Running the Application with a .p12 Keystore (Self-Signed) using the following command from the eCRNow project root directory.

```java -Djdbc.username=postgres -Djdbc.password=postgres -Dsecurity.key=test123 -Djava.version=<JAVA VERSION> -Dserver.ssl.enabled=true -Dserver.ssl.key-alias=ecrnow -Dserver.ssl.key-password=Password123@ -Dserver.ssl.key-store=c://ecr-now/ecrnow.p12 -Dserver.ssl.key-store-password=Password123@ -Dserver.ssl.key-store-type=PKCS12 -jar .\target\ecr-now.war```


#### 6.2  Running the Application with a .jks Keystore (Self-Signed) using the following command from the eCRNow project root directory.

```java -Djdbc.username=postgres -Djdbc.password=postgres -Dsecurity.key=test123 -Djava.version=<JAVA VERSION> -Dserver.ssl.enabled=true -Dserver.ssl.key-alias=ecrnow -Dserver.ssl.key-password=Password123@ -Dserver.ssl.key-store=c://ecr-now/ecrnow.jks -Dserver.ssl.key-store-password=Password123@ -Dserver.ssl.key-store-type=jks -jar .\target\ecr-now.war```


### Properties  Explanation

| **Property Name**             | **Description**                                                                                   | **Default Value** | **Example Value**                                  |
|-------------------------------|---------------------------------------------------------------------------------------------------|-------------------|----------------------------------------------------|
| `java.version`                 | Specifies the Java version required to run the application.                                      | `1.8`             | `17`                                               |
| `security.key`                 | Encryption key used to protect sensitive data like client IDs, client secrets, and transport info. | `test123`         | `mySecureKey!`                                     |
| `server.ssl.enabled`           | Enables or disables SSL for secure communication.                                                 | `false`           | `true`                                             |
| `server.ssl.key-alias`         | Alias for the key inside the keystore.                                                            | `ecrnow`          | `mykey`                                            |
| `server.ssl.key-password`      | Password for accessing the private key in the keystore.                                          | `Password123@`    | `SecurePass123`                                    |
| `server.ssl.key-store`         | Path to the keystore file containing the SSL certificate.                                        | `N/A`             | `C:\ecr-now\ecrnow.p12` or `C:\ecr-now\ecrnow.jks` |
|                               |                                                                                                   |                   |                                                    |
| `server.ssl.key-store-password`| Password for unlocking the keystore.                                                             | `Password123@`    | ``SecurePass123``                                   |
| `server.ssl.key-store-type`    | Specifies the type of keystore (e.g., JKS or PKCS12).                                            | `PKCS12`          | `JKS`, `PKCS12`                                    |

### Example Paths for Keystore Files:
#### Windows: "C:\\ecr-now\\ecrnow.p12 or  C:\\ecr-now\\ecrnow.jks"

#### Linux/macOS: "/etc/ssl/ecrnow.p12 or "/etc/ssl/ecrnow.jks"

7.**App configuration for EHR server:** 

Once the App is up and running, you can access the App configuration screen by building the eCRNowUI project and then following the instructions in that project for bringing up the UI.

Follow the App Configuration Guide present in documents folder to configure the app before using it for testing.
Only after the app is configured, you will be able to use it for reporting.

7.1 App Launch Mechanisms

The app supports three different launch mechanisms.
1. Regular SMART on FHIR ehr launch with no UI (Headless launch) that can be configured with the SMART on FHIR Backend services authorization.
3. API based launch along with System Account access which can be used to integrate with systems which generate patient/encounter lists throughout the day and can report on these patients/encounters at the end of the day. Once such integration uses the ADT feeds.

## 4.3 eCRNow App Configuration Steps: ##

The following documents can be used to configure the app based on the specific versions used.

* [eCRNow App version 2.X Configuration Guide](https://github.com/drajer-health/eCRNow/blob/master/documents/eCR%20Now%20App%20Configuration%20Guide_v2.docx)

* [eCRNow App version 3.X Configuration Guide](https://github.com/drajer-health/eCRNow/blob/master/documents/eCR%20Now%20App%20Configuration%20Guide_Release3.0.docx)

# 5. Production Deployment Considerations:
Organizations implementing in production settings should consider the following:
1. Properly securing the app user interface if it is used within the enterprise and protecting access following the [Security Guidance](https://github.com/drajer-health/eCRNow/blob/master/SecurityConfiguration.md)
2. Implement organization policies around database settings (ports), schemas, encryption.
3. Implement performance improvements using [Off Hour Scheduling](https://github.com/drajer-health/eCRNow/blob/master/OffHourConfiguration.md) and [Custom Query Configuration](https://github.com/drajer-health/eCRNow/blob/master/CustomQueryConfiguration.md).

