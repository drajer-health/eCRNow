# eCR Now Background
The purpose of the eCR Now FHIR App is to enable vendors to perform eCR reporting for COVID-19 for sites which do not currently have eCR reporting implemented via other mechanisms that are currently supported by the public health agencies. 

## Instructions to run this project ##

### To run this project below are the prerequisite ###
* Java 8 or higher version
* Postgres database
* Maven to build the project

### Steps to run the project ###
* Download the source code by using GIT clone or fork.
* Create database with name R4 (default database name in application.properties) or any other name
* Update src/main/resources/application.properties values accordingly
* Go to project directory and run "mvn clean install" to build the project
* Go to target directory and run "java -jar plandefinition-processor-0.0.1-SNAPSHOT.jar"


