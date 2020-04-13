# eCRNow 
The PlanDefinition Processor reads the eRSD JSON bundle and creates the valuesets on the R4 DB. The eRSD file is being downloaded and is saved in directory. The location of the file has to be specified as a parameter in application.properties file

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


