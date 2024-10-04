# Passing Java Version for Building and Running the Application

## Prerequisites

- Java Development Kit (JDK) installed on your system
- Apache Maven installed on your system
- Your Spring Boot application codebase

# Setting Java Version for Spring Boot Application

This document provides instructions for specifying the Java version during the building and running processes of your Spring Boot application.

## Building the Application

To specify the Java version during the build process, use the following Maven command with the `-D` flag:
````
mvn clean install -Djava.version=<java_version>
````
## Example

Java 8
````
mvn clean install -Djava.version=1.8
````

Java 17
````
mvn clean install -DJAVA.VERSION=17
````




## Running Spring Boot Application

This guide explains how to run a Spring Boot application and how to pass values during the runtime process.


1. Open a terminal or command prompt.

2. Navigate to the root directory of your Spring Boot application.

3. Run the following Maven command to start the application:

````
 mvn spring-boot:run -Djava.version=<java_version>
````

## Example

Java 8
````
mvn spring-boot:run -Djava.version=1.8
````

Java 17
````
mvn spring-boot:run -Djava.version=17
