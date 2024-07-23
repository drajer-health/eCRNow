FROM openjdk:17-alpine

RUN apk --no-cache add maven && mvn --version

WORKDIR /java-app

COPY pom.xml .
COPY src ./src

# Package the Spring Boot application, skipping tests
RUN mvn clean install  -Dmaven.test.skip=true

# Define the command to run your application
CMD ["java", "-jar", "./target/ecr-now.war"]