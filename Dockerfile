FROM alpine:3.16.0

RUN apk --no-cache add maven && mvn --version

RUN apk add --no-cache java-cacerts openjdk17


WORKDIR /java-app

COPY pom.xml .
COPY src ./src

# Package the Spring Boot application, skipping tests
RUN mvn clean install  -Dmaven.test.skip=true -Djava.version=17

# Define the command to run your application
CMD ["java", "-jar", "./target/ecr-now.war"]
