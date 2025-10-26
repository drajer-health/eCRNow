FROM openjdk:24-ea-17-jdk-slim

# Install Maven (since slim images don't include it)
RUN apt update && apt install -y maven && mvn --version

# Set working directory
WORKDIR /java-app

# Copy only pom.xml first (to cache dependencies)
COPY pom.xml ./

# Download dependencies without compiling (caching optimization)

# Copy the application source code
COPY src ./src


# Package the Spring Boot application, skipping tests
RUN mvn clean install  -Dmaven.test.skip=true

# Define the command to run your application
CMD ["java", "-jar", "./target/ecr-now.war"]