FROM maven:3.9.3-amazoncorretto-11 AS builder
WORKDIR /build
COPY pom.xml .
RUN mvn -U dependency:resolve dependency:resolve-plugins

COPY src src
RUN mvn -U package -Dskip.unit.tests=true -Dskip.integration.tests=true

ARG JDBC_USERNAME
ARG JDBC_PASSWORD
ARG SECURITY_KEY
ARG FHIR_URL
# ENV JDBC_USERNAME=JDBC_USERNAME

FROM amazoncorretto:17

VOLUME /config
VOLUME /output
VOLUME /logs
RUN mkdir -p /usr/ecrnow/lib
WORKDIR /usr/ecrnow

COPY --from=builder /build/target/ecr-now.war lib
COPY kars kars
COPY eRSDv2.json .

EXPOSE 8080

ENTRYPOINT java -Dsecurity.key=$SECURITY_KEY -jar lib/ecr-now.war