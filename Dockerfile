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
# ENV JDBC_PASSWORD=
# ENV SECURITY_KEY=
# ENV FHIR_URL=
#FROM tomcat as appserver
#RUN sed -i 's/port="8080"/port="8081"/' ${CATALINA_HOME}/conf/server.xml
#EXPOSE 8080
#RUN mkdir /app-artifacts
#COPY documents/app-artifacts /app-artifacts
#
#RUN mkdir /schema
#COPY src/test/resources/AppData/Schema /schema
#COPY eRSDv2.json /schema
#

#
#RUN rm -fr /usr/local/tomcat/webapps/ROOT.war
#COPY --from=builder /build/target/ecr-now.war /usr/local/tomcat/webapps/ROOT.war

FROM amazoncorretto:17

VOLUME /config
VOLUME /output
VOLUME /logs
RUN mkdir -p /usr/ecrnow/lib
WORKDIR /usr/ecrnow

COPY --from=builder /build/target/ecr-now.war lib
COPY eRSDv2.json .

EXPOSE 8080

ENTRYPOINT java -Dsecurity.key=test123 -jar lib/ecr-now.war