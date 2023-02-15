FROM maven:3.8.1-adoptopenjdk-11 AS builder
WORKDIR /build
COPY pom.xml .
RUN mvn -U dependency:resolve dependency:resolve-plugins

COPY src src
RUN mvn -U package -Dskip.unit.tests=true -Dskip.integration.tests=true

FROM tomcat as appserver
RUN sed -i 's/port="8080"/port="8081"/' ${CATALINA_HOME}/conf/server.xml
EXPOSE 8081
RUN mkdir /app-artifacts
COPY documents/app-artifacts /app-artifacts

RUN mkdir /schema
COPY src/test/resources/AppData/Schema /schema

VOLUME /config
VOLUME /output
VOLUME /logs

RUN rm -fr /usr/local/tomcat/webapps/ROOT.war
COPY --from=builder /build/target/ecr-now.war /usr/local/tomcat/webapps/ROOT.war