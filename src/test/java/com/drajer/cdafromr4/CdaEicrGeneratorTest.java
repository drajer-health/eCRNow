package com.drajer.cdafromr4;

import static org.junit.jupiter.api.Assertions.*;

import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.io.File;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Type;
import org.junit.jupiter.api.Test;

class CdaEicrGeneratorTest {

  @Test
  void test() {

    ArrayList<?> a = new ArrayList<>();
    // Create a Test Launch Details.
    LaunchDetails ld = getLaunchDetails();

    // Load a Bundle for debugging.
    // R4FhirData data = getFhirData();

    CdaHeaderGenerator.getPatientDetails(getPatientData(), ld);
  }

  public R4FhirData getFhirData() {

    String resourceName = "LoadingQueryR4Bundle.json";

    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(resourceName).getFile());
    String absolutePath = file.getAbsolutePath();

    R4FhirData data = new R4FhirData();

    ApplicationUtils ap = new ApplicationUtils();

    Bundle bund = ap.readBundleFromFile(absolutePath);
    data.setData(bund);

    return data;
  }

  public Patient getPatientData() {

    Patient p = new Patient();

    p.setId("5474974");

    Extension ext1 = new Extension();
    ext1.setUrl("http://hl7.org/fhir/us/core/StructureDefinition/us-core-race");

    Extension subext1 = new Extension();
    subext1.setUrl("ombCategory");

    Coding st1 = new Coding();
    st1.setSystem("http://hl7.org/fhir/v3/NullFlavor");
    st1.setCode("UNK");
    st1.setDisplay("Unknown");

    Type tp1 = (Type) st1;
    subext1.setValue(tp1);
    ext1.addExtension(subext1);

    Extension ext = new Extension();
    ext.setUrl("http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity");
    Extension subext = new Extension();
    subext.setUrl("text");

    StringType st = new StringType("Unavailable");
    Type tp = (Type) st;
    subext.setValue(tp);
    ext.addExtension(subext);

    p.addExtension(ext1);
    p.addExtension(ext);

    return p;
  }

  public LaunchDetails getLaunchDetails() {

    LaunchDetails ld = new LaunchDetails();

    ld.setId(1);
    ld.setClientId("DummyClient");
    ld.setClientSecret("DummySecret");
    ld.setEhrServerURL("DummyServerUrl");
    ld.setAuthUrl("DummyAuthUrl");
    ld.setTokenUrl("DummyTokenUrl");
    ld.setAccessToken("DummyAccessToken");
    ld.setUserId("DummyUser");
    ld.setExpiry(60);
    ld.setScope("DummyScope");
    ld.setLastUpdated(Date.from(Instant.now()));
    ld.setStartDate(Date.from(Instant.now()));
    ld.setEndDate(Date.from(Instant.now()));
    ld.setRefreshToken("DummyRefreshToken");
    ld.setLaunchPatientId("1234");
    ld.setFhirVersion("4.0.1");
    ld.setEncounterId("5678");
    ld.setStatus("active");
    ld.setAssigningAuthorityId("2.16.840.1.113883.1.1.1.1.1");
    ld.setSetId("1234" + "|" + "5678");
    ld.setVersionNumber(1);
    ld.setDirectHost("ett.healthit.gov");
    ld.setDirectUser("test@ett.healthit.gov");
    ld.setDirectPwd("password");
    ld.setSmtpPort("25");
    ld.setImapPort("443");
    ld.setDirectRecipient("connectathon@aimsplatform.org");
    ld.setRestAPIURL("DummyRestApiUrl");
    ld.setIsCovid(true);
    ld.setLaunchId("DummyLaunchId");
    ld.setLaunchState(1);
    ld.setRedirectURI("DummyRedirectUri");
    ld.setIsSystem(true);
    ld.setDebugFhirQueryAndEicr(true);

    return ld;
  }
}
