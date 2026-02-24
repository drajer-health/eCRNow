package com.drajer.sof.utils;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.lang.reflect.Field;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;

public class R4ResourcesDataTest {

  private FhirContextInitializer resourceDataMock;

  @Before
  public void setUp() {
    resourceDataMock = mock(FhirContextInitializer.class);
  }

  @Test
  public void constructR4DocumentReferenceTest() {
    R4ResourcesData dataObject = new R4ResourcesData();
    String rrXml = TestUtils.getFileContentAsString("R4/Misc/TestRRXml.xml");
    DocumentReference result =
        dataObject.constructR4DocumentReference(
            rrXml, "P123456", "E98765", "1225652472001060", "text/xml");
    assertNotNull(result);
  }

  @Test
  public void testIsConditionActive() {

    R4ResourcesData dataObject = new R4ResourcesData();
    Condition cond1 = new Condition();

    DateTimeType d = new DateTimeType(new Date((System.currentTimeMillis() - 10000)));
    cond1.setAbatement(d);

    assertFalse(dataObject.isConditionActive(cond1));

    cond1.setAbatement(new DateTimeType(new Date((System.currentTimeMillis() + 10000))));

    assertTrue(dataObject.isConditionActive(cond1));
  }

  @Test
  public void test_observationHasSameEncounter() {
    Encounter enc = new Encounter();
    enc.setId("enc-123");
    Observation obs = new Observation();
    obs.setEncounter(new Reference("Encounter/enc-123")); // reference idPart = "enc-123"
    R4ResourcesData r4ResourcesData = new R4ResourcesData();
    Boolean result = r4ResourcesData.observationHasSameEncounter(enc, obs);
    assertTrue(result);
  }

  @Test
  public void returnsFalse_whenIdDoesNotMatch() {
    Encounter enc = new Encounter();
    enc.setId("enc-123");
    Observation obs = new Observation();
    obs.setEncounter(new Reference("Encounter/enc-999"));
    R4ResourcesData r4ResourcesData = new R4ResourcesData();
    assertFalse(r4ResourcesData.observationHasSameEncounter(enc, obs));
  }

  @Test
  public void returnsTrue_whenIssuedWithinRange() {
    Date start = daysFromNow(-2);
    Date end = daysFromNow(2);
    Observation obs = new Observation();
    obs.setIssued(now());
    R4ResourcesData r4ResourcesData = new R4ResourcesData();
    assertTrue(r4ResourcesData.isObservationWithinTimeRange(start, end, obs));
  }

  @Test
  public void returnsFalse_whenAllDatesOutsideRange() {
    Date start = daysFromNow(-10);
    Date end = daysFromNow(-5);
    Observation obs = new Observation();
    obs.setIssued(now());
    Meta meta = new Meta();
    meta.setLastUpdated(now());
    obs.setMeta(meta);
    obs.setEffective(new DateTimeType(now()));
    R4ResourcesData r4ResourcesData = new R4ResourcesData();
    assertFalse(r4ResourcesData.isObservationWithinTimeRange(start, end, obs));
  }

  @Test
  public void returnsTrue_whenEffectiveWithinRange() {
    Date start = daysFromNow(-2);
    Date end = daysFromNow(2);
    Observation obs = new Observation();
    obs.setEffective(new DateTimeType(now()));
    R4ResourcesData r4ResourcesData = new R4ResourcesData();
    assertTrue(r4ResourcesData.isObservationWithinTimeRange(start, end, obs));
  }

  @Test
  public void returnsTrue_whenLastUpdatedWithinRange() {
    Date start = daysFromNow(-2);
    Date end = daysFromNow(2);
    Observation obs = new Observation();
    Meta meta = new Meta();
    meta.setLastUpdated(now());
    obs.setMeta(meta);
    R4ResourcesData r4ResourcesData = new R4ResourcesData();
    assertTrue(r4ResourcesData.isObservationWithinTimeRange(start, end, obs));
  }

  @Test
  public void getPregnancyObservationData_whenBundleNull_returnsEmpty() throws Exception {
    R4ResourcesData dataObject = new R4ResourcesData();
    injectField(dataObject, "resourceData", resourceDataMock);

    FhirContext context = mock(FhirContext.class);
    IGenericClient client = mock(IGenericClient.class);
    LaunchDetails launchDetails = mock(LaunchDetails.class);
    R4FhirData r4FhirData = mock(R4FhirData.class);

    Encounter encounter = new Encounter();
    Date start = now();
    Date end = now();

    when(resourceDataMock.getResourceByPatientIdAndCode(
            eq(launchDetails),
            eq(client),
            eq(context),
            anyString(),
            eq(QueryConstants.PREGNANCY_CODE),
            eq(QueryConstants.LOINC_CODE_SYSTEM)))
        .thenReturn(null);

    List<Observation> result =
        dataObject.getPregnancyObservationData(
            context, client, launchDetails, r4FhirData, encounter, start, end);

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void
      getPregnancyObservationData_whenBundleHasObservations_returnsFilteredList_withoutCallingPrivateMethods()
          throws Exception {
    R4ResourcesData dataObject = new R4ResourcesData();
    injectField(dataObject, "resourceData", resourceDataMock);

    FhirContext context = mock(FhirContext.class);
    IGenericClient client = mock(IGenericClient.class);
    LaunchDetails launchDetails = mock(LaunchDetails.class);
    R4FhirData r4FhirData = mock(R4FhirData.class);

    Encounter encounter = new Encounter();
    encounter.setId("Encounter/enc-1");

    Date start = daysFromNow(-2);
    Date end = daysFromNow(2);

    Bundle rawBundle = new Bundle();
    Observation badObs = new Observation();
    badObs.setId("Observation/obs-bad");
    badObs.setStatus(Observation.ObservationStatus.ENTEREDINERROR);
    badObs.setIssued(now());
    badObs.setEncounter(new Reference("Encounter/enc-1"));
    rawBundle.addEntry().setResource(badObs);

    Observation goodObs = new Observation();
    goodObs.setId("Observation/obs-good");
    goodObs.setStatus(Observation.ObservationStatus.FINAL);
    goodObs.setIssued(now());
    goodObs.setEncounter(new Reference("Encounter/enc-1"));
    rawBundle.addEntry().setResource(goodObs);

    when(resourceDataMock.getResourceByPatientIdAndCode(
            eq(launchDetails),
            eq(client),
            eq(context),
            anyString(),
            eq(QueryConstants.PREGNANCY_CODE),
            eq(QueryConstants.LOINC_CODE_SYSTEM)))
        .thenReturn(rawBundle);
    List<Observation> result =
        dataObject.getPregnancyObservationData(
            context, client, launchDetails, r4FhirData, encounter, start, end);

    // Assert
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals("obs-good", result.get(0).getIdElement().getIdPart());
  }

  private void injectField(Object target, String fieldName, Object value) throws Exception {
    Field f = target.getClass().getDeclaredField(fieldName);
    f.setAccessible(true);
    f.set(target, value);
  }

  private Date daysFromNow(int days) {
    Calendar cal = Calendar.getInstance();
    cal.add(Calendar.DATE, days);
    return cal.getTime();
  }

  private Date now() {
    return new Date();
  }
}
