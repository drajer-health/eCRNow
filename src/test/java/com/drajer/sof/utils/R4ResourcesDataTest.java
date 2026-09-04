package com.drajer.sof.utils;

// import static org.junit.Assert.*;
// import static org.mockito.ArgumentMatchers.*;
// import static org.mockito.Mockito.*;
//
// import ca.uhn.fhir.context.FhirContext;
// import ca.uhn.fhir.rest.client.api.IGenericClient;
// import com.drajer.sof.model.LaunchDetails;
// import com.drajer.sof.model.R4FhirData;
// import com.drajer.test.util.TestUtils;
// import java.lang.reflect.Field;
// import java.util.ArrayList;
// import java.util.Calendar;
// import java.util.Date;
// import java.util.List;
// import org.hl7.fhir.r4.model.*;
// import org.junit.Before;
// import org.junit.Test;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;

public class R4ResourcesDataTest {

  //  private FhirContextInitializer resourceDataMock;
  //
  //  @Before
  //  public void setUp() {
  //    resourceDataMock = mock(FhirContextInitializer.class);
  //  }
  //
  //  @Test
  //  public void constructR4DocumentReferenceTest() {
  //    R4ResourcesData dataObject = new R4ResourcesData();
  //    String rrXml = TestUtils.getFileContentAsString("R4/Misc/TestRRXml.xml");
  //    DocumentReference result =
  //        dataObject.constructR4DocumentReference(
  //            rrXml, "P123456", "E98765", "1225652472001060", "text/xml");
  //    assertNotNull(result);
  //  }
  //
  //  @Test
  //  public void testIsConditionActive() {
  //
  //    R4ResourcesData dataObject = new R4ResourcesData();
  //    Condition cond1 = new Condition();
  //
  //    DateTimeType d = new DateTimeType(new Date((System.currentTimeMillis() - 10000)));
  //    cond1.setAbatement(d);
  //
  //    assertFalse(dataObject.isConditionActive(cond1));
  //
  //    cond1.setAbatement(new DateTimeType(new Date((System.currentTimeMillis() + 10000))));
  //
  //    assertTrue(dataObject.isConditionActive(cond1));
  //  }
  //
  //  @Test
  //  public void test_observationHasSameEncounter() {
  //    Encounter enc = new Encounter();
  //    enc.setId("enc-123");
  //    Observation obs = new Observation();
  //    obs.setEncounter(new Reference("Encounter/enc-123")); // reference idPart = "enc-123"
  //    R4ResourcesData r4ResourcesData = new R4ResourcesData();
  //    Boolean result = r4ResourcesData.observationHasSameEncounter(enc, obs);
  //    assertTrue(result);
  //  }
  //
  //  @Test
  //  public void returnsFalse_whenIdDoesNotMatch() {
  //    Encounter enc = new Encounter();
  //    enc.setId("enc-123");
  //    Observation obs = new Observation();
  //    obs.setEncounter(new Reference("Encounter/enc-999"));
  //    R4ResourcesData r4ResourcesData = new R4ResourcesData();
  //    assertFalse(r4ResourcesData.observationHasSameEncounter(enc, obs));
  //  }
  //
  //  @Test
  //  public void returnsTrue_whenIssuedWithinRange() {
  //    Date start = daysFromNow(-2);
  //    Date end = daysFromNow(2);
  //    Observation obs = new Observation();
  //    obs.setIssued(now());
  //    R4ResourcesData r4ResourcesData = new R4ResourcesData();
  //    assertTrue(r4ResourcesData.isObservationWithinTimeRange(start, end, obs));
  //  }
  //
  //  @Test
  //  public void returnsFalse_whenAllDatesOutsideRange() {
  //    Date start = daysFromNow(-10);
  //    Date end = daysFromNow(-5);
  //    Observation obs = new Observation();
  //    obs.setIssued(now());
  //    Meta meta = new Meta();
  //    meta.setLastUpdated(now());
  //    obs.setMeta(meta);
  //    obs.setEffective(new DateTimeType(now()));
  //    R4ResourcesData r4ResourcesData = new R4ResourcesData();
  //    assertFalse(r4ResourcesData.isObservationWithinTimeRange(start, end, obs));
  //  }
  //
  //  @Test
  //  public void returnsTrue_whenEffectiveWithinRange() {
  //    Date start = daysFromNow(-2);
  //    Date end = daysFromNow(2);
  //    Observation obs = new Observation();
  //    obs.setEffective(new DateTimeType(now()));
  //    R4ResourcesData r4ResourcesData = new R4ResourcesData();
  //    assertTrue(r4ResourcesData.isObservationWithinTimeRange(start, end, obs));
  //  }
  //
  //  @Test
  //  public void returnsTrue_whenLastUpdatedWithinRange() {
  //    Date start = daysFromNow(-2);
  //    Date end = daysFromNow(2);
  //    Observation obs = new Observation();
  //    Meta meta = new Meta();
  //    meta.setLastUpdated(now());
  //    obs.setMeta(meta);
  //    R4ResourcesData r4ResourcesData = new R4ResourcesData();
  //    assertTrue(r4ResourcesData.isObservationWithinTimeRange(start, end, obs));
  //  }
  //
  //  @Test
  //  public void getPregnancyObservationData_whenBundleNull_returnsEmpty() throws Exception {
  //    R4ResourcesData dataObject = new R4ResourcesData();
  //    injectField(dataObject, "fhirContextInitializer", resourceDataMock);
  //
  //    FhirContext context = mock(FhirContext.class);
  //    IGenericClient client = mock(IGenericClient.class);
  //    LaunchDetails launchDetails = mock(LaunchDetails.class);
  //    R4FhirData r4FhirData = mock(R4FhirData.class);
  //
  //    Encounter encounter = new Encounter();
  //    Date start = now();
  //    Date end = now();
  //
  //    when(resourceDataMock.getResourceByPatientIdAndCode(
  //            eq(launchDetails),
  //            eq(client),
  //            eq(context),
  //            anyString(),
  //            eq(QueryConstants.PREGNANCY_CODE),
  //            eq(QueryConstants.LOINC_CODE_SYSTEM)))
  //        .thenReturn(null);
  //
  //    List<Observation> result =
  //        dataObject.getPregnancyObservationData(
  //            context, client, launchDetails, r4FhirData, encounter, start, end);
  //
  //    assertNotNull(result);
  //    assertTrue(result.isEmpty());
  //  }
  //
  //  @Test
  //  public void
  //
  // getPregnancyObservationData_whenBundleHasObservations_returnsFilteredList_withoutCallingPrivateMethods()
  //          throws Exception {
  //    R4ResourcesData dataObject = new R4ResourcesData();
  //    injectField(dataObject, "fhirContextInitializer", resourceDataMock);
  //
  //    FhirContext context = mock(FhirContext.class);
  //    IGenericClient client = mock(IGenericClient.class);
  //    LaunchDetails launchDetails = mock(LaunchDetails.class);
  //    R4FhirData r4FhirData = mock(R4FhirData.class);
  //
  //    Encounter encounter = new Encounter();
  //    encounter.setId("Encounter/enc-1");
  //
  //    Date start = daysFromNow(-2);
  //    Date end = daysFromNow(2);
  //
  //    Bundle rawBundle = new Bundle();
  //    Observation badObs = new Observation();
  //    badObs.setId("Observation/obs-bad");
  //    badObs.setStatus(Observation.ObservationStatus.ENTEREDINERROR);
  //    badObs.setIssued(now());
  //    badObs.setEncounter(new Reference("Encounter/enc-1"));
  //    rawBundle.addEntry().setResource(badObs);
  //
  //    Observation goodObs = new Observation();
  //    goodObs.setId("Observation/obs-good");
  //    goodObs.setStatus(Observation.ObservationStatus.FINAL);
  //    goodObs.setIssued(now());
  //    goodObs.setEncounter(new Reference("Encounter/enc-1"));
  //    rawBundle.addEntry().setResource(goodObs);
  //
  //    when(resourceDataMock.getResourceByPatientIdAndCode(
  //            eq(launchDetails),
  //            eq(client),
  //            eq(context),
  //            anyString(),
  //            eq(QueryConstants.PREGNANCY_CODE),
  //            eq(QueryConstants.LOINC_CODE_SYSTEM)))
  //        .thenReturn(rawBundle);
  //    List<Observation> result =
  //        dataObject.getPregnancyObservationData(
  //            context, client, launchDetails, r4FhirData, encounter, start, end);
  //
  //    // Assert
  //    assertNotNull(result);
  //    assertEquals(1, result.size());
  //    assertEquals("obs-good", result.get(0).getIdElement().getIdPart());
  //  }
  //
  //  private void injectField(Object target, String fieldName, Object value) throws Exception {
  //    Field f = target.getClass().getDeclaredField(fieldName);
  //    f.setAccessible(true);
  //    f.set(target, value);
  //  }
  //
  //  private Date daysFromNow(int days) {
  //    Calendar cal = Calendar.getInstance();
  //    cal.add(Calendar.DATE, days);
  //    return cal.getTime();
  //  }
  //
  //  private Date now() {
  //    return new Date();
  //  }

  private FhirContextInitializer resourceDataMock;
  private R4ResourcesData dataObject;
  private FhirContext context;
  private FhirClient client;
  private LaunchDetails launchDetails;
  private R4FhirData r4FhirData;

  @Before
  public void setUp() {
    resourceDataMock = mock(FhirContextInitializer.class);
    dataObject = new R4ResourcesData(resourceDataMock);
    context = FhirContext.forR4();
    // Deep stubs let a test write client.read().resource(...).withId(...).execute() (or
    // client.search().byUrl(...).returnBundle(...).execute()) directly inside when(...),
    // instead of mocking each intermediate fluent-builder interface by hand.
    client = mock(FhirClient.class, RETURNS_DEEP_STUBS);
    launchDetails = new LaunchDetails();
    launchDetails.setLaunchPatientId("P123456");
    launchDetails.setEncounterId("E123456");
    launchDetails.setEhrServerURL("http://example.com/fhir");
    // Required for FhirContextInitializer.getResourceBundleByUrl (used by
    // getTravelObservationData / getSocialHistoryObservationDataOccupation) to take the R4
    // branch instead of DSTU2. Not read by any mocked FhirContextInitializer method.
    launchDetails.setFhirVersion("R4");
    r4FhirData = new R4FhirData();
  }

  // ==================== HELPER METHODS ====================

  private Date now() {
    return new Date();
  }

  private Date daysFromNow(int days) {
    Calendar cal = Calendar.getInstance();
    cal.add(Calendar.DATE, days);
    return cal.getTime();
  }

  private Bundle createBundleWithObservations(int count, String encounterId, Date date) {
    Bundle bundle = new Bundle();
    for (int i = 0; i < count; i++) {
      Observation obs = new Observation();
      obs.setId("obs-" + i);
      obs.setStatus(Observation.ObservationStatus.FINAL);
      obs.setIssued(date);
      if (encounterId != null) {
        obs.setEncounter(new Reference("Encounter/" + encounterId));
      }

      CodeableConcept code = new CodeableConcept();
      code.addCoding().setSystem("http://loinc.org").setCode("2345-7").setDisplay("Glucose");
      obs.setCode(code);

      Quantity value = new Quantity();
      value.setValue(100 + i);
      value.setUnit("mg/dL");
      obs.setValue(value);

      bundle.addEntry().setResource(obs);
    }
    return bundle;
  }

  private Bundle createBundleWithConditions(int count, String encounterId, Date date) {
    Bundle bundle = new Bundle();
    for (int i = 0; i < count; i++) {
      Condition cond = new Condition();
      cond.setId("cond-" + i);

      CodeableConcept clinicalStatus = new CodeableConcept();
      clinicalStatus
          .addCoding()
          .setSystem("http://terminology.hl7.org/CodeSystem/condition-clinical")
          .setCode("active");
      cond.setClinicalStatus(clinicalStatus);

      CodeableConcept vStatus = new CodeableConcept();
      vStatus
          .addCoding()
          .setSystem("http://terminology.hl7.org/CodeSystem/condition-ver-status")
          .setCode("confirmed");
      cond.setVerificationStatus(vStatus);

      CodeableConcept category = new CodeableConcept();
      category
          .addCoding()
          .setSystem("http://terminology.hl7.org/CodeSystem/condition-category")
          .setCode("problem-list-item");
      cond.addCategory(category);

      CodeableConcept code = new CodeableConcept();
      code.addCoding()
          .setSystem("http://snomed.info/sct")
          .setCode("1234" + i)
          .setDisplay("Condition " + i);
      cond.setCode(code);

      if (encounterId != null) {
        cond.setEncounter(new Reference("Encounter/" + encounterId));
      }

      bundle.addEntry().setResource(cond);
    }
    return bundle;
  }

  private Encounter createEncounter(String id, Date start, Date end) {
    Encounter encounter = new Encounter();
    encounter.setId(id);
    encounter.setStatus(Encounter.EncounterStatus.FINISHED);

    Period period = new Period();
    period.setStart(start);
    period.setEnd(end);
    encounter.setPeriod(period);

    CodeableConcept type = new CodeableConcept();
    type.addCoding()
        .setSystem("http://snomed.info/sct")
        .setCode("99213")
        .setDisplay("Office visit");
    encounter.addType(type);

    return encounter;
  }

  private Condition buildCondition(
      String id,
      String verificationStatusCode,
      boolean active,
      String categoryCode,
      String snomedCode,
      String encounterId) {
    Condition cond = new Condition();
    cond.setId(id);

    if (!active) {
      cond.setAbatement(new DateTimeType(daysFromNow(-10)));
    }

    if (verificationStatusCode != null) {
      CodeableConcept vStatus = new CodeableConcept();
      vStatus
          .addCoding()
          .setSystem("http://terminology.hl7.org/CodeSystem/condition-ver-status")
          .setCode(verificationStatusCode);
      cond.setVerificationStatus(vStatus);
    }

    if (categoryCode != null) {
      CodeableConcept category = new CodeableConcept();
      category
          .addCoding()
          .setSystem("http://terminology.hl7.org/CodeSystem/condition-category")
          .setCode(categoryCode);
      cond.addCategory(category);
    }

    if (snomedCode != null) {
      CodeableConcept code = new CodeableConcept();
      code.addCoding().setSystem("http://snomed.info/sct").setCode(snomedCode);
      cond.setCode(code);
    }

    if (encounterId != null) {
      cond.setEncounter(new Reference("Encounter/" + encounterId));
    }

    return cond;
  }

  // ==================== Pure helper-method tests (no mocking) ====================

  @Test
  public void isResourceWithinDateTime_coversAllBranches() {
    Date start = daysFromNow(-5);
    Date end = daysFromNow(5);

    assertTrue(dataObject.isResourceWithinDateTime(start, end, now()));
    assertFalse(dataObject.isResourceWithinDateTime(start, end, daysFromNow(-10)));
    assertFalse(dataObject.isResourceWithinDateTime(start, end, daysFromNow(10)));
    assertFalse(dataObject.isResourceWithinDateTime(null, end, now()));
    assertFalse(dataObject.isResourceWithinDateTime(start, null, now()));
    assertFalse(dataObject.isResourceWithinDateTime(start, end, null));
  }

  @Test
  public void isConditionActive_coversAllBranches() {
    assertTrue(dataObject.isConditionActive(new Condition()));

    Condition futureAbatement = new Condition();
    futureAbatement.setAbatement(new DateTimeType(daysFromNow(10)));
    assertTrue(dataObject.isConditionActive(futureAbatement));

    Condition pastAbatement = new Condition();
    pastAbatement.setAbatement(new DateTimeType(daysFromNow(-10)));
    assertFalse(dataObject.isConditionActive(pastAbatement));

    // R4 Condition.abatement[x] supports Age/Period/Range/string/dateTime (no boolean).
    // isConditionActive only treats a DateTimeType abatement as a deactivation signal, so any
    // other type (e.g. string) should leave the condition active.
    Condition nonDateTimeAbatement = new Condition();
    nonDateTimeAbatement.setAbatement(new StringType("resolved"));
    assertTrue(dataObject.isConditionActive(nonDateTimeAbatement));
  }

  @Test
  public void observationHasSameEncounter_coversAllBranches() {
    Encounter encounter = new Encounter();
    encounter.setId("E123");

    Observation matching = new Observation();
    matching.setEncounter(new Reference("Encounter/E123"));
    assertTrue(dataObject.observationHasSameEncounter(encounter, matching));

    Observation different = new Observation();
    different.setEncounter(new Reference("Encounter/E999"));
    assertFalse(dataObject.observationHasSameEncounter(encounter, different));

    assertFalse(dataObject.observationHasSameEncounter(null, matching));
  }

  @Test
  public void isObservationWithinTimeRange_coversAllBranches() {
    Date start = daysFromNow(-5);
    Date end = daysFromNow(5);

    Observation issued = new Observation();
    issued.setIssued(now());
    assertTrue(dataObject.isObservationWithinTimeRange(start, end, issued));

    Observation effective = new Observation();
    effective.setEffective(new DateTimeType(now()));
    assertTrue(dataObject.isObservationWithinTimeRange(start, end, effective));

    Observation lastUpdated = new Observation();
    Meta meta = new Meta();
    meta.setLastUpdated(now());
    lastUpdated.setMeta(meta);
    assertTrue(dataObject.isObservationWithinTimeRange(start, end, lastUpdated));

    Observation outsideRange = new Observation();
    outsideRange.setIssued(now());
    Meta outsideMeta = new Meta();
    outsideMeta.setLastUpdated(now());
    outsideRange.setMeta(outsideMeta);
    assertFalse(
        dataObject.isObservationWithinTimeRange(daysFromNow(-10), daysFromNow(-5), outsideRange));
  }

  @Test
  public void findAllValueCodes_coversCodeableConceptAndQuantityBranches() {
    Observation withCode = new Observation();
    CodeableConcept cc = new CodeableConcept();
    cc.addCoding().setSystem("http://snomed.info/sct").setCode("12345").setDisplay("Test");
    withCode.setValue(cc);

    List<Observation> valueObs = new ArrayList<>();
    List<CodeableConcept> valueCodes = new ArrayList<>();
    dataObject.findAllValueCodes(withCode, valueObs, valueCodes);
    assertEquals(1, valueObs.size());
    assertEquals(1, valueCodes.size());

    Observation withQuantity = new Observation();
    Quantity q = new Quantity();
    q.setValue(100);
    withQuantity.setValue(q);
    dataObject.findAllValueCodes(withQuantity, valueObs, valueCodes);
    // Quantity values are not codes, so nothing new is added.
    assertEquals(1, valueObs.size());
  }

  @Test
  public void getResourceFromBundle_coversFoundNotFoundAndNullBundleBranches() {
    Bundle bundle = new Bundle();
    Patient patient = new Patient();
    patient.setId("P1");
    bundle.addEntry().setResource(patient);

    Resource found = dataObject.getResourceFromBundle(bundle, Patient.class);
    assertNotNull(found);
    assertTrue(found instanceof Patient);

    assertNull(dataObject.getResourceFromBundle(bundle, Condition.class));
    assertNull(dataObject.getResourceFromBundle(null, Patient.class));
  }

  @Test
  public void filterObservation_withEncounterReference_returnsMatching() {
    Bundle bundle = createBundleWithObservations(3, "E123", now());
    Encounter encounter = new Encounter();
    encounter.setId("E123");

    List<Observation> result = dataObject.filterObservation(bundle, encounter, null, null);

    assertEquals(3, result.size());
  }

  @Test
  public void filterObservation_withoutEncounter_coversDateRangeFallbacks() {
    Bundle bundle = createBundleWithObservations(3, null, now());

    Observation effectiveOnly = new Observation();
    effectiveOnly.setId("obs-effective");
    effectiveOnly.setEffective(new DateTimeType(now()));
    bundle.addEntry().setResource(effectiveOnly);

    Observation lastUpdatedOnly = new Observation();
    lastUpdatedOnly.setId("obs-last-updated");
    lastUpdatedOnly.getMeta().setLastUpdated(now());
    bundle.addEntry().setResource(lastUpdatedOnly);

    List<Observation> result =
        dataObject.filterObservation(bundle, null, daysFromNow(-5), daysFromNow(5));

    assertEquals(5, result.size());
  }

  @Test
  public void constructR4DocumentReference_coversAllParamVariations() {
    DocumentReference withAllParams =
        dataObject.constructR4DocumentReference(
            "<eICR>content</eICR>", "P123", "E456", "PRACT789", "text/xml");
    assertNotNull(withAllParams);
    assertEquals(Enumerations.DocumentReferenceStatus.CURRENT, withAllParams.getStatus());
    assertEquals(DocumentReference.ReferredDocumentStatus.FINAL, withAllParams.getDocStatus());
    assertEquals("Patient/P123", withAllParams.getSubject().getReference());
    assertEquals("Practitioner/PRACT789", withAllParams.getAuthor().get(0).getReference());

    DocumentReference withNullXml =
        dataObject.constructR4DocumentReference(null, "P123", "E456", "PRACT789", "text/xml");
    assertNotNull(withNullXml);
    assertNull(withNullXml.getContent().get(0).getAttachment().getData());

    DocumentReference withNullProvider =
        dataObject.constructR4DocumentReference("<xml/>", "P123", "E456", null, "text/xml");
    assertNotNull(withNullProvider);
    assertTrue(withNullProvider.getAuthor().isEmpty());
  }

  // ==================== getEncounterData Tests ====================

  @Test
  public void getEncounterData_resolvesByIdOrByMostRecentlyUpdatedFromBundle() {
    // launchDetails has encounterId "E123456", so R4ResourcesData#getEncounterData reads it
    // directly via client.read().resource("Encounter").withId(id).execute() -- it does not
    // go through fhirContextInitializer.getResouceById for the by-id lookup.
    Encounter byId = createEncounter("E123456", daysFromNow(-1), now());
    when(client.read().resource("Encounter").withId("E123456").execute()).thenReturn(byId);

    Encounter result1 =
        dataObject.getEncounterData(
            context, client, launchDetails, r4FhirData, daysFromNow(-5), daysFromNow(5));
    assertNotNull(result1);
    assertEquals("E123456", result1.getIdElement().getIdPart());

    // No encounter ID -> searches the patient's Encounter bundle and picks the entry with the
    // most recently updated Meta.lastUpdated. One entry has no period at all, exercising
    // extractEncounterDate's Meta.lastUpdated fallback too.
    launchDetails.setEncounterId(null);

    Encounter noPeriod = new Encounter();
    noPeriod.setId("E-no-period");
    noPeriod.setStatus(Encounter.EncounterStatus.FINISHED);
    noPeriod.getMeta().setLastUpdated(daysFromNow(-4));

    Encounter older = createEncounter("E-older", daysFromNow(-3), now());
    older.getMeta().setLastUpdated(daysFromNow(-2));

    Encounter mostRecent = createEncounter("E-recent", daysFromNow(-3), now());
    mostRecent.getMeta().setLastUpdated(daysFromNow(-1));

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(noPeriod);
    bundle.addEntry().setResource(older);
    bundle.addEntry().setResource(mostRecent);

    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Encounter")))
        .thenReturn(bundle);

    Encounter result2 =
        dataObject.getEncounterData(
            context, client, launchDetails, r4FhirData, daysFromNow(-5), daysFromNow(5));
    assertNotNull(result2);
    assertEquals("E-recent", result2.getIdElement().getIdPart());
  }

  @Test
  public void getEncounterData_handlesSkippedResourceExceptionAndNullBundle() {
    // 1. checkSkipResource() true -> returns null immediately without calling client.read().
    when(resourceDataMock.checkSkipResource(eq("Encounter"), any(FhirClient.class)))
        .thenReturn(true);
    assertNull(
        dataObject.getEncounterData(
            context, client, launchDetails, r4FhirData, daysFromNow(-5), daysFromNow(5)));
    verify(client, never()).read();

    // 2. Not skipped, but client.read()...execute() throws -> caught internally, returns null.
    when(resourceDataMock.checkSkipResource(eq("Encounter"), any(FhirClient.class)))
        .thenReturn(false);
    when(client.read().resource("Encounter").withId("E123456").execute())
        .thenThrow(new RuntimeException("simulated read failure"));
    assertNull(
        dataObject.getEncounterData(
            context, client, launchDetails, r4FhirData, daysFromNow(-5), daysFromNow(5)));

    // 3. No encounter ID and the patient-search bundle is null -> returns null.
    launchDetails.setEncounterId(null);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Encounter")))
        .thenReturn(null);
    assertNull(
        dataObject.getEncounterData(
            context, client, launchDetails, r4FhirData, daysFromNow(-5), daysFromNow(5)));
  }

  // ==================== getConditionData Tests ====================

  @Test
  public void getConditionData_filtersMixedConditionsAndHandlesNullBundle() {
    // Entered-in-error verification status -> skipped, counted as inError.
    Condition enteredInError =
        buildCondition("cond-error", "entered-in-error", true, "problem-list-item", "1111", null);
    // Inactive (abated in the past) -> skipped, counted as missingAbatement.
    Condition inactive =
        buildCondition("cond-inactive", "confirmed", false, "problem-list-item", "2222", null);
    // No category at all -> skipped, counted as missingAbatement.
    Condition noCategory =
        buildCondition("cond-no-category", "confirmed", true, null, "3333", null);
    // Problem list item -> added to problemConditions.
    Condition problem =
        buildCondition("cond-problem", "confirmed", true, "problem-list-item", "4444", null);
    // Encounter diagnosis matching launchDetails' encounter -> added to
    // encounterDiagnosisConditions.
    Condition encounterDiagnosis =
        buildCondition(
            "cond-enc-diag", "confirmed", true, "encounter-diagnosis", "5555", "E123456");
    // Pregnancy code on a problem-list-item -> dedup logic excludes it from problemConditions.
    Condition pregnancy =
        buildCondition("cond-pregnancy", "confirmed", true, "problem-list-item", "77386006", null);

    Bundle bundle = new Bundle();
    for (Condition c :
        List.of(enteredInError, inactive, noCategory, problem, encounterDiagnosis, pregnancy)) {
      bundle.addEntry().setResource(c);
    }

    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Condition")))
        .thenReturn(bundle);

    List<Condition> result =
        dataObject.getConditionData(
            context,
            client,
            launchDetails,
            r4FhirData,
            createEncounter("E123456", daysFromNow(-1), now()),
            daysFromNow(-5),
            daysFromNow(5));

    assertNotNull(result);
    // Only "problem" and "encounterDiagnosis" survive filtering; pregnancy is deduped out.
    assertEquals(2, result.size());
    assertTrue(result.contains(problem));
    assertTrue(result.contains(encounterDiagnosis));
    assertFalse(result.contains(pregnancy));

    // A null bundle from the FHIR server yields an empty (not null) result.
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Condition")))
        .thenReturn(null);
    List<Condition> emptyResult =
        dataObject.getConditionData(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertTrue(emptyResult.isEmpty());
  }

  // ==================== getObservationData Tests ====================

  @Test
  public void getObservationData_filtersByEncounterAndHandlesNullBundle() {
    Bundle bundle = createBundleWithObservations(5, "E123456", now());
    when(resourceDataMock.getObservationByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Observation"), eq("laboratory")))
        .thenReturn(bundle);

    List<Observation> result =
        dataObject.getObservationData(
            context, client, launchDetails, r4FhirData, daysFromNow(-5), daysFromNow(5));
    assertNotNull(result);
    assertEquals(5, result.size());

    when(resourceDataMock.getObservationByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Observation"), eq("laboratory")))
        .thenReturn(null);
    List<Observation> emptyResult =
        dataObject.getObservationData(
            context, client, launchDetails, r4FhirData, daysFromNow(-5), daysFromNow(5));
    assertTrue(emptyResult.isEmpty());
  }

  @Test
  public void getObservationData_withoutEncounterId_usesDateRangeFilter() {
    launchDetails.setEncounterId(null); // Forces filterByDateRange instead of filterByEncounterId

    Bundle bundle = createBundleWithObservations(2, null, now());

    Observation effectiveOnly = new Observation();
    effectiveOnly.setId("obs-effective");
    effectiveOnly.setStatus(Observation.ObservationStatus.FINAL);
    effectiveOnly.setEffective(new DateTimeType(now()));
    bundle.addEntry().setResource(effectiveOnly);

    Observation lastUpdatedOnly = new Observation();
    lastUpdatedOnly.setId("obs-last-updated");
    lastUpdatedOnly.setStatus(Observation.ObservationStatus.FINAL);
    lastUpdatedOnly.getMeta().setLastUpdated(now());
    bundle.addEntry().setResource(lastUpdatedOnly);

    // Effective set to a non-DateTimeType (Period) -> getEffectiveDateTimeType() throws,
    // exercising extractObservationDate's catch block; falls back to Meta.lastUpdated.
    Observation effectiveAsPeriod = new Observation();
    effectiveAsPeriod.setId("obs-effective-period");
    effectiveAsPeriod.setStatus(Observation.ObservationStatus.FINAL);
    effectiveAsPeriod.setEffective(new Period().setStart(daysFromNow(-1)).setEnd(now()));
    effectiveAsPeriod.getMeta().setLastUpdated(now());
    bundle.addEntry().setResource(effectiveAsPeriod);

    when(resourceDataMock.getObservationByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Observation"), eq("laboratory")))
        .thenReturn(bundle);

    List<Observation> result =
        dataObject.getObservationData(
            context, client, launchDetails, r4FhirData, daysFromNow(-5), daysFromNow(5));

    assertNotNull(result);
    assertEquals(5, result.size());
  }

  // ==================== getMedicationData Tests ====================

  @Test
  public void getMedicationData_coversValidAndNullIdBranches() {
    Medication med = new Medication();
    med.setId("M123");
    when(resourceDataMock.getResouceById(
            eq(launchDetails), eq(client), eq(context), eq("Medication"), eq("M123")))
        .thenReturn(med);

    Medication result =
        dataObject.getMedicationData(context, client, launchDetails, r4FhirData, "M123");
    assertNotNull(result);
    assertEquals("M123", result.getId());

    when(resourceDataMock.getResouceById(
            eq(launchDetails), eq(client), eq(context), eq("Medication"), isNull()))
        .thenReturn(null);
    assertNull(dataObject.getMedicationData(context, client, launchDetails, r4FhirData, null));
  }

  // ==================== getMedicationAdministrationData Tests ====================

  @Test
  public void getMedicationAdministrationData_filtersByEncounterOrDateRange() {
    CodeableConcept medCode = new CodeableConcept();
    medCode.addCoding().setSystem("http://www.nlm.nih.gov/research/umls/rxnorm").setCode("197361");

    // Matches by encounter reference.
    Bundle byEncounterBundle = new Bundle();
    MedicationAdministration medAdmin = new MedicationAdministration();
    medAdmin.setId("M1");
    medAdmin.setStatus(MedicationAdministration.MedicationAdministrationStatus.COMPLETED);
    medAdmin.setContext(new Reference("Encounter/E123456"));
    // medication[x] is required on MedicationAdministration; findMedicationCodes()
    // dereferences it, so it must be populated to reflect real FHIR data.
    medAdmin.setMedication(medCode);
    byEncounterBundle.addEntry().setResource(medAdmin);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationAdministration")))
        .thenReturn(byEncounterBundle);

    List<MedicationAdministration> byEncounterResult =
        dataObject.getMedicationAdministrationData(
            context,
            client,
            launchDetails,
            r4FhirData,
            createEncounter("E123456", daysFromNow(-1), now()),
            daysFromNow(-5),
            daysFromNow(5));
    assertEquals(1, byEncounterResult.size());
    assertEquals(1, r4FhirData.getR4MedicationCodes().size());

    // No encounter -> falls back to date-range filtering.
    Bundle byDateBundle = new Bundle();
    MedicationAdministration medAdminByDate = new MedicationAdministration();
    medAdminByDate.setId("M2");
    medAdminByDate.setStatus(MedicationAdministration.MedicationAdministrationStatus.COMPLETED);
    medAdminByDate.setEffective(new DateTimeType(now()));
    medAdminByDate.setMedication(medCode);
    byDateBundle.addEntry().setResource(medAdminByDate);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationAdministration")))
        .thenReturn(byDateBundle);

    List<MedicationAdministration> byDateResult =
        dataObject.getMedicationAdministrationData(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertEquals(1, byDateResult.size());
  }

  // ==================== getMedicationRequestData Tests ====================

  @Test
  public void getMedicationRequestData_filtersByEncounterOrDateRange() {
    CodeableConcept medCode = new CodeableConcept();
    medCode.addCoding().setSystem("http://www.nlm.nih.gov/research/umls/rxnorm").setCode("197361");

    Bundle byEncounterBundle = new Bundle();
    MedicationRequest medReq = new MedicationRequest();
    medReq.setId("MR1");
    medReq.setStatus(MedicationRequest.MedicationRequestStatus.ACTIVE);
    medReq.setEncounter(new Reference("Encounter/E123456"));
    medReq.setMedication(medCode);
    byEncounterBundle.addEntry().setResource(medReq);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationRequest")))
        .thenReturn(byEncounterBundle);

    List<MedicationRequest> byEncounterResult =
        dataObject.getMedicationRequestData(
            context,
            client,
            launchDetails,
            r4FhirData,
            createEncounter("E123456", daysFromNow(-1), now()),
            daysFromNow(-5),
            daysFromNow(5));
    assertEquals(1, byEncounterResult.size());
    assertEquals(1, r4FhirData.getR4MedicationCodes().size());

    // No encounter -> falls back to date-range filtering. One entry has authoredOn set, the
    // other doesn't (exercising extractMedicationRequestDate's Meta.lastUpdated fallback).
    Bundle byDateBundle = new Bundle();
    MedicationRequest withAuthoredOn = new MedicationRequest();
    withAuthoredOn.setId("MR2");
    withAuthoredOn.setStatus(MedicationRequest.MedicationRequestStatus.ACTIVE);
    withAuthoredOn.setAuthoredOn(now());
    withAuthoredOn.setMedication(medCode);
    byDateBundle.addEntry().setResource(withAuthoredOn);

    MedicationRequest withoutAuthoredOn = new MedicationRequest();
    withoutAuthoredOn.setId("MR3");
    withoutAuthoredOn.setStatus(MedicationRequest.MedicationRequestStatus.ACTIVE);
    withoutAuthoredOn.setMedication(medCode);
    withoutAuthoredOn.getMeta().setLastUpdated(now());
    byDateBundle.addEntry().setResource(withoutAuthoredOn);

    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationRequest")))
        .thenReturn(byDateBundle);

    List<MedicationRequest> byDateResult =
        dataObject.getMedicationRequestData(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertEquals(2, byDateResult.size());
  }

  // ==================== getMedicationStatementData Tests ====================

  @Test
  public void getMedicationStatementData_filtersByEncounterOrDateRange() {
    CodeableConcept medCode = new CodeableConcept();
    medCode.addCoding().setSystem("http://www.nlm.nih.gov/research/umls/rxnorm").setCode("197361");

    Bundle byEncounterBundle = new Bundle();
    MedicationStatement medStmt = new MedicationStatement();
    medStmt.setId("MS1");
    medStmt.setStatus(MedicationStatement.MedicationStatementStatus.ACTIVE);
    medStmt.setContext(new Reference("Encounter/E123456"));
    medStmt.setMedication(medCode);
    byEncounterBundle.addEntry().setResource(medStmt);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationStatement")))
        .thenReturn(byEncounterBundle);

    List<MedicationStatement> byEncounterResult =
        dataObject.getMedicationStatementData(
            context,
            client,
            launchDetails,
            r4FhirData,
            createEncounter("E123456", daysFromNow(-1), now()),
            daysFromNow(-5),
            daysFromNow(5));
    assertEquals(1, byEncounterResult.size());
    assertEquals(1, r4FhirData.getR4MedicationCodes().size());

    // No encounter -> falls back to date-range filtering. One entry has effective[x] set, the
    // other doesn't (exercising extractMedicationStatementDate's Meta.lastUpdated fallback).
    Bundle byDateBundle = new Bundle();
    MedicationStatement withEffective = new MedicationStatement();
    withEffective.setId("MS2");
    withEffective.setStatus(MedicationStatement.MedicationStatementStatus.ACTIVE);
    withEffective.setEffective(new DateTimeType(now()));
    withEffective.setMedication(medCode);
    byDateBundle.addEntry().setResource(withEffective);

    MedicationStatement withoutEffective = new MedicationStatement();
    withoutEffective.setId("MS3");
    withoutEffective.setStatus(MedicationStatement.MedicationStatementStatus.ACTIVE);
    withoutEffective.setMedication(medCode);
    withoutEffective.getMeta().setLastUpdated(now());
    byDateBundle.addEntry().setResource(withoutEffective);

    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationStatement")))
        .thenReturn(byDateBundle);

    List<MedicationStatement> byDateResult =
        dataObject.getMedicationStatementData(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertEquals(2, byDateResult.size());
  }

  // ==================== getDiagnosticReportData Tests ====================

  @Test
  public void getDiagnosticReportData_filtersByEncounterOrDateRange() {
    Bundle byEncounterBundle = new Bundle();
    DiagnosticReport diag = new DiagnosticReport();
    diag.setId("D1");
    diag.setStatus(DiagnosticReport.DiagnosticReportStatus.FINAL);
    diag.setEncounter(new Reference("Encounter/E123456"));
    CodeableConcept code = new CodeableConcept();
    code.addCoding().setSystem("http://loinc.org").setCode("58410-2");
    diag.setCode(code);
    byEncounterBundle.addEntry().setResource(diag);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("DiagnosticReport")))
        .thenReturn(byEncounterBundle);

    List<DiagnosticReport> byEncounterResult =
        dataObject.getDiagnosticReportData(
            context,
            client,
            launchDetails,
            r4FhirData,
            createEncounter("E123456", daysFromNow(-1), now()),
            daysFromNow(-5),
            daysFromNow(5));
    assertEquals(1, byEncounterResult.size());
    assertEquals(1, r4FhirData.getR4DiagnosticReportCodes().size());

    // No encounter -> falls back to date-range filtering, covering the issued,
    // effective-DateTimeType, and effective-Period (catch block) date-extraction branches.
    // Note: unlike most other R4 resources here, DiagnosticReport.getEffective() returns null
    // (not an empty Type) when unset, so a "neither issued nor effective" entry would NPE in
    // extractDiagnosticReportDate -- not exercised here.
    Bundle byDateBundle = new Bundle();
    DiagnosticReport withIssued = new DiagnosticReport();
    withIssued.setId("D2");
    withIssued.setStatus(DiagnosticReport.DiagnosticReportStatus.FINAL);
    withIssued.setIssued(now());
    byDateBundle.addEntry().setResource(withIssued);

    DiagnosticReport withEffectiveDateTime = new DiagnosticReport();
    withEffectiveDateTime.setId("D3");
    withEffectiveDateTime.setStatus(DiagnosticReport.DiagnosticReportStatus.FINAL);
    withEffectiveDateTime.setEffective(new DateTimeType(now()));
    byDateBundle.addEntry().setResource(withEffectiveDateTime);

    DiagnosticReport withEffectivePeriod = new DiagnosticReport();
    withEffectivePeriod.setId("D4");
    withEffectivePeriod.setStatus(DiagnosticReport.DiagnosticReportStatus.FINAL);
    withEffectivePeriod.setEffective(new Period().setStart(daysFromNow(-1)).setEnd(now()));
    withEffectivePeriod.getMeta().setLastUpdated(now());
    byDateBundle.addEntry().setResource(withEffectivePeriod);

    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("DiagnosticReport")))
        .thenReturn(byDateBundle);

    List<DiagnosticReport> byDateResult =
        dataObject.getDiagnosticReportData(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertEquals(3, byDateResult.size());
  }

  // ==================== getImmunizationData Tests ====================

  @Test
  public void getImmunizationData_coversEncounterMatchNonMatchAndDateRangeBranches() {
    // Matches the given encounter -> added directly, with its vaccine code captured.
    Immunization matching = new Immunization();
    matching.setId("I1");
    matching.setStatus(Immunization.ImmunizationStatus.COMPLETED);
    matching.setEncounter(new Reference("Encounter/E123456"));
    matching.setOccurrence(new DateTimeType(now()));
    CodeableConcept vaccineCode = new CodeableConcept();
    vaccineCode.addCoding().setSystem("http://hl7.org/fhir/sid/cvx").setCode("140");
    matching.setVaccineCode(vaccineCode);

    Bundle matchingBundle = new Bundle();
    matchingBundle.addEntry().setResource(matching);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Immunization")))
        .thenReturn(matchingBundle);

    List<Immunization> matchResult =
        dataObject.getImmunizationData(
            context,
            client,
            launchDetails,
            r4FhirData,
            createEncounter("E123456", daysFromNow(-1), now()),
            daysFromNow(-5),
            daysFromNow(5));
    assertEquals(1, matchResult.size());
    assertEquals(1, r4FhirData.getR4ImmunizationCodes().size());

    // Encounter is present, but this immunization references a different one -> the per-entry
    // "else" branch falls back to date-range matching instead.
    Immunization nonMatching = new Immunization();
    nonMatching.setId("I2");
    nonMatching.setStatus(Immunization.ImmunizationStatus.COMPLETED);
    nonMatching.setEncounter(new Reference("Encounter/OTHER"));
    nonMatching.setOccurrence(new DateTimeType(now()));

    Bundle nonMatchingBundle = new Bundle();
    nonMatchingBundle.addEntry().setResource(nonMatching);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Immunization")))
        .thenReturn(nonMatchingBundle);

    List<Immunization> nonMatchResult =
        dataObject.getImmunizationData(
            context,
            client,
            launchDetails,
            r4FhirData,
            createEncounter("E123456", daysFromNow(-1), now()),
            daysFromNow(-5),
            daysFromNow(5));
    assertEquals(1, nonMatchResult.size());

    // No encounter at all -> both entries go through populateImmunizationsWithoutEncounters,
    // covering both its occurrence-dateTime branch and its Meta.lastUpdated fallback branch
    // (occurrence[x] is required on Immunization, but StringType is a valid non-dateTime choice).
    Immunization withOccurrence = new Immunization();
    withOccurrence.setId("I3");
    withOccurrence.setStatus(Immunization.ImmunizationStatus.COMPLETED);
    withOccurrence.setOccurrence(new DateTimeType(now()));

    Immunization withStringOccurrence = new Immunization();
    withStringOccurrence.setId("I4");
    withStringOccurrence.setStatus(Immunization.ImmunizationStatus.COMPLETED);
    withStringOccurrence.setOccurrence(new StringType("Unknown"));
    withStringOccurrence.getMeta().setLastUpdated(now());

    Bundle noEncounterBundle = new Bundle();
    noEncounterBundle.addEntry().setResource(withOccurrence);
    noEncounterBundle.addEntry().setResource(withStringOccurrence);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Immunization")))
        .thenReturn(noEncounterBundle);

    List<Immunization> noEncounterResult =
        dataObject.getImmunizationData(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertEquals(2, noEncounterResult.size());
  }

  // ==================== getServiceRequestData Tests ====================

  @Test
  public void getServiceRequestData_filtersByEncounterOrDateRange() {
    Bundle byEncounterBundle = new Bundle();
    ServiceRequest sr = new ServiceRequest();
    sr.setId("SR1");
    sr.setStatus(ServiceRequest.ServiceRequestStatus.ACTIVE);
    sr.setEncounter(new Reference("Encounter/E123456"));
    CodeableConcept code = new CodeableConcept();
    code.addCoding().setSystem("http://snomed.info/sct").setCode("386053000");
    sr.setCode(code);
    byEncounterBundle.addEntry().setResource(sr);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("ServiceRequest")))
        .thenReturn(byEncounterBundle);

    List<ServiceRequest> byEncounterResult =
        dataObject.getServiceRequestData(
            context, client, launchDetails, r4FhirData, daysFromNow(-5), daysFromNow(5));
    assertEquals(1, byEncounterResult.size());
    assertEquals(1, r4FhirData.getR4ServiceRequestCodes().size());

    // No encounter ID -> falls back to date-range filtering. One entry has occurrence[x] set,
    // the other doesn't (exercising extractServiceRequestDate's Meta.lastUpdated fallback).
    launchDetails.setEncounterId(null);

    Bundle byDateBundle = new Bundle();
    ServiceRequest withOccurrence = new ServiceRequest();
    withOccurrence.setId("SR2");
    withOccurrence.setStatus(ServiceRequest.ServiceRequestStatus.ACTIVE);
    withOccurrence.setOccurrence(new DateTimeType(now()));
    byDateBundle.addEntry().setResource(withOccurrence);

    ServiceRequest withoutOccurrence = new ServiceRequest();
    withoutOccurrence.setId("SR3");
    withoutOccurrence.setStatus(ServiceRequest.ServiceRequestStatus.ACTIVE);
    withoutOccurrence.getMeta().setLastUpdated(now());
    byDateBundle.addEntry().setResource(withoutOccurrence);

    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("ServiceRequest")))
        .thenReturn(byDateBundle);

    List<ServiceRequest> byDateResult =
        dataObject.getServiceRequestData(
            context, client, launchDetails, r4FhirData, daysFromNow(-5), daysFromNow(5));
    assertEquals(2, byDateResult.size());
  }

  // ==================== getPregnancyObservationData Tests ====================

  @Test
  public void getPregnancyObservationData_coversBundleAndNullBundleBranches() {
    Bundle bundle = createBundleWithObservations(2, "E123456", now());
    when(resourceDataMock.getResourceByPatientIdAndCode(
            eq(launchDetails),
            eq(client),
            eq(context),
            eq("Observation"),
            anyString(),
            anyString()))
        .thenReturn(bundle);

    List<Observation> result =
        dataObject.getPregnancyObservationData(
            context,
            client,
            launchDetails,
            r4FhirData,
            createEncounter("E123456", daysFromNow(-1), now()),
            daysFromNow(-5),
            daysFromNow(5));
    assertNotNull(result);

    when(resourceDataMock.getResourceByPatientIdAndCode(
            eq(launchDetails),
            eq(client),
            eq(context),
            eq("Observation"),
            anyString(),
            anyString()))
        .thenReturn(null);
    List<Observation> emptyResult =
        dataObject.getPregnancyObservationData(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertTrue(emptyResult.isEmpty());
  }

  // ==================== getPregnancyConditions Tests ====================

  @Test
  public void getPregnancyConditions_coversMatchingAndNullBundleBranches() {
    Condition pregnancyCondition = new Condition();
    pregnancyCondition.setId("preg-1");
    CodeableConcept code = new CodeableConcept();
    code.addCoding().setSystem("http://snomed.info/sct").setCode("77386006");
    pregnancyCondition.setCode(code);

    Condition nonMatchingCondition = new Condition();
    nonMatchingCondition.setId("preg-2");
    CodeableConcept otherCode = new CodeableConcept();
    otherCode.addCoding().setSystem("http://snomed.info/sct").setCode("99999");
    nonMatchingCondition.setCode(otherCode);

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(pregnancyCondition);
    bundle.addEntry().setResource(nonMatchingCondition);

    when(resourceDataMock.getResourceByPatientIdAndCode(
            eq(launchDetails),
            eq(client),
            eq(context),
            eq("Condition"),
            eq("77386006"),
            eq("http://snomed.info/sct")))
        .thenReturn(bundle);

    List<Condition> result =
        dataObject.getPregnancyConditions(
            context,
            client,
            launchDetails,
            r4FhirData,
            createEncounter("E123456", daysFromNow(-1), now()),
            daysFromNow(-5),
            daysFromNow(5));
    assertEquals(1, result.size());
    assertEquals("preg-1", result.get(0).getIdElement().getIdPart());

    when(resourceDataMock.getResourceByPatientIdAndCode(
            eq(launchDetails), eq(client), eq(context), eq("Condition"), anyString(), anyString()))
        .thenReturn(null);
    List<Condition> emptyResult =
        dataObject.getPregnancyConditions(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertTrue(emptyResult.isEmpty());
  }

  // ==================== getTravelObservationData Tests ====================
  // getTravelObservationData calls FhirContextInitializer.getResourceBundleByUrl (a static
  // helper) directly with the `client` argument, so it's exercised here via the client mock's
  // search().byUrl(...).returnBundle(...).execute() chain rather than resourceDataMock.

  @Test
  public void getTravelObservationData_coversFilteringAndNullBundleBranches() {
    Encounter encounter = createEncounter("E123456", daysFromNow(-1), now());

    CodeableConcept socialHistoryCategory = new CodeableConcept();
    socialHistoryCategory
        .addCoding()
        .setSystem("http://terminology.hl7.org/CodeSystem/observation-category")
        .setCode("social-history");

    Observation matching = new Observation();
    matching.setId("obs-travel");
    matching.setStatus(Observation.ObservationStatus.FINAL);
    matching.setEncounter(new Reference("Encounter/E123456"));
    matching.addCategory(socialHistoryCategory);

    Observation enteredInError = new Observation();
    enteredInError.setId("obs-error");
    enteredInError.setStatus(Observation.ObservationStatus.ENTEREDINERROR);
    enteredInError.addCategory(socialHistoryCategory);
    enteredInError.setEncounter(new Reference("Encounter/E123456"));

    CodeableConcept laboratoryCategory = new CodeableConcept();
    laboratoryCategory
        .addCoding()
        .setSystem("http://terminology.hl7.org/CodeSystem/observation-category")
        .setCode("laboratory");
    Observation wrongCategory = new Observation();
    wrongCategory.setId("obs-wrong-category");
    wrongCategory.setStatus(Observation.ObservationStatus.FINAL);
    wrongCategory.setEncounter(new Reference("Encounter/E123456"));
    wrongCategory.addCategory(laboratoryCategory);

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(matching);
    bundle.addEntry().setResource(enteredInError);
    bundle.addEntry().setResource(wrongCategory);
    when(client.search().byUrl(anyString()).returnBundle(Bundle.class).execute())
        .thenReturn(bundle);

    List<Observation> result =
        dataObject.getTravelObservationData(
            context, client, launchDetails, r4FhirData, encounter, daysFromNow(-5), daysFromNow(5));
    assertEquals(1, result.size());
    assertEquals("obs-travel", result.get(0).getIdElement().getIdPart());

    when(client.search().byUrl(anyString()).returnBundle(Bundle.class).execute()).thenReturn(null);
    List<Observation> emptyResult =
        dataObject.getTravelObservationData(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertTrue(emptyResult.isEmpty());
  }

  // ==================== getSocialHistoryObservationDataOccupation Tests ====================

  @Test
  public void getSocialHistoryObservationDataOccupation_coversFilteringAndNullBundleBranches() {
    Observation valid = new Observation();
    valid.setId("obs-occupation");
    valid.setStatus(Observation.ObservationStatus.FINAL);

    Observation enteredInError = new Observation();
    enteredInError.setId("obs-occupation-error");
    enteredInError.setStatus(Observation.ObservationStatus.ENTEREDINERROR);

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(valid);
    bundle.addEntry().setResource(enteredInError);
    when(client.search().byUrl(anyString()).returnBundle(Bundle.class).execute())
        .thenReturn(bundle);

    List<Observation> result =
        dataObject.getSocialHistoryObservationDataOccupation(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertEquals(1, result.size());
    assertEquals("obs-occupation", result.get(0).getIdElement().getIdPart());

    when(client.search().byUrl(anyString()).returnBundle(Bundle.class).execute()).thenReturn(null);
    List<Observation> emptyResult =
        dataObject.getSocialHistoryObservationDataOccupation(
            context, client, launchDetails, r4FhirData, null, daysFromNow(-5), daysFromNow(5));
    assertTrue(emptyResult.isEmpty());
  }

  // ==================== getCommonResources Tests ====================

  @Test
  public void getCommonResources_createsBundle() {
    Patient patient = new Patient();
    patient.setId("P123456");
    Encounter encounter = createEncounter("E123456", daysFromNow(-1), now());

    when(resourceDataMock.getResouceById(
            eq(launchDetails), eq(client), eq(context), eq("Patient"), eq("P123456")))
        .thenReturn(patient);
    // launchDetails carries encounterId "E123456", so getCommonResources resolves the
    // encounter via client.read()...execute(), not fhirContextInitializer.getResourceByPatientId.
    when(client.read().resource("Encounter").withId("E123456").execute()).thenReturn(encounter);

    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Condition")))
        .thenReturn(createBundleWithConditions(2, "E123456", now()));

    // 1 lab observation with a Quantity value, plus 1 with a CodeableConcept value so
    // r4FhirData.getLabResultValueObservations() is populated and its own bundle branch runs.
    Bundle observationBundle = createBundleWithObservations(1, "E123456", now());
    Observation codedValueObs = new Observation();
    codedValueObs.setId("obs-coded");
    codedValueObs.setStatus(Observation.ObservationStatus.FINAL);
    codedValueObs.setIssued(now());
    codedValueObs.setEncounter(new Reference("Encounter/E123456"));
    CodeableConcept obsValue = new CodeableConcept();
    obsValue.addCoding().setSystem("http://snomed.info/sct").setCode("10828004");
    codedValueObs.setValue(obsValue);
    observationBundle.addEntry().setResource(codedValueObs);
    when(resourceDataMock.getObservationByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Observation"), eq("laboratory")))
        .thenReturn(observationBundle);

    ServiceRequest sr = new ServiceRequest();
    sr.setId("SR1");
    sr.setStatus(ServiceRequest.ServiceRequestStatus.ACTIVE);
    sr.setEncounter(new Reference("Encounter/E123456"));
    Bundle serviceRequestBundle = new Bundle();
    serviceRequestBundle.addEntry().setResource(sr);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("ServiceRequest")))
        .thenReturn(serviceRequestBundle);

    Bundle result =
        dataObject.getCommonResources(
            r4FhirData, daysFromNow(-5), daysFromNow(5), launchDetails, client, context);

    assertNotNull(result);
    assertEquals(patient, r4FhirData.getPatient());
    assertEquals(encounter, r4FhirData.getEncounter());
    // patient(1) + encounter(1) + conditions(2) + lab observations(2) + value observations(1)
    // + service request(1)
    assertEquals(8, result.getEntry().size());
  }

  @Test
  public void getCommonResources_degradesGracefullyWhenDependenciesThrow() {
    // Phase 1: Patient lookup throws -> patient stays unset, but the rest of the pipeline
    // (encounter) still completes normally.
    when(resourceDataMock.getResouceById(
            eq(launchDetails), eq(client), eq(context), eq("Patient"), eq("P123456")))
        .thenThrow(new RuntimeException("simulated Patient failure"));
    Encounter encounter = createEncounter("E123456", daysFromNow(-1), now());
    when(client.read().resource("Encounter").withId("E123456").execute()).thenReturn(encounter);

    Bundle result1 =
        dataObject.getCommonResources(
            r4FhirData, daysFromNow(-5), daysFromNow(5), launchDetails, client, context);
    assertNotNull(result1);
    assertNull(r4FhirData.getPatient());
    assertEquals(encounter, r4FhirData.getEncounter());

    // Phase 2 (fresh R4FhirData): Patient now succeeds, but checkSkipResource() (called before
    // getEncounterData's own internal try/catch, so the exception escapes it entirely) plus
    // the Condition, Observation, and ServiceRequest lookups all throw. getCommonResources
    // still degrades gracefully and returns a bundle containing just the patient.
    R4FhirData r4FhirData2 = new R4FhirData();
    Patient patient = new Patient();
    patient.setId("P123456");
    when(resourceDataMock.getResouceById(
            eq(launchDetails), eq(client), eq(context), eq("Patient"), eq("P123456")))
        .thenReturn(patient);
    when(resourceDataMock.checkSkipResource(anyString(), any(FhirClient.class)))
        .thenThrow(new RuntimeException("simulated checkSkipResource failure"));
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Condition")))
        .thenThrow(new RuntimeException("simulated Condition failure"));
    when(resourceDataMock.getObservationByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("Observation"), eq("laboratory")))
        .thenThrow(new RuntimeException("simulated Observation failure"));
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("ServiceRequest")))
        .thenThrow(new RuntimeException("simulated ServiceRequest failure"));

    Bundle result2 =
        dataObject.getCommonResources(
            r4FhirData2, daysFromNow(-5), daysFromNow(5), launchDetails, client, context);
    assertNotNull(result2);
    assertEquals(patient, r4FhirData2.getPatient());
    assertNull(r4FhirData2.getEncounter());
    assertEquals(1, result2.getEntry().size());
  }

  // ==================== loadMedicationsData Tests ====================

  @Test
  public void loadMedicationsData_withReferenceMedications_resolvesRemoteAndContainedReferences() {
    Encounter encounter = createEncounter("E123456", daysFromNow(-1), now());

    // MedicationAdministration references a remote Medication resource.
    MedicationAdministration medAdmin = new MedicationAdministration();
    medAdmin.setId("MA1");
    medAdmin.setStatus(MedicationAdministration.MedicationAdministrationStatus.COMPLETED);
    medAdmin.setContext(new Reference("Encounter/E123456"));
    medAdmin.setMedication(new Reference("Medication/M123"));

    // A second MedicationAdministration with a non-Reference (CodeableConcept) medication ->
    // processMedicationReferences() early-returns for it without any remote lookup.
    MedicationAdministration medAdminWithCodedMedication = new MedicationAdministration();
    medAdminWithCodedMedication.setId("MA2");
    medAdminWithCodedMedication.setStatus(
        MedicationAdministration.MedicationAdministrationStatus.COMPLETED);
    medAdminWithCodedMedication.setContext(new Reference("Encounter/E123456"));
    CodeableConcept codedMedication = new CodeableConcept();
    codedMedication
        .addCoding()
        .setSystem("http://www.nlm.nih.gov/research/umls/rxnorm")
        .setCode("197361");
    medAdminWithCodedMedication.setMedication(codedMedication);

    Bundle medAdminBundle = new Bundle();
    medAdminBundle.addEntry().setResource(medAdmin);
    medAdminBundle.addEntry().setResource(medAdminWithCodedMedication);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationAdministration")))
        .thenReturn(medAdminBundle);

    Medication remoteMedication = new Medication();
    remoteMedication.setId("M123");
    when(resourceDataMock.getResouceById(
            eq(launchDetails), eq(client), eq(context), eq("Medication"), eq("Medication/M123")))
        .thenReturn(remoteMedication);

    // MedicationRequest references a contained Medication resource (no remote lookup needed).
    MedicationRequest medReq = new MedicationRequest();
    medReq.setId("MR1");
    medReq.setStatus(MedicationRequest.MedicationRequestStatus.ACTIVE);
    medReq.setEncounter(new Reference("Encounter/E123456"));
    Medication containedMedication = new Medication();
    containedMedication.setId("#med1");
    medReq.addContained(containedMedication);
    medReq.setMedication(new Reference("#med1"));
    Bundle medReqBundle = new Bundle();
    medReqBundle.addEntry().setResource(medReq);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationRequest")))
        .thenReturn(medReqBundle);

    Bundle resultBundle = new Bundle();
    dataObject.loadMedicationsData(
        context,
        client,
        launchDetails,
        r4FhirData,
        encounter,
        resultBundle,
        daysFromNow(-5),
        daysFromNow(5));

    // 2 MedicationAdministrations + 1 remote Medication + 1 MedicationRequest = 4 entries.
    // The contained Medication is not added again since it's already inline in the resource.
    assertEquals(4, resultBundle.getEntry().size());
    assertTrue(r4FhirData.getMedicationList().contains(remoteMedication));
  }

  @Test
  public void loadMedicationsData_handlesEmptyBundlesAndDownstreamExceptions() {
    // Phase 1: no medications found for either type -> nothing added to the result bundle.
    Bundle medBundle = new Bundle();
    MedicationAdministration medAdmin = new MedicationAdministration();
    medAdmin.setId("M1");
    medAdmin.setStatus(MedicationAdministration.MedicationAdministrationStatus.COMPLETED);
    medBundle.addEntry().setResource(medAdmin);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationAdministration")))
        .thenReturn(medBundle);
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationRequest")))
        .thenReturn(new Bundle());

    Bundle resultBundle1 = new Bundle();
    dataObject.loadMedicationsData(
        context,
        client,
        launchDetails,
        r4FhirData,
        null,
        resultBundle1,
        daysFromNow(-5),
        daysFromNow(5));
    assertTrue(resultBundle1.getEntry().isEmpty());

    // Phase 2: both lookups throw -> loadMedicationsData still completes without propagating.
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationAdministration")))
        .thenThrow(new RuntimeException("simulated MedicationAdministration failure"));
    when(resourceDataMock.getResourceByPatientId(
            eq(launchDetails), eq(client), eq(context), eq("MedicationRequest")))
        .thenThrow(new RuntimeException("simulated MedicationRequest failure"));

    Bundle resultBundle2 = new Bundle();
    dataObject.loadMedicationsData(
        context,
        client,
        launchDetails,
        r4FhirData,
        null,
        resultBundle2,
        daysFromNow(-5),
        daysFromNow(5));
    assertTrue(resultBundle2.getEntry().isEmpty());
  }

  // ==================== loadPractitionersLocationAndOrganization Tests ====================

  @Test
  public void loadPractitionersLocationAndOrganization_withFullEncounter_loadsAllResources() {
    Encounter encounter = createEncounter("E123456", daysFromNow(-1), now());
    encounter.addParticipant().setIndividual(new Reference("Practitioner/PRACT1"));
    encounter.setServiceProvider(new Reference("Organization/ORG1"));
    encounter.addLocation().setLocation(new Reference("Location/LOC1"));

    Practitioner pract = new Practitioner();
    pract.setId("PRACT1");
    pract.addName().setFamily("Smith");
    when(resourceDataMock.getResouceById(
            eq(launchDetails), eq(client), eq(context), eq("Practitioner"), eq("PRACT1")))
        .thenReturn(pract);

    Organization org = new Organization();
    org.setId("ORG1");
    org.setName("Test Hospital");
    when(resourceDataMock.getResouceById(
            eq(launchDetails), eq(client), eq(context), eq("Organization"), eq("ORG1")))
        .thenReturn(org);

    Location location = new Location();
    location.setId("LOC1");
    location.setAddress(new Address().addLine("123 Main St").setCity("Anytown"));
    when(resourceDataMock.getResouceById(
            eq(launchDetails), eq(client), eq(context), eq("Location"), eq("LOC1")))
        .thenReturn(location);

    Bundle bundle = new Bundle();
    dataObject.loadPractitionersLocationAndOrganization(
        context,
        client,
        launchDetails,
        r4FhirData,
        encounter,
        bundle,
        daysFromNow(-5),
        daysFromNow(5));

    assertEquals(1, r4FhirData.getPractitionersList().size());
    assertEquals(org, r4FhirData.getOrganization());
    assertEquals(location, r4FhirData.getLocation());
    assertEquals(1, r4FhirData.getLocationList().size());
    // practitioner(1) + organization(1) + location(1) = 3 entries
    assertEquals(3, bundle.getEntry().size());
  }

  @Test
  public void loadPractitionersLocationAndOrganization_skipsIncompleteReferences() {
    // A Location without an address is looked up but not added.
    Encounter encounterWithBadLocation = createEncounter("E123456", daysFromNow(-1), now());
    encounterWithBadLocation.addLocation().setLocation(new Reference("Location/LOC1"));
    Location locationWithoutAddress = new Location();
    locationWithoutAddress.setId("LOC1");
    when(resourceDataMock.getResouceById(
            eq(launchDetails), eq(client), eq(context), eq("Location"), eq("LOC1")))
        .thenReturn(locationWithoutAddress);

    Bundle bundle1 = new Bundle();
    dataObject.loadPractitionersLocationAndOrganization(
        context,
        client,
        launchDetails,
        r4FhirData,
        encounterWithBadLocation,
        bundle1,
        daysFromNow(-5),
        daysFromNow(5));
    assertNull(r4FhirData.getLocation());
    assertTrue(bundle1.getEntry().isEmpty());

    // A service-provider Reference with only a display (no reference element) is skipped
    // entirely -- no Organization lookup is even attempted.
    Encounter encounterWithBadServiceProvider = createEncounter("E123456", daysFromNow(-1), now());
    encounterWithBadServiceProvider.setServiceProvider(
        new Reference().setDisplay("Some Organization"));

    Bundle bundle2 = new Bundle();
    dataObject.loadPractitionersLocationAndOrganization(
        context,
        client,
        launchDetails,
        r4FhirData,
        encounterWithBadServiceProvider,
        bundle2,
        daysFromNow(-5),
        daysFromNow(5));
    assertNull(r4FhirData.getOrganization());
    assertTrue(bundle2.getEntry().isEmpty());
    verify(resourceDataMock, never())
        .getResouceById(any(), any(), any(), eq("Organization"), any());
  }

  @Test
  public void
      loadPractitionersLocationAndOrganization_withNullEncounter_returnsWithoutProcessing() {
    Bundle bundle = new Bundle();
    dataObject.loadPractitionersLocationAndOrganization(
        context, client, launchDetails, r4FhirData, null, bundle, daysFromNow(-5), daysFromNow(5));

    assertTrue(bundle.getEntry().isEmpty());
  }

  // ==================== Edge Cases & Error Handling ====================

  @Test
  public void constructors_createInstances() {
    assertNotNull(new R4ResourcesData());
    assertNotNull(new R4ResourcesData(resourceDataMock));
  }

  @Test
  public void modelIntegration_launchDetailsAndR4FhirDataSettersWork() {
    Patient patient = new Patient();
    patient.setId("P1");
    r4FhirData.setPatient(patient);
    assertEquals(patient, r4FhirData.getPatient());

    Encounter encounter = new Encounter();
    encounter.setId("E1");
    r4FhirData.setEncounter(encounter);
    assertEquals(encounter, r4FhirData.getEncounter());

    launchDetails.setLaunchPatientId("P999");
    assertEquals("P999", launchDetails.getLaunchPatientId());

    launchDetails.setEncounterId("E999");
    assertEquals("E999", launchDetails.getEncounterId());
  }
}
