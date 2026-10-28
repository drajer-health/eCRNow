package com.drajer.bsa.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.drajer.bsa.exceptions.InvalidLaunchContext;
import com.drajer.bsa.exceptions.InvalidNotification;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PatientLaunchContext;
import com.drajer.test.util.TestUtils;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.StringType;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

@RunWith(MockitoJUnitRunner.class)
public class SubscriptionUtilsTest {

  private Bundle bundle;

  private MockHttpServletRequest mockHttpServletRequest;

  private MockHttpServletResponse mockHttpServletResponse;

  @Before
  public void setUp() throws Exception {
    bundle =
        TestUtils.loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");
    Bundle.BundleEntryComponent encounterEntry = new Bundle.BundleEntryComponent();
    encounterEntry.setResource(bundle);
    bundle.addEntry(encounterEntry);
    bundle.setType(BundleType.HISTORY);
    mockHttpServletRequest = new MockHttpServletRequest();
    mockHttpServletResponse = new MockHttpServletResponse();
  }

  @Test
  public void testGetNotificationContext() throws Exception {

    mockHttpServletRequest.addHeader("X-Request-ID", "ecrunittest_id");
    mockHttpServletRequest.addHeader("X-Correlation-ID", "ecrUnitTestCorrelationID");
    PatientLaunchContext launchContext = new PatientLaunchContext();

    NotificationContext notificationContext =
        SubscriptionUtils.getNotificationContext(
            bundle, mockHttpServletRequest, mockHttpServletResponse, false, false, launchContext);
    assertNotNull(notificationContext);
  }

  @Test
  public void getFhirServerUrl() {
    Reference reference = new Reference();
    String fhirServerUrl = SubscriptionUtils.getFhirServerUrl(reference);
    assertNull(fhirServerUrl);
  }

  @Test
  public void getNotificationContextWithoutPeriod() throws Exception {
    Bundle localBundle =
        TestUtils.loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");

    Bundle.BundleEntryComponent encounterEntry = new Bundle.BundleEntryComponent();
    encounterEntry.setResource(localBundle);
    localBundle.addEntry(encounterEntry);
    localBundle.setType(BundleType.HISTORY);
    PatientLaunchContext launchContext = new PatientLaunchContext();

    NotificationContext notificationContext =
        SubscriptionUtils.getNotificationContext(
            localBundle,
            mockHttpServletRequest,
            mockHttpServletResponse,
            false,
            false,
            launchContext);
    assertNotNull(notificationContext);
  }

  @Test
  public void getNotificationContextWithEmptyBundle()
      throws InvalidLaunchContext, InvalidNotification {
    Bundle localBundle = new Bundle();
    PatientLaunchContext launchContext = new PatientLaunchContext();

    try {
      SubscriptionUtils.getNotificationContext(
          localBundle,
          mockHttpServletRequest,
          mockHttpServletResponse,
          false,
          false,
          launchContext);
    } catch (InvalidNotification e) {
      assertTrue(true);
    }
  }

  @Test
  public void testGetNotificationContextForElse() throws Exception {
    MockHttpServletRequest localMockHttpServletRequest = new MockHttpServletRequest();
    List<Map<String, String>> headersList = new ArrayList<>();
    headersList.add(createHeaderMap("X-Correlation-ID", "ecrUnitTestCorrelationID"));
    headersList.add(createHeaderMap("X-Request-ID", "ecrunittest_id"));

    for (Map<String, String> headers : headersList) {
      for (Map.Entry<String, String> entry : headers.entrySet()) {
        localMockHttpServletRequest.addHeader(entry.getKey(), entry.getValue());
      }
      PatientLaunchContext launchContext = new PatientLaunchContext();
      NotificationContext notificationContext =
          SubscriptionUtils.getNotificationContext(
              bundle,
              localMockHttpServletRequest,
              mockHttpServletResponse,
              false,
              false,
              launchContext);
      localMockHttpServletRequest.removeHeader("X-Correlation-ID");
      localMockHttpServletRequest.removeHeader("X-Request-ID");

      assertNotNull(notificationContext);
    }
  }

  private Map<String, String> createHeaderMap(String key, String value) {
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(key, value);
    return headerMap;
  }

  @Test
  public void getPatientId() {
    Patient patient = new Patient();
    String patientId = SubscriptionUtils.getPatientId(patient);
    assertNull(patientId);
  }

  @Test
  public void testGetNotificationContextWithInvalidBundle() throws Exception {
    Bundle localBundle = TestUtils.loadBundleFromFile("Bsa/NotificationBundle.json");
    Bundle.BundleEntryComponent encounterEntry = new Bundle.BundleEntryComponent();
    encounterEntry.setResource(localBundle);
    localBundle.addEntry(encounterEntry);
    localBundle.setType(BundleType.HISTORY);
    PatientLaunchContext launchContext = new PatientLaunchContext();
    try {
      SubscriptionUtils.getNotificationContext(
          localBundle,
          mockHttpServletRequest,
          mockHttpServletResponse,
          false,
          false,
          launchContext);
    } catch (InvalidNotification e) {
      assertTrue(true);
    }
  }

  @Test
  public void getNotificationContext_readsTheClosedEncounterFromTheNotification() throws Exception {
    Bundle closed =
        TestUtils.loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithPeriod.json");
    Encounter encounter = (Encounter) closed.getEntry().get(1).getResource();

    NotificationContext context =
        SubscriptionUtils.getNotificationContext(
            closed,
            mockHttpServletRequest,
            mockHttpServletResponse,
            false,
            false,
            new PatientLaunchContext());

    assertEquals("Encounter", context.getNotificationResourceType());
    assertEquals("551683b3-1477-41d1-b58e-32fe8b0047b0", context.getNotificationResourceId());
    assertEquals("46db3334-dbf5-43f3-868f-93ae0883cce1", context.getPatientId());
    assertEquals("encounter-close", context.getTriggerEvent());
    assertEquals(encounter.getPeriod().getStart(), context.getEncounterStartTime());
    assertEquals(encounter.getPeriod().getEnd(), context.getEncounterEndTime());
    // Known bug in NotificationContext.setEncounterClass: the parameter is "encounterclass"
    // (small c) but the body does "this.encounterClass = encounterClass", so the value is
    // never stored. SubscriptionUtils still runs both branches (VR -> "AMB", IMP -> "IMP"),
    // but getEncounterClass() always returns null.
    assertNull(context.getEncounterClass());

    encounter.getClass_().setCode("IMP");
    assertNull(
        SubscriptionUtils.getNotificationContext(
                closed,
                mockHttpServletRequest,
                mockHttpServletResponse,
                false,
                false,
                new PatientLaunchContext())
            .getEncounterClass());
  }

  @Test
  public void getNotificationContext_rejectsNotificationsThatCannotBeProcessed() {
    List<Consumer<Bundle>> breakers =
        List.of(
            b -> b.setType(BundleType.COLLECTION),
            b ->
                ((Parameters) b.getEntryFirstRep().getResource())
                    .getParameter("topic")
                    .setValue(new CanonicalType("http://example.org/SubscriptionTopic/unknown")),
            b ->
                ((Parameters) b.getEntryFirstRep().getResource())
                    .getParameter("type")
                    .setValue(new CodeType("handshake")),
            b ->
                ((Parameters) b.getEntryFirstRep().getResource())
                    .getParameter("subscription")
                    .setValue(new Reference("http://example.org/fhir/Basic/1")),
            b ->
                b.getEntry()
                    .get(1)
                    .setResource(
                        TestUtils.loadResourceDataFromFile(
                            Patient.class, "R4/Patient/Patient.json")));

    for (Consumer<Bundle> breakIt : breakers) {
      Bundle notification =
          TestUtils.loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithPeriod.json");
      breakIt.accept(notification);

      assertThrows(
          InvalidNotification.class,
          () ->
              SubscriptionUtils.getNotificationContext(
                  notification,
                  mockHttpServletRequest,
                  mockHttpServletResponse,
                  false,
                  false,
                  new PatientLaunchContext()));
    }
  }

  // =====================================================================================
  // New tests added for more coverage. The existing tests above are unchanged.
  // =====================================================================================

  private static final String CLOSED_ENCOUNTER_BUNDLE =
      "Bsa/NotificationBundleEncounterCloseWithPeriod.json";

  @Test
  public void privateConstructor_canBeInvokedByReflection() throws Exception {
    Constructor<SubscriptionUtils> constructor = SubscriptionUtils.class.getDeclaredConstructor();
    assertTrue(Modifier.isPrivate(constructor.getModifiers()));
    constructor.setAccessible(true);
    assertNotNull(constructor.newInstance());
  }

  // ---------------- trigger event: relaunch / reprocess ----------------

  @Test
  public void getNotificationContext_relaunch_addsRelaunchId() throws Exception {
    NotificationContext nc = process(loadClosed(), true, false, new PatientLaunchContext());
    assertTrue(nc.getTriggerEvent().startsWith("encounter-close|relaunch-id:"));
  }

  @Test
  public void getNotificationContext_reprocessWithForceReprocessingFalse_addsUuid()
      throws Exception {
    PatientLaunchContext lc = launchContext(Map.of("forceReprocessing", "false"));
    NotificationContext nc = process(loadClosed(), false, true, lc);

    assertTrue(nc.getTriggerEvent().startsWith("encounter-close|reprocessed:"));
    assertNotNull(nc.getEhrLaunchContext());
  }

  @Test
  public void getNotificationContext_reprocessWithForceReprocessingTrue_isReprocessed()
      throws Exception {
    PatientLaunchContext lc = launchContext(Map.of("forceReprocessing", "true"));
    NotificationContext nc = process(loadClosed(), false, true, lc);
    assertEquals("encounter-close|reprocessed", nc.getTriggerEvent());
  }

  @Test
  public void getNotificationContext_reprocessWithoutForceReprocessingKey_isReprocessed()
      throws Exception {
    PatientLaunchContext lc = launchContext(Map.of("someOtherKey", "value"));
    NotificationContext nc = process(loadClosed(), false, true, lc);
    assertEquals("encounter-close|reprocessed", nc.getTriggerEvent());
  }

  @Test
  public void getNotificationContext_reprocessWithoutEhrLaunchContext_isReprocessed()
      throws Exception {
    NotificationContext nc = process(loadClosed(), false, true, new PatientLaunchContext());
    assertEquals("encounter-close|reprocessed", nc.getTriggerEvent());
  }

  // ---------------- ehr launch context ----------------

  @Test
  public void getNotificationContext_nullLaunchContext_isAllowedWhenNotReprocessing()
      throws Exception {
    NotificationContext nc = process(loadClosed(), false, false, null);
    assertNull(nc.getEhrLaunchContext());
  }

  @Test
  public void getNotificationContext_emptyEhrLaunchContext_isNotStored() throws Exception {
    NotificationContext nc = process(loadClosed(), false, false, launchContext(new HashMap<>()));
    assertNull(nc.getEhrLaunchContext());
  }

  @Test
  public void getNotificationContext_ehrLaunchContext_isStoredAsJson() throws Exception {
    NotificationContext nc =
        process(loadClosed(), false, false, launchContext(Map.of("encounterId", "123")));
    assertEquals("{\"encounterId\":\"123\"}", nc.getEhrLaunchContext());
  }

  @Test
  @SuppressWarnings({"unchecked", "rawtypes"})
  public void getNotificationContext_ehrLaunchContextThatCannotBeSerialized_throws() {
    // A plain Object has no properties, so Jackson throws a JsonProcessingException,
    // which SubscriptionUtils turns into InvalidNotification (the catch block).
    Map<String, Object> notJson = new HashMap<>();
    notJson.put("bad", new Object());
    PatientLaunchContext lc = launchContext((Map<String, String>) (Map) notJson);
    Bundle closed = loadClosed();

    assertThrows(InvalidNotification.class, () -> process(closed, false, false, lc));
  }

  // ---------------- request / correlation headers ----------------

  @Test
  public void getNotificationContext_emptyRequestIdHeader_usesCorrelationIdForBoth()
      throws Exception {
    mockHttpServletRequest.addHeader("X-Request-ID", "");
    mockHttpServletRequest.addHeader("X-Correlation-ID", "corr-1");

    NotificationContext nc = process(loadClosed(), false, false, new PatientLaunchContext());

    assertEquals("corr-1", nc.getxRequestId());
    assertEquals("corr-1", nc.getxCorrelationId());
  }

  @Test
  public void getNotificationContext_emptyCorrelationIdHeader_usesRequestIdForBoth()
      throws Exception {
    mockHttpServletRequest.addHeader("X-Request-ID", "req-1");
    mockHttpServletRequest.addHeader("X-Correlation-ID", "");

    NotificationContext nc = process(loadClosed(), false, false, new PatientLaunchContext());

    assertEquals("req-1", nc.getxRequestId());
    assertEquals("req-1", nc.getxCorrelationId());
  }

  @Test
  public void getNotificationContext_bothHeadersEmpty_generatesTheSameGuid() throws Exception {
    mockHttpServletRequest.addHeader("X-Request-ID", "");
    mockHttpServletRequest.addHeader("X-Correlation-ID", "");

    NotificationContext nc = process(loadClosed(), false, false, new PatientLaunchContext());

    assertNotNull(nc.getxRequestId());
    assertEquals(nc.getxRequestId(), nc.getxCorrelationId());
  }

  // ---------------- encounter period and class ----------------

  @Test
  public void getNotificationContext_encounterWithStartButNoEnd() throws Exception {
    Bundle closed = loadClosed();
    Encounter enc = (Encounter) closed.getEntry().get(1).getResource();
    enc.getPeriod().setEnd(null);

    NotificationContext nc = process(closed, false, false, new PatientLaunchContext());

    assertEquals(enc.getPeriod().getStart(), nc.getEncounterStartTime());
    assertNull(nc.getEncounterEndTime());
  }

  @Test
  public void getNotificationContext_encounterClassVariants_doNotFail() throws Exception {
    // AMB and HH are ambulatory; a class without a code and no class at all are inpatient.
    // (getEncounterClass() cannot be asserted: see the NotificationContext setter bug above.)
    for (String code : new String[] {"AMB", "HH", null}) {
      Bundle closed = loadClosed();
      Encounter enc = (Encounter) closed.getEntry().get(1).getResource();
      enc.getClass_().setCode(code);
      assertNotNull(process(closed, false, false, new PatientLaunchContext()));
    }

    Bundle noClass = loadClosed();
    ((Encounter) noClass.getEntry().get(1).getResource()).setClass_(null);
    assertNotNull(process(noClass, false, false, new PatientLaunchContext()));
  }

  // ---------------- invalid notifications ----------------

  @Test
  public void getNotificationContext_nullBundle_throws() {
    assertThrows(
        InvalidNotification.class, () -> process(null, false, false, new PatientLaunchContext()));
  }

  @Test
  public void getNotificationContext_firstEntryNotParameters_throws() {
    Bundle closed = loadClosed();
    closed.getEntryFirstRep().setResource(new Patient());

    assertThrows(
        InvalidNotification.class, () -> process(closed, false, false, new PatientLaunchContext()));
  }

  @Test
  public void getNotificationContext_onlyOneEntry_throws() {
    Bundle closed = loadClosed();
    closed.getEntry().remove(1);

    assertThrows(
        InvalidNotification.class, () -> process(closed, false, false, new PatientLaunchContext()));
  }

  @Test
  public void getNotificationContext_missingTypeSubscriptionOrTopic_throws() {
    // Each parameter is replaced by a value of the wrong type, so it is read as null.
    for (String name : new String[] {"type", "subscription", "topic"}) {
      Bundle closed = loadClosed();
      ((Parameters) closed.getEntryFirstRep().getResource())
          .getParameter(name)
          .setValue(new StringType("wrong type"));

      assertThrows(
          name,
          InvalidNotification.class,
          () -> process(closed, false, false, new PatientLaunchContext()));
    }
  }

  @Test
  public void getNotificationContext_entryWithoutResource_throwsResourceNotFound() {
    Bundle closed = loadClosed();
    closed.getEntry().get(1).setResource(null);

    InvalidNotification ex =
        assertThrows(
            InvalidNotification.class,
            () -> process(closed, false, false, new PatientLaunchContext()));
    assertTrue(ex.getMessage().startsWith("Resource not found for type: Encounter"));
  }

  @Test
  public void getNotificationContext_fhirServerUrlTooShort_throws() {
    Bundle closed = loadClosed();
    ((Parameters) closed.getEntryFirstRep().getResource())
        .getParameter("subscription")
        .setValue(new Reference("a/Subscription/1"));

    InvalidNotification ex =
        assertThrows(
            InvalidNotification.class,
            () -> process(closed, false, false, new PatientLaunchContext()));
    assertTrue(ex.getMessage().endsWith("Url Value : a"));
  }

  // ---------------- getFhirServerUrl ----------------

  @Test
  public void getFhirServerUrl_allPaths() {
    assertNull(SubscriptionUtils.getFhirServerUrl(null));
    assertNull(
        SubscriptionUtils.getFhirServerUrl(new Reference("http://example.org/fhir/Basic/1")));
    assertEquals(
        "https://example.org/fhir/r4",
        SubscriptionUtils.getFhirServerUrl(
            new Reference("https://example.org/fhir/r4/Subscription/encounter-close")));
    // "/Subscription" splits into an empty array -> null
    assertNull(SubscriptionUtils.getFhirServerUrl(new Reference("/Subscription")));
  }

  // ---------------- getPatientId ----------------

  @Test
  public void getPatientId_allPaths() {
    assertNull(SubscriptionUtils.getPatientId(null));

    Encounter withSubject = new Encounter();
    withSubject.setSubject(new Reference("Patient/patient-123"));
    assertEquals("patient-123", SubscriptionUtils.getPatientId(withSubject));

    assertNull(SubscriptionUtils.getPatientId(new Encounter()));
  }

  // ---------------- helpers for the new tests ----------------

  private Bundle loadClosed() {
    return TestUtils.loadBundleFromFile(CLOSED_ENCOUNTER_BUNDLE);
  }

  private NotificationContext process(
      Bundle notification, boolean relaunch, boolean reprocess, PatientLaunchContext lc)
      throws InvalidLaunchContext, InvalidNotification {
    return SubscriptionUtils.getNotificationContext(
        notification, mockHttpServletRequest, mockHttpServletResponse, relaunch, reprocess, lc);
  }

  private static PatientLaunchContext launchContext(Map<String, String> ehrLaunchContext) {
    PatientLaunchContext lc = new PatientLaunchContext();
    lc.setEhrLaunchContext(ehrLaunchContext);
    return lc;
  }
}
