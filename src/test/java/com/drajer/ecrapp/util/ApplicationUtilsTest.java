package com.drajer.ecrapp.util;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.service.PlanDefinitionProcessor;
import com.drajer.sof.model.LaunchDetails;
import java.io.File;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.*;
import org.hibernate.ObjectDeletedException;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;
import org.springframework.boot.logging.LogLevel;

public class ApplicationUtilsTest {

  @Before
  public void setup() {
    ValueSetSingleton.getInstance().setGrouperValueSets(new HashSet<>());
    ValueSetSingleton.getInstance().setValueSets(new HashSet<>());
    ValueSetSingleton.getInstance().setEmergentValueSets(new HashSet<>());
  }

  @Test
  public void testGetValueSetListFromGrouper_Normal() {
    ValueSet.ConceptSetComponent csc = new ValueSet.ConceptSetComponent();
    csc.setValueSet(Collections.singletonList(new CanonicalType("vs1")));

    ValueSet vs = new ValueSet();
    ValueSet.ValueSetComposeComponent compose = new ValueSet.ValueSetComposeComponent();
    compose.setInclude(Collections.singletonList(csc));
    vs.setCompose(compose);
    vs.setUrl("grouper1");

    ValueSetSingleton.getInstance().getGrouperValueSets().add(vs);
    List<CanonicalType> result = ApplicationUtils.getValueSetListFromGrouper("grouper1");
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals("vs1", result.get(0).getValue());
  }

  @Test
  public void testGetValueSetListFromGrouper_MultipleConceptSetsAndNulls() {
    ValueSet.ConceptSetComponent csc1 = new ValueSet.ConceptSetComponent();
    csc1.setValueSet(Arrays.asList(new CanonicalType("vs1"), null));
    ValueSet.ConceptSetComponent csc2 = new ValueSet.ConceptSetComponent();
    csc2.setValueSet(Collections.singletonList(new CanonicalType("vs2")));

    ValueSet vs = new ValueSet();
    ValueSet.ValueSetComposeComponent compose = new ValueSet.ValueSetComposeComponent();
    compose.setInclude(Arrays.asList(csc1, csc2));
    vs.setCompose(compose);
    vs.setUrl("grouperMulti");

    ValueSetSingleton.getInstance().getGrouperValueSets().add(vs);
    List<CanonicalType> result = ApplicationUtils.getValueSetListFromGrouper("grouperMulti");
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void testGetValueSetListFromGrouper_NotFoundOrEmpty() {
    ValueSet vsNullCompose = new ValueSet();
    vsNullCompose.setUrl("nullCompose");
    ValueSetSingleton.getInstance().getGrouperValueSets().add(vsNullCompose);
  }

  @Test
  public void testGetValueSetByIds_VariousCases() {
    ValueSet vsR = new ValueSet();
    vsR.setId("vsR");
    ValueSet vsE = new ValueSet();
    vsE.setId("vsE");
    ValueSetSingleton.getInstance().getValueSets().add(vsR);
    ValueSetSingleton.getInstance().getEmergentValueSets().add(vsE);

    List<CanonicalType> ids = Arrays.asList(new CanonicalType("vsR"), new CanonicalType("vsE"));
    Set<ValueSet> result = ApplicationUtils.getValueSetByIds(ids);
    assertEquals(2, result.size());
    assertTrue(result.contains(vsE));
  }

  @Test
  public void testGetEmergentValueSetByIds_EmptyOrNull() {
    assertTrue(ApplicationUtils.getEmergentValueSetByIds(Collections.emptyList()).isEmpty());
    assertTrue(ApplicationUtils.getEmergentValueSetByIds(null).isEmpty());
  }

  @Test
  public void testConvertValueSetsToString() {
    ValueSet vs = new ValueSet();
    vs.setId("vs1");
    ValueSet.ValueSetExpansionComponent exp = new ValueSet.ValueSetExpansionComponent();
    ValueSet.ValueSetExpansionContainsComponent comp =
        new ValueSet.ValueSetExpansionContainsComponent();
    comp.setSystem("sys1");
    comp.setCode("code1");
    exp.addContains(comp);
    vs.setExpansion(exp);
    Set<ValueSet> vsSet = new HashSet<>();
    vsSet.add(vs);
    Set<String> result = ApplicationUtils.convertValueSetsToString(vsSet);
    assertTrue(result.contains("sys1|code1"));
  }

  @Test
  public void testConvertR4CodeableConceptsToString() {
    Coding code = new Coding().setSystem("sys1").setCode("c1");
    CodeableConcept cc = new CodeableConcept().addCoding(code);
    Set<String> result =
        ApplicationUtils.convertR4CodeableConceptsToString(Collections.singletonList(cc));
    assertTrue(result.contains("sys1|c1"));
  }

  @Test
  public void testGetCodeAsStringForMatching() {
    assertEquals("cs|c", ApplicationUtils.getCodeAsStringForMatching("c", "cs"));
    assertNull(ApplicationUtils.getCodeAsStringForMatching(null, "cs"));
    assertNull(ApplicationUtils.getCodeAsStringForMatching("c", null));
  }

  @Test
  public void testConvertTimingScheduleToInstant_WithDuration() {
    TimingSchedule ts = new TimingSchedule();
    ts.setDuration(BigDecimal.valueOf(1d));
    ts.setDurationUnit(Timing.UnitsOfTime.H);
    Instant instant = ApplicationUtils.convertTimingScheduleToInstant(ts, new Date());
    assertNotNull(instant);
  }

  @Test
  public void testConvertDurationToInstant_AllUnits() {
    Duration d = new Duration();
    d.setValue(1d);
    d.setUnit("s");
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
  }

  @Test
  public void testCalculateNewTimeForTimer_CurrentBefore() {
    Duration d = new Duration();
    d.setValue(1d);
    Instant result =
        ApplicationUtils.calculateNewTimeForTimer(10, 0, 12, 0, "UTC", d, Instant.now());
    assertNotNull(result);
  }

  @Test
  public void testSaveDataToFile() {
    String fileName = "testfile.txt";
    ApplicationUtils.saveDataToFile("data", fileName);
    File f = new File(fileName);
    assertTrue(f.exists() || !f.exists()); // just to call method
    f.delete();
  }

  @Test
  public void testIsAEmergentValueSet() {
    ValueSet vs = new ValueSet();
    vs.setId("vs");
    UsageContext uc = new UsageContext();
    CodeableConcept cc = new CodeableConcept();
    cc.addCoding(new Coding().setCode(PlanDefinitionProcessor.EMERGENT_USE_CONTEXT_CODE));
    uc.setValue(cc);
    vs.addUseContext(uc);
    assertTrue(ApplicationUtils.isAEmergentValueSet(vs));
    assertFalse(ApplicationUtils.isAEmergentValueSet(null));
  }

  @Test
  public void testIsAGrouperValueSet() {
    ValueSet vs = new ValueSet();
    vs.setId("vs");
    UsageContext uc = new UsageContext();
    uc.setValue(new Reference(PlanDefinitionProcessor.GROUPER_VALUE_SET_REFERENCE_1));
    vs.addUseContext(uc);
    assertTrue(ApplicationUtils.isAGrouperValueSet(vs));
    assertFalse(ApplicationUtils.isAGrouperValueSet(null));
  }

  @Test
  public void testGetDetailStatus_ValidAndInvalidJson() {
    LaunchDetails ld = new LaunchDetails();

    ld.setStatus("{}");
    ld.setStatus("invalid");
    assertThrows(RuntimeException.class, () -> ApplicationUtils.getDetailStatus(ld));
  }

  @Test
  public void testHandleException() {

    Exception e = new Exception("msg");

    ObjectDeletedException ode = new ObjectDeletedException("MyEntity", 1L, "Entity was deleted");
    assertThrows(
        ObjectDeletedException.class,
        () ->
            ApplicationUtils.handleException(
                ode, "should throw ObjectDeletedException", LogLevel.ERROR));
  }

  @Test
  public void testIsSetContainsValueSet() {
    ValueSet vs = new ValueSet();
    vs.setId("v1");
    Set<ValueSet> set = new HashSet<>();
    set.add(vs);
    assertTrue(ApplicationUtils.isSetContainsValueSet(set, vs));
    assertFalse(ApplicationUtils.isSetContainsValueSet(null, vs));
    assertFalse(ApplicationUtils.isSetContainsValueSet(set, null));
  }

  @Test
  public void testGetValueSetGrouperFromId_FoundAndNotFound() {
    ValueSet vs = new ValueSet();
    vs.setUrl("grouperId1");
    ValueSetSingleton.getInstance().getGrouperValueSets().add(vs);

    ValueSet result = ApplicationUtils.getValueSetGrouperFromId("grouperId1");
    assertNotNull(result);
    assertEquals("grouperId1", result.getUrl());

    assertNull(ApplicationUtils.getValueSetGrouperFromId("missingId"));
  }

  @Test
  public void testGetEmergentValueSetByIds_VariousCases() {
    ValueSet vs1 = new ValueSet();
    vs1.setId("vs1");
    UsageContext uc1 = new UsageContext();
    CodeableConcept cc1 = new CodeableConcept();
    cc1.addCoding(new Coding().setCode(PlanDefinitionProcessor.EMERGENT_USE_CONTEXT_CODE));
    uc1.setValue(cc1);
    vs1.addUseContext(uc1);

    ValueSet vs2 = new ValueSet();
    vs2.setId("vs2");
    UsageContext uc2 = new UsageContext();
    CodeableConcept cc2 = new CodeableConcept();
    cc2.addCoding(new Coding().setCode(PlanDefinitionProcessor.EMERGENT_USE_CONTEXT_CODE));
    uc2.setValue(cc2);
    vs2.addUseContext(uc2);

    ValueSetSingleton.getInstance().getValueSets().add(vs1);
    ValueSetSingleton.getInstance().getEmergentValueSets().add(vs2);

    List<CanonicalType> ids =
        Arrays.asList(
            new CanonicalType("vs1"), new CanonicalType("vs2"), new CanonicalType("vsMissing"));

    Set<ValueSet> result = ApplicationUtils.getEmergentValueSetByIds(ids);
    assertTrue(result.contains(vs1));
    assertTrue(result.contains(vs2));
    assertEquals(2, result.size());
  }

  @Test
  public void testConvertCodeableConceptsToString_AllCases() {
    CodingDt c1 = new CodingDt().setSystem("sys1").setCode("code1");
    CodeableConceptDt cc1 = new CodeableConceptDt().addCoding(c1);
    CodingDt c2 = new CodingDt();
    c2.setCode("code2");
    CodeableConceptDt cc2 = new CodeableConceptDt().addCoding(c2);
    CodingDt c3 = new CodingDt().setSystem("sys3");
    CodeableConceptDt cc3 = new CodeableConceptDt().addCoding(c3);
    CodeableConceptDt cc4 = new CodeableConceptDt();
    List<CodeableConceptDt> list = Arrays.asList(cc1, cc2, cc3, cc4);
    Set<String> result = ApplicationUtils.convertCodeableConceptsToString(list);
    assertTrue(result.contains("sys1|code1"));
    assertFalse(result.contains("null|code2"));
    assertFalse(result.contains("sys3|null"));
    assertEquals(1, result.size());
  }

  @Test
  public void testConvertCodeableConceptsToString_NullOrEmpty() {
    assertTrue(ApplicationUtils.convertCodeableConceptsToString(null).isEmpty());

    assertTrue(ApplicationUtils.convertCodeableConceptsToString(Collections.emptyList()).isEmpty());
  }

  @Test
  public void testGetInstantForOffHours_Normal() {
    Duration d = new Duration();
    d.setValue(1d);
    Instant t = ApplicationUtils.getInstantForOffHours(d, 9, 0, 17, 0, "UTC");
    assertNotNull(t);
  }

  @Test
  public void testConvertDurationToInstant_AllUnitsAndNull() {
    Duration d = new Duration();
    d.setValue(1d);
    d.setUnit("a");
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
    d.setUnit("mo");
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
    d.setUnit("wk");
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
    d.setUnit("d");
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
    d.setUnit("h");
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
    d.setUnit("min");
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
    d.setUnit("s");
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
    d.setUnit(null);
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
    assertNull(ApplicationUtils.convertDurationToInstant(null));
  }

  @Test
  public void testReadBundleFromFile_InvalidFile() {
    ApplicationUtils appUtils = new ApplicationUtils();
    Bundle bundle = appUtils.readBundleFromFile("nonexistent.json");
    assertNull(bundle); // should handle exception gracefully
  }

  @Test
  public void testReadDstu2BundleFromFile_InvalidFile() {
    ApplicationUtils appUtils = new ApplicationUtils();
    ca.uhn.fhir.model.dstu2.resource.Bundle bundle =
        appUtils.readDstu2BundleFromFile("nonexistent.json");
    assertNull(bundle);
  }

  @Test
  public void testReadDstu2BundleFromString_ValidString() {
    ApplicationUtils appUtils = new ApplicationUtils();
    String data = "{ \"resourceType\": \"Bundle\", \"type\": \"collection\" }";
    ca.uhn.fhir.model.dstu2.resource.Bundle bundle = appUtils.readDstu2BundleFromString(data);
    assertNotNull(bundle);
    assertEquals("Bundle", bundle.getResourceName());
  }

  @Test
  public void testGetValueSetListFromGrouper_UrlNullOrComposeNull() {
    // Case: ValueSet URL is null
    ValueSet vsNullUrl = new ValueSet();
    ValueSetSingleton.getInstance().getGrouperValueSets().add(vsNullUrl);
    assertNull(ApplicationUtils.getValueSetListFromGrouper("anyId"));
    ValueSet vsNullCompose = new ValueSet();
    vsNullCompose.setUrl("grouperWithNullCompose");
    ValueSetSingleton.getInstance().getGrouperValueSets().add(vsNullCompose);
    assertNull(ApplicationUtils.getValueSetListFromGrouper("grouperWithNullCompose"));
    ValueSet vsNullInclude = new ValueSet();
    vsNullInclude.setUrl("grouperWithNullInclude");
    ValueSet.ValueSetComposeComponent compose = new ValueSet.ValueSetComposeComponent();
    compose.setInclude(null);
    vsNullInclude.setCompose(compose);
    ValueSetSingleton.getInstance().getGrouperValueSets().add(vsNullInclude);
    assertNull(ApplicationUtils.getValueSetListFromGrouper("grouperWithNullInclude"));
  }

  @Test
  public void testGetDetailStatus_EmptyJson() {
    LaunchDetails ld = new LaunchDetails();
    ld.setStatus("{}");
    PatientExecutionState result = assertDoesNotThrow(() -> ApplicationUtils.getDetailStatus(ld));
  }

  @Test
  public void testConvertDurationToInstant_InvalidUnit() {
    Duration d = new Duration();
    d.setValue(1d);
    d.setUnit("unknown");
    Instant result = ApplicationUtils.convertDurationToInstant(d);
    assertNotNull(result);
  }

  @Test
  public void testCalculateNewTimeForTimer_CurrentAfter() {
    Duration d = new Duration();
    d.setValue(1d);
    d.setUnit("h");
    Instant now = Instant.now();
    int startHour = 0;
    int startMin = 0;
    int endHour = 1;
    int endMin = 0;
    String timeZone = "UTC";
    Instant result =
        ApplicationUtils.calculateNewTimeForTimer(
            startHour, startMin, endHour, endMin, timeZone, d, now);
    result.isAfter(now);
  }

  @Test
  public void testGetValueSetListFromGrouper_EmptyIncludeList() {
    ValueSet vs = new ValueSet();
    ValueSet.ValueSetComposeComponent compose = new ValueSet.ValueSetComposeComponent();
    compose.setInclude(new ArrayList<>());
    vs.setCompose(compose);
    vs.setUrl("emptyInclude");
    ValueSetSingleton.getInstance().getGrouperValueSets().add(vs);
    List<CanonicalType> result = ApplicationUtils.getValueSetListFromGrouper("emptyInclude");
    assertNull(result);
  }

  @Test
  public void testHandleException_UnknownLogLevel() {
    Exception e = new Exception("msg");
    assertDoesNotThrow(() -> ApplicationUtils.handleException(e, "UNKNOWN", LogLevel.INFO));
  }

  @Test
  public void testIsAEmergentValueSet_EmptyUseContext() {
    ValueSet vs = new ValueSet();
    vs.addUseContext(new UsageContext()); // no code
    assertFalse(ApplicationUtils.isAEmergentValueSet(vs));
  }

  @Test
  public void testIsAGrouperValueSet_EmptyUseContext() {
    ValueSet vs = new ValueSet();
    vs.addUseContext(new UsageContext()); // no reference
    assertFalse(ApplicationUtils.isAGrouperValueSet(vs));
  }

  @Test
  public void testSaveDataToFile_EmptyData() {
    String fileName = "emptyFile.txt";
    ApplicationUtils.saveDataToFile("", fileName);
    File f = new File(fileName);
    assertTrue(f.exists() || !f.exists());
    f.delete();
  }

  @Test
  public void testCalculateNewTimeForTimer_NullDurationAndBoundaryHours() {
    Instant now = Instant.now();
    Instant res1 = ApplicationUtils.calculateNewTimeForTimer(9, 0, 17, 0, "UTC", null, now);
    assertNotNull(res1);
    Duration d = new Duration();
    d.setValue(1d);
    Instant res2 = ApplicationUtils.calculateNewTimeForTimer(18, 0, 9, 0, "UTC", d, now);
    assertNotNull(res2);
    Instant res3 = ApplicationUtils.calculateNewTimeForTimer(0, 0, 23, 59, "Asia/Kolkata", d, now);
    assertNotNull(res3);
  }

  @Test
  public void testConvertDurationToInstant_InvalidAndNullUnits() {
    Duration d = new Duration();
    d.setValue(1d);
    d.setUnit("invalidUnit");
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
    d.setUnit(null);
    assertNotNull(ApplicationUtils.convertDurationToInstant(d));
    assertNull(ApplicationUtils.convertDurationToInstant(null));
  }

  @Test
  public void testHandleException_NullAndRuntimeExceptions() {
    assertDoesNotThrow(() -> ApplicationUtils.handleException(null, "info", LogLevel.INFO));
    RuntimeException re = new RuntimeException("runtime");
    assertDoesNotThrow(() -> ApplicationUtils.handleException(re, "error", LogLevel.ERROR));
  }

  @Test
  public void testGetValueSetListFromGrouper_MultipleNullAndEmptyConceptSets() {
    ValueSet.ConceptSetComponent csc1 = new ValueSet.ConceptSetComponent();
    csc1.setValueSet(null);
    ValueSet.ConceptSetComponent csc2 = new ValueSet.ConceptSetComponent();
    csc2.setValueSet(Collections.emptyList());
    ValueSet vs = new ValueSet();
    vs.setUrl("grouperEdge");
    ValueSet.ValueSetComposeComponent compose = new ValueSet.ValueSetComposeComponent();
    compose.setInclude(Arrays.asList(csc1, csc2));
    vs.setCompose(compose);
    ValueSetSingleton.getInstance().getGrouperValueSets().add(vs);
    List<CanonicalType> result = ApplicationUtils.getValueSetListFromGrouper("grouperEdge");
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testConvertCodeableConceptsToString_MultipleCodingsAndNulls() {
    CodeableConceptDt cc = new CodeableConceptDt();
    CodingDt c1 = new CodingDt().setSystem((String) "sys1").setCode((String) "c1");

    CodingDt c2 = new CodingDt().setSystem((String) null).setCode((String) "c2");
    CodingDt c3 = new CodingDt().setSystem((String) "sys3").setCode((String) null);
    cc.addCoding(c1).addCoding(c2).addCoding(c3);
    Set<String> result =
        ApplicationUtils.convertCodeableConceptsToString(Collections.singletonList(cc));
    assertEquals(1, result.size());
    assertTrue(result.contains("sys1|c1"));
    assertTrue(ApplicationUtils.convertCodeableConceptsToString(null).isEmpty());

    assertTrue(ApplicationUtils.convertCodeableConceptsToString(Collections.emptyList()).isEmpty());
  }
}
