package com.drajer.fhirecr;

import static org.junit.Assert.*;

import com.drajer.sof.model.R4FhirData;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import org.hl7.fhir.r4.model.*;
import org.junit.Test;

public class EicrCompositionGeneratorTest {

  @Test
  public void EicrCompositionGeneratortest_privateConstructor()
      throws NoSuchMethodException,
          InvocationTargetException,
          InstantiationException,
          IllegalAccessException {
    Constructor<EicrCompositionGenerator> constructor =
        EicrCompositionGenerator.class.getDeclaredConstructor();
    assertTrue(Modifier.isPrivate(constructor.getModifiers()));
    constructor.setAccessible(true);
    EicrCompositionGenerator instance = constructor.newInstance();
    assertNotNull(instance);
  }

  @Test
  public void EicrCompositionGeneratortest() {
    R4FhirData data = new R4FhirData();
    Patient patient = new Patient();
    patient.setId("patient1");

    Encounter encounter = new Encounter();
    encounter.setId("encounter1");
    data.setPatient(patient);
    data.setEncounter(encounter);

    Organization organization = new Organization();
    organization.setId("organization1");
    data.setOrganization(organization);

    Practitioner practitioner = new Practitioner();
    practitioner.setId("pract1");
    Composition result = EicrCompositionGenerator.convertR4FhirBundletoCdaEicr(data);

    assertNotNull(result);

    assertNotNull(result.getSubject().getResource());
    assertEquals("patient1", result.getSubject().getResource().getIdElement().getIdPart());

    assertNotNull(result.getEncounter().getResource());
    assertEquals("encounter1", result.getEncounter().getResource().getIdElement().getIdPart());

    assertNotNull(result.getAuthor());
    assertEquals(1, result.getAuthor().size());
  }
}
