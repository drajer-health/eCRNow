package com.drajer.sof.launch;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.IReadExecutable;
import ca.uhn.fhir.rest.gclient.IReadTyped;
import ca.uhn.fhir.rest.server.exceptions.ResourceNotFoundException;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import org.apache.commons.lang3.time.DateUtils;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.Period;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.web.server.ResponseStatusException;

public class LaunchControllerTest {

  private LaunchDetails currentStateDetails;
  private ClientDetails clientDetails;

  @InjectMocks LaunchController launchController;

  @Mock FhirContextInitializer fhirContextInitializer;

  @Before
  public void init() {
    MockitoAnnotations.initMocks(this);
    currentStateDetails =
        (LaunchDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/LaunchDetails/LaunchDetails.json", LaunchDetails.class);
    currentStateDetails.setLastUpdated(new Date());
    clientDetails =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDetail_IT_FullECR.json", ClientDetails.class);
  }

  @Test
  public void testStartEndDateDSTU2_basedOnClientDetailThreshold()
      throws JsonParseException, JsonMappingException, IOException, ParseException {

    currentStateDetails.setFhirVersion("DSTU2");
    Date startDate =
        DateUtils.addHours(
            new Date(), Integer.parseInt(clientDetails.getEncounterStartThreshold()));
    Date endDate =
        DateUtils.addHours(new Date(), Integer.parseInt(clientDetails.getEncounterEndThreshold()));

    Encounter encounter = new Encounter();
    encounter.setPeriod(new PeriodDt());

    launchController.setStartAndEndDates(clientDetails, currentStateDetails, encounter);

    assertEquals(
        DateUtils.truncate(startDate, Calendar.SECOND),
        DateUtils.truncate(currentStateDetails.getStartDate(), Calendar.SECOND));
    assertEquals(
        DateUtils.truncate(endDate, Calendar.SECOND),
        DateUtils.truncate(currentStateDetails.getEndDate(), Calendar.SECOND));
  }

  @Test
  public void testStartEndDateDSTU2_basedOnEncounterPeriod()
      throws JsonParseException, JsonMappingException, IOException, ParseException {

    currentStateDetails.setFhirVersion("DSTU2");

    SimpleDateFormat sdf = new SimpleDateFormat("yyyMMddHHmmss");
    String startDateStr = "20200101010101";
    Date startDate = sdf.parse(startDateStr);
    String endDateStr = "20200102010101";
    Date endDate = sdf.parse(endDateStr);

    PeriodDt periodDt = new PeriodDt();
    periodDt.setStartWithSecondsPrecision(startDate);
    periodDt.setEndWithSecondsPrecision(endDate);
    Encounter encounter = new Encounter();
    encounter.setPeriod(periodDt);

    launchController.setStartAndEndDates(clientDetails, currentStateDetails, encounter);

    assertEquals(startDate, currentStateDetails.getStartDate());
    assertEquals(endDate, currentStateDetails.getEndDate());
  }

  @Test
  public void testStartEndDateR4_basedOnEncounterPeriod()
      throws JsonParseException, JsonMappingException, IOException, ParseException {

    currentStateDetails.setFhirVersion("R4");

    SimpleDateFormat sdf = new SimpleDateFormat("yyyMMddHHmmss");
    String startDateStr = "20200101010101";
    Date startDate = sdf.parse(startDateStr);
    String endDateStr = "20200102010101";
    Date endDate = sdf.parse(endDateStr);

    Period period = new Period();
    period.setStart(startDate);
    period.setEnd(endDate);
    org.hl7.fhir.r4.model.Encounter r4Encounter = new org.hl7.fhir.r4.model.Encounter();
    r4Encounter.setPeriod(period);

    launchController.setStartAndEndDates(clientDetails, currentStateDetails, r4Encounter);

    assertEquals(currentStateDetails.getStartDate(), startDate);
    assertEquals(currentStateDetails.getEndDate(), endDate);
  }

  @Test
  public void testStartEndDateR4_basedOnClientDetailThreshold()
      throws JsonParseException, JsonMappingException, IOException, ParseException {

    currentStateDetails.setFhirVersion("R4");
    Date startDate =
        DateUtils.addHours(
            new Date(), Integer.parseInt(clientDetails.getEncounterStartThreshold()));
    Date endDate =
        DateUtils.addHours(new Date(), Integer.parseInt(clientDetails.getEncounterEndThreshold()));

    org.hl7.fhir.r4.model.Encounter r4Encounter = new org.hl7.fhir.r4.model.Encounter();
    r4Encounter.setPeriod(new Period());

    launchController.setStartAndEndDates(clientDetails, currentStateDetails, r4Encounter);

    assertEquals(
        DateUtils.truncate(startDate, Calendar.SECOND),
        DateUtils.truncate(currentStateDetails.getStartDate(), Calendar.SECOND));
    assertEquals(
        DateUtils.truncate(endDate, Calendar.SECOND),
        DateUtils.truncate(currentStateDetails.getEndDate(), Calendar.SECOND));
  }

  @Test
  public void testStartEndDate_nullValue()
      throws JsonParseException, JsonMappingException, IOException, ParseException {

    currentStateDetails.setFhirVersion("DSTU3");
    org.hl7.fhir.r4.model.Encounter r4Encounter = new org.hl7.fhir.r4.model.Encounter();
    r4Encounter.setPeriod(new Period());

    launchController.setStartAndEndDates(clientDetails, currentStateDetails, r4Encounter);

    assertEquals(null, currentStateDetails.getStartDate());
    assertEquals(null, currentStateDetails.getEndDate());

    currentStateDetails.setFhirVersion("R4");
    r4Encounter = null;
    launchController.setStartAndEndDates(clientDetails, currentStateDetails, r4Encounter);

    assertEquals(null, currentStateDetails.getStartDate());
    assertEquals(null, currentStateDetails.getEndDate());

    currentStateDetails.setFhirVersion("DSTU3");
    r4Encounter = new org.hl7.fhir.r4.model.Encounter();
    r4Encounter.setPeriod(null);

    launchController.setStartAndEndDates(clientDetails, currentStateDetails, r4Encounter);

    assertEquals(null, currentStateDetails.getStartDate());
    assertEquals(null, currentStateDetails.getEndDate());
  }

  @Test(expected = ResponseStatusException.class)
  public void testGetEncounterById_Exception() {
    FhirContext context = Mockito.mock(FhirContext.class);
    IGenericClient client = Mockito.mock(IGenericClient.class);
    IRead read = Mockito.mock(IRead.class);
    IReadTyped<IBaseResource> readType = Mockito.mock((IReadTyped.class));
    IReadExecutable<IBaseResource> readExecutable = Mockito.mock((IReadExecutable.class));

    currentStateDetails.setFhirVersion("R4");

    when(fhirContextInitializer.getFhirContext(currentStateDetails.getFhirVersion()))
        .thenReturn(context);

    when(fhirContextInitializer.createClient(
            context,
            currentStateDetails.getEhrServerURL(),
            currentStateDetails.getAccessToken(),
            currentStateDetails.getxRequestId()))
        .thenReturn(client);

    when(client.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId(currentStateDetails.getEncounterId())).thenReturn(readExecutable);
    when(readExecutable.execute()).thenThrow(new ResourceNotFoundException("Resource Not Found"));

    IBaseResource encounter = launchController.getEncounterById(currentStateDetails);
  }
}
