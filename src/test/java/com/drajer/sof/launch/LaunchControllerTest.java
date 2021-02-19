package com.drajer.sof.launch;

import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import java.io.IOException;
import java.util.Date;
import org.hl7.fhir.r4.model.Period;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

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
                "R4/Misc/ClientDetails/ClientDataEntry1.json", ClientDetails.class);
  }

  @Test
  public void testStartEndDateDSTU2() throws JsonParseException, JsonMappingException, IOException {

    Date startDate = currentStateDetails.getStartDate();
    Date endDate = currentStateDetails.getEndDate();

    FhirContext context = Mockito.mock(FhirContext.class);
    IGenericClient client = Mockito.mock(IGenericClient.class);

    currentStateDetails.setFhirVersion("DSTU2");

    when(fhirContextInitializer.getFhirContext(currentStateDetails.getFhirVersion()))
        .thenReturn(context);

    when(fhirContextInitializer.createClient(
            context, currentStateDetails.getEhrServerURL(), currentStateDetails.getAccessToken()))
        .thenReturn(client);

    Encounter encounter = new Encounter();

    encounter.setPeriod(new PeriodDt());

    when(fhirContextInitializer.getResouceById(
            currentStateDetails,
            client,
            context,
            "Encounter",
            currentStateDetails.getEncounterId()))
        .thenReturn(encounter);

    launchController.setStartAndEndDates(clientDetails, currentStateDetails);

    assertNotEquals(startDate, currentStateDetails.getStartDate());
    assertNotEquals(endDate, currentStateDetails.getEndDate());
  }

  @Test
  public void testStartEndDateR4() throws JsonParseException, JsonMappingException, IOException {

    currentStateDetails.setFhirVersion("R4");

    Date startDate = currentStateDetails.getStartDate();
    Date endDate = currentStateDetails.getEndDate();

    FhirContext context = Mockito.mock(FhirContext.class);
    IGenericClient client = Mockito.mock(IGenericClient.class);

    when(fhirContextInitializer.getFhirContext(currentStateDetails.getFhirVersion()))
        .thenReturn(context);

    when(fhirContextInitializer.createClient(
            context, currentStateDetails.getEhrServerURL(), currentStateDetails.getAccessToken()))
        .thenReturn(client);

    org.hl7.fhir.r4.model.Encounter r4Encounter = new org.hl7.fhir.r4.model.Encounter();

    r4Encounter.setPeriod(new Period());

    when(fhirContextInitializer.getResouceById(
            currentStateDetails,
            client,
            context,
            "Encounter",
            currentStateDetails.getEncounterId()))
        .thenReturn(r4Encounter);

    launchController.setStartAndEndDates(clientDetails, currentStateDetails);

    assertNotEquals(startDate, currentStateDetails.getStartDate());
    assertNotEquals(endDate, currentStateDetails.getEndDate());
  }
}
