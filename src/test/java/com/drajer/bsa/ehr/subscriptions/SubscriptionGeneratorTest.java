package com.drajer.bsa.ehr.subscriptions;

import static org.junit.jupiter.api.Assertions.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.parser.JsonParser;
import ca.uhn.fhir.parser.LenientErrorHandler;
import com.drajer.bsa.ehr.subscriptions.impl.SubscriptionGeneratorImpl;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.List;
import java.util.Objects;
import org.hl7.fhir.r4.model.PlanDefinition;
import org.hl7.fhir.r4.model.Subscription;
import org.junit.Test;

public class SubscriptionGeneratorTest {

  @Test
  public void testSubscriptionGeneration() {
    String resourceName = "Bsa/TestPlanDefinition.json";
    SubscriptionGeneratorImpl subscriptionGenerator =
        new SubscriptionGeneratorImpl("http://example.org/");
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(Objects.requireNonNull(classLoader.getResource(resourceName)).getFile());
    IParser jsonParser = new JsonParser(FhirContext.forR4(), new LenientErrorHandler());
    try {
      InputStream inputStream = new FileInputStream(file);
      PlanDefinition planDefinition = jsonParser.parseResource(PlanDefinition.class, inputStream);
      List<Subscription> subscriptionList =
          subscriptionGenerator.subscriptionsFromPlanDef(planDefinition);
      assertEquals(subscriptionList.size(), 1);
      Subscription sub = subscriptionList.get(0);
      assertEquals(sub.getCriteria(), "Encounter?");
      assertEquals(sub.getChannel().getEndpoint(), "http://example.org/");
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }
  }
}
