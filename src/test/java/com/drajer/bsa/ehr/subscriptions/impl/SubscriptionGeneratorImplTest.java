package com.drajer.bsa.ehr.subscriptions.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KarProcessingData;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class SubscriptionGeneratorImplTest {

  @Mock private EhrQueryService ehrQueryService;
  @Mock private KarProcessingData karProcessingData;
  @Mock private KnowledgeArtifactStatus karStatus;

  private SubscriptionGeneratorImpl subscriptionGenerator;

  @Before
  public void setup() {
    subscriptionGenerator = new SubscriptionGeneratorImpl("http://localhost/notify");
  }

  @Test
  public void testInactiveKar() {
    when(karProcessingData.getKarStatus()).thenReturn(karStatus);
    when(karStatus.getIsActive()).thenReturn(false);
    subscriptionGenerator.createSubscriptions(karProcessingData);
    verifyNoInteractions(ehrQueryService);
  }

  @Test
  public void testSubscriptionsDisabled() {
    when(karProcessingData.getKarStatus()).thenReturn(karStatus);
    when(karStatus.getIsActive()).thenReturn(true);
    when(karStatus.getSubscriptionsEnabled()).thenReturn(false);
    subscriptionGenerator.createSubscriptions(karProcessingData);
    verifyNoInteractions(ehrQueryService);
  }

  @Test
  public void testSubscriptionAlreadyExists() {
    when(karProcessingData.getKarStatus()).thenReturn(karStatus);
    when(karStatus.getIsActive()).thenReturn(true);
    when(karStatus.getSubscriptionsEnabled()).thenReturn(true);
    when(karStatus.getSubscriptions()).thenReturn(Collections.singleton("sub1"));
    subscriptionGenerator.createSubscriptions(karProcessingData);
    verifyNoInteractions(ehrQueryService);
    assertTrue(karStatus.getSubscriptions().contains("sub1"));
  }

  @Test
  public void testCreateSubscriptions_EmptySubscriptions_GenerateValid() {
    when(karProcessingData.getKarStatus()).thenReturn(karStatus);
    when(karStatus.getIsActive()).thenReturn(true);
    when(karStatus.getSubscriptionsEnabled()).thenReturn(true);
    Set<String> subscriptionsSet = new HashSet<>();
    when(karStatus.getSubscriptions()).thenReturn(subscriptionsSet);

    PlanDefinition pd = new PlanDefinition();
    PlanDefinition.PlanDefinitionActionComponent action =
        new PlanDefinition.PlanDefinitionActionComponent();
    TriggerDefinition trigger = new TriggerDefinition();
    trigger
        .addExtension()
        .setUrl("http://hl7.org/fhir/us/medmorph/StructureDefinition/ext-us-ph-namedEventType")
        .setValue(
            new CodeableConcept()
                .addCoding(
                    new Coding()
                        .setSystem(
                            "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-triggerdefinition-namedevents")
                        .setCode("new-labresult")));
    action.addTrigger(trigger);
    pd.addAction(action);

    Bundle bundle = new Bundle();
    bundle.addEntry(new Bundle.BundleEntryComponent().setResource(pd));

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(kar.getOriginalKarBundle()).thenReturn(bundle);
    when(karProcessingData.getKar()).thenReturn(kar);
    when(karProcessingData.getEhrQueryService()).thenReturn(ehrQueryService);

    subscriptionGenerator.createSubscriptions(karProcessingData);
    verify(ehrQueryService, atLeastOnce())
        .createResource(eq(karProcessingData), any(Subscription.class));
    assertFalse(subscriptionsSet.isEmpty());
  }

  @Test
  public void testCreateSubscriptions_TriggerInvalidNamedEvent() {
    when(karProcessingData.getKarStatus()).thenReturn(karStatus);
    when(karStatus.getIsActive()).thenReturn(true);
    when(karStatus.getSubscriptionsEnabled()).thenReturn(true);
    Set<String> subscriptionsSet = new HashSet<>();
    when(karStatus.getSubscriptions()).thenReturn(subscriptionsSet);

    PlanDefinition pd = new PlanDefinition();
    PlanDefinition.PlanDefinitionActionComponent action =
        new PlanDefinition.PlanDefinitionActionComponent();
    TriggerDefinition trigger = new TriggerDefinition();
    trigger
        .addExtension()
        .setUrl("http://hl7.org/fhir/us/medmorph/StructureDefinition/ext-us-ph-namedEventType")
        .setValue(new StringType("invalid"));
    action.addTrigger(trigger);
    pd.addAction(action);

    Bundle bundle = new Bundle();
    bundle.addEntry(new Bundle.BundleEntryComponent().setResource(pd));

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(kar.getOriginalKarBundle()).thenReturn(bundle);
    when(karProcessingData.getKar()).thenReturn(kar);
    when(karProcessingData.getEhrQueryService()).thenReturn(ehrQueryService);

    subscriptionGenerator.createSubscriptions(karProcessingData);
    verifyNoInteractions(ehrQueryService);
    assertTrue(subscriptionsSet.isEmpty());
  }

  @Test
  public void testDeleteSubscriptions_NonEmpty() {
    when(karProcessingData.getKarStatus()).thenReturn(karStatus);
    when(karProcessingData.getEhrQueryService()).thenReturn(ehrQueryService);
    Set<String> subscriptions = new HashSet<>(Arrays.asList("sub1", "sub2"));
    when(karStatus.getSubscriptions()).thenReturn(subscriptions);

    subscriptionGenerator.deleteSubscriptions(karProcessingData);
    verify(ehrQueryService).deleteResource(karProcessingData, ResourceType.Subscription, "sub1");
    verify(ehrQueryService).deleteResource(karProcessingData, ResourceType.Subscription, "sub2");
  }

  @Test
  public void testDeleteSubscriptions_Empty() {
    when(karProcessingData.getKarStatus()).thenReturn(karStatus);
    when(karStatus.getSubscriptions()).thenReturn(Collections.emptySet());
    when(karProcessingData.getEhrQueryService()).thenReturn(ehrQueryService);

    subscriptionGenerator.deleteSubscriptions(karProcessingData);
    verifyNoInteractions(ehrQueryService);
  }

  @Test
  public void testSubscriptionsFromBundle_NoPlanDefinition() {
    Bundle bundle = new Bundle();
    bundle.addEntry(new Bundle.BundleEntryComponent().setResource(new Patient()));
    assertTrue(subscriptionGenerator.subscriptionsFromBundle(bundle).isEmpty());
  }

  @Test
  public void testSubscriptionsFromPlanDef_NoTrigger() {
    PlanDefinition pd = new PlanDefinition();
    pd.addAction(new PlanDefinition.PlanDefinitionActionComponent());
    assertTrue(subscriptionGenerator.subscriptionsFromPlanDef(pd).isEmpty());
  }

  @Test
  public void testSubscriptionsFromPlanDef_TriggerEmptyExtension() {
    PlanDefinition pd = new PlanDefinition();
    PlanDefinition.PlanDefinitionActionComponent action =
        new PlanDefinition.PlanDefinitionActionComponent();
    action.addTrigger(new TriggerDefinition());
    pd.addAction(action);
    assertTrue(subscriptionGenerator.subscriptionsFromPlanDef(pd).isEmpty());
  }

  @Test
  public void testGenerateSubscription_NullCode() {
    Subscription sub =
        subscriptionGenerator.generateSubscription(
            "Observation?status=final", "http://localhost/notify", "sub1", null);
    assertNotNull(sub);
    assertEquals("sub1", sub.getId());
    assertEquals(Subscription.SubscriptionStatus.REQUESTED, sub.getStatus());
    assertEquals("Observation?status=final", sub.getCriteria());
  }

  @Test
  public void testGenerateSubscription_NonNullCodeAndMedication() {
    Subscription sub =
        subscriptionGenerator.generateSubscription(
            "Medication", "http://localhost/notify", "sub2", "medication-start");
    assertNotNull(sub);
    assertEquals("sub2", sub.getId());
    assertEquals(Subscription.SubscriptionStatus.REQUESTED, sub.getStatus());
    assertNotNull(sub.getCriteriaElement());
    assertNotNull(sub.getMeta());
  }

  public void testNamedEventToResourceType_EdgeCasesSafe() throws Exception {
    String[] codes = {
      "new-encounter", "new-diagnosis", "new-medication", "new-labresult",
      "new-order", "new-procedure", "new-immunization", "new-demographic",
      "modified-change", "foo-start", "bar-xyz", "invalidcode"
    };
    ResourceType[] expected = {
      ResourceType.Encounter,
      ResourceType.Condition,
      ResourceType.Medication,
      ResourceType.Observation,
      ResourceType.ServiceRequest,
      ResourceType.Procedure,
      ResourceType.Immunization,
      ResourceType.Patient,
      null,
      null,
      null,
      null
    };

    java.lang.reflect.Method method =
        SubscriptionGeneratorImpl.class.getDeclaredMethod("namedEventToResourceType", String.class);
    method.setAccessible(true);

    for (int i = 0; i < codes.length; i++) {
      try {
        assertEquals(expected[i], method.invoke(subscriptionGenerator, codes[i]));
      } catch (Exception e) {
        assertTrue(e instanceof java.lang.reflect.InvocationTargetException);
      }
    }
  }

  @Test
  public void testSubscriptionsFromPlanDef_MapsEachNamedEventToItsCriteria() {
    String planDefinitionId = "plandefinition-ersd-instance-example";
    Map<String, String> expectedCriteria = new HashMap<>();
    expectedCriteria.put("encounter-start", "Encounter?");
    expectedCriteria.put("encounter-close", "Encounter?");
    expectedCriteria.put("encounter-change", "Encounter?");
    expectedCriteria.put("new-diagnosis", "Condition?");
    expectedCriteria.put("modified-diagnosis", "Condition?");
    expectedCriteria.put("new-medication", "Medication?");
    expectedCriteria.put("new-labresult", "Observation?category=laboratory");
    expectedCriteria.put("new-order", "ServiceRequest?");
    expectedCriteria.put("new-procedure", "Procedure?");
    expectedCriteria.put("new-immunization", "Immunization?");
    expectedCriteria.put("modified-demographic", "Patient?");

    PlanDefinition planDefinition = new PlanDefinition();
    planDefinition.setId(planDefinitionId);
    PlanDefinition.PlanDefinitionActionComponent action = planDefinition.addAction();
    Set<String> namedEvents = new HashSet<>(expectedCriteria.keySet());
    namedEvents.addAll(Set.of("new-allergy", "outbreak-alert"));
    for (String namedEvent : namedEvents) {
      action
          .addTrigger()
          .addExtension()
          .setUrl("http://hl7.org/fhir/us/medmorph/StructureDefinition/ext-us-ph-namedEventType")
          .setValue(
              new CodeableConcept()
                  .addCoding(
                      new Coding()
                          .setSystem(
                              "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-triggerdefinition-namedevents")
                          .setCode(namedEvent)));
    }

    Map<String, String> actualCriteria = new HashMap<>();
    List<Subscription> subscriptions =
        subscriptionGenerator.subscriptionsFromPlanDef(planDefinition);
    for (Subscription subscription : subscriptions) {
      actualCriteria.put(
          subscription.getIdElement().getIdPart().replace("sub" + planDefinitionId, ""),
          subscription.getCriteria());
    }

    assertEquals(expectedCriteria, actualCriteria);
  }

  @Test
  public void testGenerateSubscription_PostsFullResourcesToTheNotificationEndpoint() {
    Subscription subscription =
        subscriptionGenerator.generateSubscription(
            "Encounter", "http://localhost/notify", "subencounter-close", "encounter-close");

    assertEquals(Subscription.SubscriptionStatus.REQUESTED, subscription.getStatus());
    assertEquals("Encounter?", subscription.getCriteria());
    assertEquals("http://localhost/notify", subscription.getChannel().getEndpoint());
    assertEquals(
        Subscription.SubscriptionChannelType.RESTHOOK, subscription.getChannel().getType());
    assertEquals(
        "http://example.org/medmorph/subscriptiontopic/encounter-close",
        subscription
            .getExtensionByUrl(
                "http://hl7.org/fhir/uv/subscriptions-backport/StructureDefinition/backport-topic-canonical")
            .getValue()
            .primitiveValue());
  }
}
