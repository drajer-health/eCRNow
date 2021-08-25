package com.drajer.bsa.ehr.subscriptions.impl;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.ehr.subscriptions.SubscriptionGeneratorService;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.service.impl.SubscriptionNotificationReceiverImpl;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Meta;
import org.hl7.fhir.r4.model.PlanDefinition;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Subscription;
import org.hl7.fhir.r4.model.Subscription.SubscriptionChannelComponent;
import org.hl7.fhir.r4.model.UriType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class SubscriptionGeneratorImpl implements SubscriptionGeneratorService {

  private final String notificationEndpoint;
  private final String NAMED_EVENT_EXTENSION =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/ext-us-ph-namedEventType";
  private final String NAMED_EVENT_CODE_SYSTEM =
      "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-triggerdefinition-namedevents";
  private final String BACKPORT_SUBSCRIPTION =
      "http://hl7.org/fhir/uv/subscriptions-backport/StructureDefinition/backport-subscription";
  private final String BACKPORT_TOPIC =
      "http://hl7.org/fhir/uv/subscriptions-backport/StructureDefinition/backport-topic-canonical";
  private final String BACKPORT_PAYLOAD =
      "http://hl7.org/fhir/uv/subscriptions-backport/StructureDefinition/backport-payload-content";
  private final String BACKPORT_ADDITIONAL_CRITERIA =
      "http://hl7.org/fhir/uv/subscriptions-backport/StructureDefinition/backport-additional-criteria";

  private final Logger logger = LoggerFactory.getLogger(SubscriptionNotificationReceiverImpl.class);

  public SubscriptionGeneratorImpl(@Value("${notification.endpoint}") String notificationEndpoint) {
    this.notificationEndpoint = notificationEndpoint;
  }

  public void createSubscriptions(KarProcessingData kd) {
    KnowledgeArtifactStatus status = kd.getKarStatus();
    EhrQueryService ehrQueryService = kd.getEhrQueryService();

    if (status.getIsActive() && status.getSubscriptionsEnabled()) {
      Set<String> subscriptions = status.getSubscriptions();
      if (subscriptions.size() == 0) {
        List<Subscription> subscriptionResources =
            subscriptionsFromBundle(kd.getKar().getOriginalKarBundle());
        for (Subscription sub : subscriptionResources) {
          ehrQueryService.createResource(kd, sub);
          subscriptions.add(sub.getId());
        }
      }
    }
  }

  public void deleteSubscriptions(KarProcessingData kd) {
    Set<String> subscriptions = kd.getKarStatus().getSubscriptions();
    EhrQueryService ehrQueryService = kd.getEhrQueryService();
    subscriptions.forEach(s -> ehrQueryService.deleteResource(kd, s));
  }

  public List<Subscription> subscriptionsFromBundle(Bundle bundle) {
    List<PlanDefinition> planDefinitions =
        bundle
            .getEntry()
            .stream()
            .filter(entry -> entry.getResource().getResourceType() == ResourceType.PlanDefinition)
            .map(entry -> (PlanDefinition) entry.getResource())
            .collect(Collectors.toList());
    return planDefinitions
        .stream()
        .map(this::subscriptionsFromPlanDef)
        .flatMap(List::stream)
        .collect(Collectors.toList());
  }

  public List<Subscription> subscriptionsFromPlanDef(PlanDefinition planDefinition) {
    PlanDefinition.PlanDefinitionActionComponent action = planDefinition.getActionFirstRep();
    String planDefinitionId = planDefinition.getIdElement().getIdPart();
    List<Subscription> subscriptions = new ArrayList<>();
    if (action.hasTrigger() && !action.getTrigger().isEmpty()) {

      action
          .getTrigger()
          .forEach(
              triggerDefinition -> {
                if (!triggerDefinition.hasExtension()) {
                  logger.error(" PlanDefinition/{} does not have a trigger", planDefinitionId);
                } else {
                  Extension extension = triggerDefinition.getExtensionByUrl(NAMED_EVENT_EXTENSION);
                  if (extension.isEmpty()) {
                    logger.error(
                        " PlanDefinition/{} does not have a named event trigger",
                        planDefinition.getIdElement().getIdPart());
                  } else {
                    if (extension.getValue() instanceof CodeableConcept) {
                      Coding namedEventCoding =
                          ((CodeableConcept) extension.getValue())
                              .getCoding()
                              .stream()
                              .filter(coding -> coding.getSystem().equals(NAMED_EVENT_CODE_SYSTEM))
                              .findFirst()
                              .orElse(null);
                      if (namedEventCoding != null) {
                        String code = namedEventCoding.getCode();
                        String criteria = namedEventToCriteria(code);
                        String uniqueId = String.format("sub%s%s", planDefinitionId, code);
                        if (criteria != null) {
                          subscriptions.add(
                              generateSubscription(criteria, notificationEndpoint, uniqueId, code));
                        }
                      }
                    }
                  }
                }
              });
    }
    return subscriptions;
  }

  public Subscription generateSubscription(
      String criteria, String notificationEndpoint, String id, String code) {
    Subscription subscription = new Subscription();
    if (!criteria.contains("?")) criteria += "?"; // HAPI expects some criteria
    subscription.setId(id);
    subscription.setStatus(Subscription.SubscriptionStatus.REQUESTED);
    subscription.setCriteria(criteria);
    SubscriptionChannelComponent subscriptionChannelComponent = new SubscriptionChannelComponent();
    subscriptionChannelComponent
        .setEndpoint(notificationEndpoint)
        .setPayload("application/fhir+json")
        .setType(Subscription.SubscriptionChannelType.RESTHOOK);
    //      .addHeader(String.format("Authorization: Bearer %s", token)); //TODO: the subscription
    // needs auth token
    if (code != null) {
      subscription.setMeta(new Meta().addProfile(BACKPORT_SUBSCRIPTION));
      subscription.addExtension(
          BACKPORT_TOPIC,
          new UriType(String.format("http://example.org/medmorph/subscriptiontopic/%s", code)));
      CodeType _payload = new CodeType("application/fhir+json");
      _payload.addExtension(BACKPORT_PAYLOAD, new CodeType("full-resource"));
      subscriptionChannelComponent.setPayloadElement(_payload);
    }

    subscription.setChannel(subscriptionChannelComponent);
    if (subscription.getCriteria().equals("Medication")) {
      StringType _criteria = new StringType("MedicationRequest?_lastUpdated=gt2021-01-01");
      _criteria
          .addExtension()
          .setUrl(BACKPORT_ADDITIONAL_CRITERIA)
          .setValue(new StringType("MedicationDispense"))
          .addExtension()
          .setUrl(BACKPORT_ADDITIONAL_CRITERIA)
          .setValue(new StringType("MedicationStatement"))
          .addExtension()
          .setUrl(BACKPORT_ADDITIONAL_CRITERIA)
          .setValue(new StringType("MedicationAdministration"));
      subscription.setCriteriaElement(_criteria);
    }

    return subscription;
  }

  private String namedEventToCriteria(String code) {
    ResourceType resourceType = namedEventToResourceType(code);
    if (resourceType == ResourceType.Observation) {
      return String.format("%s?category=laboratory", ResourceType.Observation);
    } else if (resourceType != null) {
      return resourceType.toString();
    } else {
      return null;
    }
  }

  private ResourceType namedEventToResourceType(String code) {
    String[] parts = code.split("-");
    String resource;
    if (parts[0].equals("new") || parts[0].equals("modified")) {
      resource = parts[1];
    } else if (parts[1].equals("change") || parts[1].equals("start") || parts[1].equals("close")) {
      resource = parts[0];
    } else {
      return null;
    }

    switch (resource) {
      case "encounter":
        return ResourceType.Encounter;
      case "diagnosis":
        return ResourceType.Condition;
      case "medication":
        return ResourceType.Medication;
      case "labresult":
        return ResourceType.Observation;
      case "order":
        return ResourceType.ServiceRequest;
      case "procedure":
        return ResourceType.Procedure;
      case "immunization":
        return ResourceType.Immunization;
      case "demographic":
        return ResourceType.Patient;
      default:
        return null;
    }
  }

  //        planDefinition.getAction()
  //        .stream()
  //        .filter(
  //            action ->
  //                action
  //                    .getCode()
  //                    .stream()
  //                    .anyMatch(
  //                        codeableConcept ->
  //                            codeableConcept
  //                                .getCoding()
  //                                .stream()
  //                                .anyMatch(
  //                                    coding ->
  //                                        coding
  //                                            .getCode()
  //                                            .equals(
  //                                                BsaTypes.ActionType.InitiateReportingWorkflow
  //                                                    .toString()))));

}
