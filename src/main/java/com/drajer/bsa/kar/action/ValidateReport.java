package com.drajer.bsa.kar.action;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.OperationOutcome;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class ValidateReport extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(ValidateReport.class);

  @Autowired RestTemplate restTemplate;

  @Autowired IParser jsonParser;

  @Value("${validator.endpoint}")
  private String validatorEndpoint;

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {
    // TODO Auto-generated method stub

    logger.info(" Executing the Validation of the Report");

    String bundleString =
        "{\"resourceType\" : \"Bundle\",\"id\" : \"reporting-bundle-example\",\"meta\" : {\"versionId\" : \"1\",\"lastUpdated\" : \"2020-11-29T02:03:28.045+00:00\",\"profile\" : [\"http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-reporting-bundle\"]},\"type\" : \"message\",\"timestamp\" : \"2020-11-20T11:15:33-10:00\",\"entry\" : [{\"fullUrl\" : \"MessageHeader/messageheader-example-reportheader\",\"resource\" : {\"resourceType\" : \"MessageHeader\",\"id\" : \"messageheader-example-reportheader\",\"meta\" : {\"versionId\" : \"1\",\"lastUpdated\" : \"2020-11-29T02:03:28.045+00:00\",\"profile\" : [\"http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-messageheader\"]},\"text\" : {\"status\" : \"generated\",\"div\" : \"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">\\n            <p>Reporting Data</p> \\n          </div>\"},\"extension\" : [{\"url\" : \"http://hl7.org/fhir/us/medmorph/StructureDefinition/ext-dataEncrypted\",\"valueBoolean\" : false},{\"url\" : \"http://hl7.org/fhir/us/medmorph/StructureDefinition/ext-messageProcessingCategory\",\"valueCode\" : \"consequence\"}],\"eventCoding\" : {\"system\" : \"http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-messageheader-message-types\",\"code\" : \"cancer-report-message\"},\"destination\" : [{\"name\" : \"PHA endpoint\",\"endpoint\" : \"http://example.pha.org/fhir\"}],\"sender\" : {\"reference\" : \"Organization/example-healthcare-org\"},\"source\" : {\"name\" : \"Healthcare Organization\",\"software\" : \"Backend Service App\",\"version\" : \"3.1.45.AABB\",\"contact\" : {\"system\" : \"phone\",\"value\" : \"+1 (917) 123 4567\"},\"endpoint\" : \"http://example.healthcare.org/fhir\"},\"reason\" : {\"coding\" : [{\"system\" : \"http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-triggerdefinition-namedevents\",\"code\" : \"encounter-close\"}]},\"focus\" : [{\"reference\" : \"Bundle/content-bundle-example\"}]}},{\"fullUrl\" : \"Bundle/content-bundle-example\",\"resource\" : {\"resourceType\" : \"Bundle\",\"id\" : \"content-bundle-example\",\"meta\" : {\"versionId\" : \"1\",\"lastUpdated\" : \"2020-11-29T02:03:28.045+00:00\",\"profile\" : [\"http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-content-bundle\"]},\"type\" : \"collection\",\"timestamp\" : \"2020-11-20T11:15:33-10:00\",\"entry\" : [{\"fullUrl\" : \"Patient/1\",\"resource\" : {\"resourceType\" : \"Patient\",\"id\" : \"1\",\"text\" : {\"status\" : \"generated\",\"div\" : \"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">\\n                  <p> Patient Dominique Ledner</p>           \\n                </div>\"},\"identifier\" : [{\"use\" : \"usual\",\"type\" : {\"coding\" : [{\"system\" : \"http://terminology.hl7.org/CodeSystem/v2-0203\",\"code\" : \"MR\"}]},\"system\" : \"urn:oid:0.1.2.3.4.5.6.7\",\"value\" : \"654321\"}],\"active\" : true,\"name\" : [{\"use\" : \"official\",\"family\" : \"Ledner\",\"given\" : [\"Dominique\"]}],\"gender\" : \"male\",\"managingOrganization\" : {\"reference\" : \"Organization/example-healthcare-org\",\"display\" : \"Example Healthcare org\"}}}]}}]}";
    OperationOutcome outcome = new OperationOutcome();
    try {
      Bundle bundle = (Bundle) jsonParser.parseResource(bundleString);

      String request = jsonParser.encodeResourceToString(bundle);

      ResponseEntity<String> response =
          restTemplate.postForEntity(validatorEndpoint, request, String.class);
      logger.info(response.getBody());
      outcome = (OperationOutcome) jsonParser.parseResource(response.getBody());

    } catch (Exception e) {
      outcome
          .addIssue()
          .setSeverity(org.hl7.fhir.r4.model.OperationOutcome.IssueSeverity.ERROR)
          .setDiagnostics(
              "Failed to parse request body as JSON resource. Error was: " + e.getMessage());
    }
    return null;
  }
}
