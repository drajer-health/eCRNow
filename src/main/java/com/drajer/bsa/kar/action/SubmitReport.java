package com.drajer.bsa.kar.action;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;
import org.hl7.fhir.r4.model.Bundle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class SubmitReport extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(SubmitReport.class);

  @Value("${eCRFhir.endpoint}")
  private String eCRonFhirEndpoint;

  private static final FhirContext context = FhirContext.forR4();

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {

    logger.info(" Executing the submission of the Report");

    String bundleString =
        "{\"resourceType\" : \"Bundle\",\"id\" : \"reporting-bundle-example\",\"meta\" : {\"versionId\" : \"1\",\"lastUpdated\" : \"2020-11-29T02:03:28.045+00:00\",\"profile\" : [\"http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-reporting-bundle\"]},\"type\" : \"message\",\"timestamp\" : \"2020-11-20T11:15:33-10:00\",\"entry\" : [{\"fullUrl\" : \"MessageHeader/messageheader-example-reportheader\",\"resource\" : {\"resourceType\" : \"MessageHeader\",\"id\" : \"messageheader-example-reportheader\",\"meta\" : {\"versionId\" : \"1\",\"lastUpdated\" : \"2020-11-29T02:03:28.045+00:00\",\"profile\" : [\"http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-messageheader\"]},\"text\" : {\"status\" : \"generated\",\"div\" : \"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">\\n            <p>Reporting Data</p> \\n          </div>\"},\"extension\" : [{\"url\" : \"http://hl7.org/fhir/us/medmorph/StructureDefinition/ext-dataEncrypted\",\"valueBoolean\" : false},{\"url\" : \"http://hl7.org/fhir/us/medmorph/StructureDefinition/ext-messageProcessingCategory\",\"valueCode\" : \"consequence\"}],\"eventCoding\" : {\"system\" : \"http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-messageheader-message-types\",\"code\" : \"cancer-report-message\"},\"destination\" : [{\"name\" : \"PHA endpoint\",\"endpoint\" : \"http://example.pha.org/fhir\"}],\"sender\" : {\"reference\" : \"Organization/example-healthcare-org\"},\"source\" : {\"name\" : \"Healthcare Organization\",\"software\" : \"Backend Service App\",\"version\" : \"3.1.45.AABB\",\"contact\" : {\"system\" : \"phone\",\"value\" : \"+1 (917) 123 4567\"},\"endpoint\" : \"http://example.healthcare.org/fhir\"},\"reason\" : {\"coding\" : [{\"system\" : \"http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-triggerdefinition-namedevents\",\"code\" : \"encounter-close\"}]},\"focus\" : [{\"reference\" : \"Bundle/content-bundle-example\"}]}},{\"fullUrl\" : \"Bundle/content-bundle-example\",\"resource\" : {\"resourceType\" : \"Bundle\",\"id\" : \"content-bundle-example\",\"meta\" : {\"versionId\" : \"1\",\"lastUpdated\" : \"2020-11-29T02:03:28.045+00:00\",\"profile\" : [\"http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-content-bundle\"]},\"type\" : \"collection\",\"timestamp\" : \"2020-11-20T11:15:33-10:00\",\"entry\" : [{\"fullUrl\" : \"Patient/1\",\"resource\" : {\"resourceType\" : \"Patient\",\"id\" : \"1\",\"text\" : {\"status\" : \"generated\",\"div\" : \"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">\\n                  <p> Patient Dominique Ledner</p>           \\n                </div>\"},\"identifier\" : [{\"use\" : \"usual\",\"type\" : {\"coding\" : [{\"system\" : \"http://terminology.hl7.org/CodeSystem/v2-0203\",\"code\" : \"MR\"}]},\"system\" : \"urn:oid:0.1.2.3.4.5.6.7\",\"value\" : \"654321\"}],\"active\" : true,\"name\" : [{\"use\" : \"official\",\"family\" : \"Ledner\",\"given\" : [\"Dominique\"]}],\"gender\" : \"male\",\"managingOrganization\" : {\"reference\" : \"Organization/example-healthcare-org\",\"display\" : \"Example Healthcare org\"}}}]}}]}";
    Bundle bundle = (Bundle) context.newJsonParser().parseResource(bundleString);

    IGenericClient client = context.newRestfulGenericClient(eCRonFhirEndpoint);
    context.getRestfulClientFactory().setSocketTimeout(30 * 1000);
    Bundle responseBundle =
        (Bundle)
            client.operation().processMessage().setMessageBundle(bundle).encodedJson().execute();

    logger.info(
        "Response Bundle:::::{}", context.newJsonParser().encodeResourceToString(responseBundle));

    return null;
  }
}
