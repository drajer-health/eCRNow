package com.drajer.bsa.ehr.service;

import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DocumentReference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.json.JSONObject;

/**
 *
 *
 * <h1>EhrQueryService</h1>
 *
 * This class defines the interface to access data from the Ehr for a set of resources.
 *
 * @author nbashyam
 */
public interface EhrQueryService {

  /**
   * The method is used to retrieve data from the Ehr.
   *
   * @param kd The processing context which contains information such as patient, encounter,
   *     previous data etc.
   * @return The Map of Resources to its type.
   */
  Map<ResourceType, Set<Resource>> getFilteredData(
      KarProcessingData kd, Map<String, ResourceType> resTypes);

  /**
   * The method is used to retrieve data from the Ehr.
   *
   * @param kd The processing context which contains information such as patient, encounter,
   *     previous data etc.
   * @param dRequirements The list of data requirements to get the data for.
   * @return The Map of Resources to its type.
   */
  Map<ResourceType, Set<Resource>> getFilteredData(
      KarProcessingData kd, List<DataRequirement> dRequirements);

  Map<ResourceType, Set<Resource>> loadJurisdicationData(KarProcessingData kd);

  void createResource(KarProcessingData kd, Resource resource);

  void updateResource(KarProcessingData kd, Resource resource);

  void deleteResource(KarProcessingData kd, ResourceType resourceType, String id);

  Resource getResourceById(KarProcessingData data, String resourceName, String id);

  Resource getResourceByUrl(KarProcessingData data, String resourceName, String id);

  DocumentReference constructR4DocumentReference(
      String payload,
      String patientId,
      String encounterID,
      String providerUUID,
      String rrDocRefMimeType,
      String title,
      String docCode,
      String docDisplayName,
      String docCodeSystem);

  JSONObject getAuthorizationToken(HealthcareSetting hs);

  void executeQuery(KarProcessingData kd, String dataReqId, FhirQueryFilter query);
}
