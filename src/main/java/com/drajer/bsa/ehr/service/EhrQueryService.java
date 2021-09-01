package com.drajer.bsa.ehr.service;

import com.drajer.bsa.model.KarProcessingData;
import java.util.HashMap;
import java.util.Set;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;

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
  HashMap<ResourceType, Set<Resource>> getFilteredData(
      KarProcessingData kd, HashMap<String, ResourceType> resTypes);

  void createResource(KarProcessingData kd, Resource resource);

  void deleteResource(KarProcessingData kd, ResourceType resourceType, String id);
}
