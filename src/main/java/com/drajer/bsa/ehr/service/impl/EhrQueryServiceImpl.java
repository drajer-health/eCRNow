package com.drajer.bsa.ehr.service.impl;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.KarProcessingData;
import java.util.HashMap;
import java.util.Set;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>EhrQueryService</h1>
 *
 * This class defines the interface to access data from the Ehr for a set of resources.
 *
 * @author nbashyam
 */
@Service
@Transactional
public class EhrQueryServiceImpl implements EhrQueryService {

  /**
   * The method is used to retrieve data from the Ehr.
   *
   * @param kd The processing context which contains information such as patient, encounter,
   *     previous data etc.
   * @return The Map of Resources to its type.
   */
  @Override
  public HashMap<ResourceType, Set<Resource>> getFilteredData(KarProcessingData kd) {
    // TODO Auto-generated method stub
    return null;
  }
}
